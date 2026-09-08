#' Plot point data on a map
#'
#' Plots data as points on a map, with the colour scaled according to the
#' selected column. The output is a ggplot so can be modified by adding ggplot2
#' scales, coords and themes etc. By default the map is plotted using longitude
#' and latitude coordinates. You can change this by passing a proj4 string, or a
#' `geodomain` on which you want the data to be plotted. This function is called
#' by \code{\link{plot_point_verif()}} for plotting point verifcation scores on
#' a map.
#'
#' @param .data A data frame. Must include columns `"lon"` and `"lat"`.
#' @param col The column for which to plot points. Can be passed quoted or
#'   unquoted. If passing a variable for the column name, it should be embraced
#'   in double curly braces, i.e. `{{ }}`
#' @param map_db The map database to get the map from. By default this is
#'   \code{\link[maps]{world}}, a 1:50m resolution map. You can also choose maps
#'   from the rnaturalearth set of packages, but note that it can be problematic
#'   to plot polygons with the `rnaturalearthhires::countries10` dataset.
#' @param proj The projection of the map. By default the data are plotted on
#'   lon and lat coordinates. Here you can pass a proj4 string, or a `gemdomain`,
#'   `geofield` or `geolist` to define a domain. If a domain is passed, the
#'   data will be cropped to that domain if any points fall outside of the
#'   domain.
#' @param expand An expansion around the data. In order to prevent points from
#'   being plotted too close to the edges of the map you can provide an
#'   expansion factor. Can be a one or two element vector - if a one element
#'   vector the expansion is applied in all directions, if a two element vector,
#'   the first value is the expansion applied in the left-right direction and
#'   the second in the bottom-top direction. By default a 10% expansion (0.1) is
#'   applied in all directions.
#' @param polygon Logical. Whether to get the map data as polygons (`TRUE`), or
#'   paths (`FALSE`). The default is to get paths. If you experience issues
#'   plotting polygons with `map_db = "world"`, try using
#'   `map_db = rnaturalearthdata::countries50`.
#' @param land_fill If `polygon = TRUE`, the colour of land areas.
#' @param land_stroke The colour of country outlines.
#' @param sea_fill If `polygon = TRUE`, the colour of sea areas.
#' @param point_size The size of the data points.
#' @param point_stroke The outline colour of data points.
#'
#' @returns A ggplot plot
#' @export
#'
#' @examples
#' # Make some fake data
#' df <- subset(station_list, SID > 1000 & SID < 4000 | SID > 5000 & SID < 8500)
#' df$fcst <- abs(rnorm(nrow(df)))
#' plot_point_map(df, fcst)
#'
#' # Use a mercator projection
#' plot_point_map(df, fcst, proj = "+proj=merc")
#'
#' # Plot map as filled polygons
#' plot_point_map(
#'   df, fcst, polygon = TRUE, map_db = rnaturalearthdata::countries50,
#'   land_fill = scales::muted("green"),
#'   sea_fill  = scales::muted("blue")
#' )
plot_point_map <- function(
  .data, col, map_db = "world", proj = NULL, expand = 0.1,
  polygon = FALSE, land_fill = "#EEEEEE", land_stroke = "black",
  sea_fill = "white", point_size = 3, point_stroke = "black"
) {
  col_quo  <- rlang::enquo(col)
  col_name <- rlang::as_name(col_quo)

  if (length(expand) < 2) {
    expand <- c(expand, expand)
  }

  # Check that lon and lat columns are available
  if (length(intersect(c("lon", "lat"), colnames(.data))) != 2) {
    cli::cli_abort(c(
      "Columns {.var {c('lon', 'lat')}} not found in {.arg .data}.",
      "x" = "You must have {.var {c('lon', 'lat')}} columns to plot a map.",
      "i" = "If you only have {.var SID}, try joining to a station list."
    ))
  }

  # Reproject if a projection is passed, otherwise rename lon and lat
  # to x and y
  if (!is.null(proj)) {
    crop <- FALSE
    if (inherits(proj, c("harp_geolist", "geofield", "geodomain"))) {
      crop <- TRUE
    }
    .data <- harpCore::geo_reproject(.data, proj, crop = crop)
  } else {
    colnames(.data)[colnames(.data) == "lon"] <- "x"
    colnames(.data)[colnames(.data) == "lat"] <- "y"
  }

  # If we need a domain, create one from the data - always make it 100 * 100
  if (
    is.null(proj) ||
    !inherits(proj, c("harp_geolist", "geofield", "geodomain"))
  ) {

    if (is.null(proj)) {
      proj <- "longlat"
    }

    dom_mid <- mid_domain(.data$x, .data$y, proj)

    dx      <- delta(.data$x, expand[1]) / 100
    dy      <- delta(.data$y, expand[2]) / 100
    dom     <- harpCore::define_domain(
      dom_mid["lon"], dom_mid["lat"], 100, c(dx, dy), proj
    )
  } else {
    dom        <- meteogrid::as.geodomain(proj)
    proj_attrs <- dom$projection
    dom_ext    <- harpCore::domain_extent(dom)
    dx         <- delta(c(dom_ext$x0, dom_ext$x1), expand[1]) / 100
    dy         <- delta(c(dom_ext$y0, dom_ext$y1), expand[1]) / 100
    dom        <- harpCore::define_domain(
      dom_ext$clonlat[1], dom_ext$clonlat[2], 100, c(dx, dy),
      meteogrid::proj4.list2str(proj_attrs)
    )
  }

  # Get the map
  map <- get_map(dom, map_db, polygon)

  # If there were issues getting the polygons set polygom to FALSE
  if (!is.element("group", colnames(map))) {
    polygon = FALSE
  }

  # For some reason, stripes can appear in the map data so remove
  if (!polygon) {
    split_map <- split(tidyr::drop_na(map), cumsum(is.na(map$x))[!is.na(map$x)])
    split_map <- split_map[vapply(split_map, function(x) nrow(x) > 2, logical(1))]

    map <- Reduce(
      rbind,
      lapply(split_map, function(x) rbind(x, data.frame(x = NA, y = NA)))
    )
  }

  # Initialize the plot
  gg <- ggplot2::ggplot(map, ggplot2::aes(.data[["x"]], .data[["y"]]))

  # Add the land
  if (polygon) {
    gg <- gg +
      ggplot2::geom_polygon(
        ggplot2::aes(group = .data[["group"]]),
        colour = land_stroke,
        fill   = land_fill
      )
  } else {
    gg <- gg + ggplot2::geom_path(colour = land_stroke)
  }

  # Add the data points
  gg <- gg + ggplot2::geom_point(
    ggplot2::aes(fill = .data[[col_name]]),
    data   = .data,
    shape  = 21,
    size   = point_size,
    colour = point_stroke
  )

  # Make it like a map!
  gg <- gg + ggplot2::coord_equal(expand = FALSE) + theme_harp_map()

  # Fill in the sea colour
  if (polygon) {
    gg <- gg +
      ggplot2::theme(panel.background = ggplot2::element_rect(fill = sea_fill))
  }

  # Set the colour range
  if (col_name %in% c("bias", "mean_bias")) {
    gg <- gg + scale_fill_diff(col_name, direction = -1)
  } else {
    gg <- gg + ggplot2::scale_fill_distiller(
      col_name, palette = "YlOrRd", direction = 1
    )
  }

  gg

}

mid_point <- function(x) {
  min(x) + diff(range(x)) / 2
}

delta <- function(x, expand) {
  diff(range(x)) * (1 + expand)
}

mid_domain <- function(x, y, proj) {
  mid_x <- mid_point(x)
  mid_y <- mid_point(y)

  if (proj == "longlat") {
    return(c(lon = mid_x, lat = mid_y))
  }

  mid_ll <- harpCore::geo_reproject(
    data.frame(x = mid_x, y = mid_y), proj,
    x_col = "x", y_col = "y", inverse = TRUE
  )

  return(c(lon = mid_ll$lon, lat = mid_ll$lat))
}

#' Options for point map plotting
#'
#' Generate a list of options for calling \code{\link{plot_point_map()}}. This
#' is useful for passing options for map plots to
#' \code{\link{plot_point_verif()}}.
#'
#' @inheritParams plot_point_map
#' @export
point_map_opts <- function(
  map_db       = "world",
  proj         = NULL,
  expand       = 0.1,
  polygon      = FALSE,
  land_fill    = "#EEEEEE",
  land_stroke  = "black",
  sea_fill     = "white",
  point_size   = 3,
  point_stroke = "black"
) {
  list(
    map_db       = map_db,
    proj         = proj,
    expand       = expand,
    polygon      = polygon,
    land_fill    = land_fill,
    land_stroke  = land_stroke,
    sea_fill     = sea_fill,
    point_size   = point_size,
    point_stroke = point_stroke
  )
}


