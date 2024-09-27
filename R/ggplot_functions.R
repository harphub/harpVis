#' Plot method for harp_grid_df data frames
#'
#' Plots rasters of the chosen column, `col`, faceted by `facet_col` with a map.
#' For faster plotting `upscale_factor` could be increased. This method uses
#' `ggplot` to make the plots so colour scales, labels, themes etc. can be
#' changed or added in the usual way for `ggplot`.
#'
#'
#' @param x a `harp_grid_df` data frame.
#' @param col <[`tidy-select`][dplyr_tidy_select]> The column to plot.
#' @param facet_col <[`tidy-select`][dplyr_tidy_select]> The column to facet by.
#' @param map_db The map data base to use. By default this uses "world" from the
#'   _maps_ package, but if you have, for example, the _rnaturalearth_ package
#'   installed you could use these.
#' @param poly Logical. Whether the map should be drawn as paths (`FALSE`), or
#'   as polygons that can be filled (`TRUE`). Default is `FALSE`.
#' @param land_colour If `poly = TRUE`, the fill colour for land polygons.
#' @param country_outline The colour of the country outlines.
#' @param water_colour If `poly = TRUE`, the colour of water areas.
#' @param plot_land If `poly = TRUE`, when to plot the land polygons in relation
#'   to the raster data. Can be "before" (the default) or "after". If "after",
#'   the land polygons will overlay the rasters.
#' @inheritParams geom_georaster
#' @param ... Other arguments to \code{\link{geom_georaster()}}.
#'
#' @export
#' @examples
#' \dontrun{
#'   # This example reads open data from the Norwegian Meteorogical Institute.
#'   # The ncdf4 and harpIO packages are required.
#'
#'   library(harpIO)
#'   tt <- read_grid(
#'     paste0(
#'       "https://thredds.met.no/thredds/dodsC/aromearcticarchive/2024/09/27/",
#'       "arome_arctic_det_2_5km_20240927T09Z.nc"
#'     ),
#'     "t2m",
#'     lead_time = seq(0, 5),
#'     file_format = "netcdf",
#'     data_frame  = TRUE
#'   )
#'
#'   # If there is only 1 geolist column you don't need to specify anything
#'   plot(tt)
#'
#'   # For faster plotting increase the upscale_factor, though note loss of
#'   # quality
#'   plot(tt, upscale_factor = 4)
#'
#'   # You can change the colour scale by adding a new fill scale
#'   plot(tt, upscale_factor = 4) +
#'     scale_fill_gradient(low = "yellow", high = "red")
#'
#'   # Modify the data during the column specification
#'   plot(tt, col = gridded_data - 273.15, upscale_factor = 4)
#'
#'   # Colour the land and the sea - this is best illustrated with a paremeter
#'   # like precipitation.
#'   pcp <- read_grid(
#'     paste0(
#'       "https://thredds.met.no/thredds/dodsC/aromearcticarchive/2024/09/27/",
#'       "arome_arctic_det_2_5km_20240927T09Z.nc"
#'     ),
#'     "pcp",
#'     lead_time = seq(9, 15),
#'     file_format = "netcdf",
#'     data_frame  = TRUE
#'   )
#'   # Since we used read_grid, we have to convert the data frame to one that
#'   # harp recognises as a deterministic forecast so we can decumulate
#'   colnames(pcp)[colnames(pcp) == "gridded_data"] <- "aa_det"
#'   pcp <- decum(as_harp_df(pcp), 1)[2:nrow(pcp), ]
#'   plot(
#'     pcp, upscale_factor = 4, poly = TRUE,
#'     water_colour = "#AADDFF", land_colour = "#77BB99"
#'   ) +
#'   scale_fill_gradientn(
#'     colours  = c("grey80", "steelblue", "violetred4"),
#'     limits   = c(0.125, NA),
#'     na.value = "transparent",
#'     trans    = "log",
#'     breaks   = seq_double(0.125, 8)
#'   )
#'
#'   # Or you can overlay land on the temperature plot to only show temperature
#'   # over the sea. (Note that to do the opposite you will need land mask data)
#'   # Can also use labs() to change the title & legend title.
#'   plot(tt, upscale_factor = 4, poly = TRUE, plot_land = "after") +
#'     labs(
#'       title = "Temperature over the sea",
#'       fill  = "[K]"
#'     )
#' }
#'
plot.harp_grid_df <- function(
  x,
  col             = NULL,
  facet_col       = "valid_dttm",
  map_db          = "world",
  poly            = FALSE,
  land_colour     = "grey70",
  country_outline = "grey30",
  water_colour    = NULL,
  upscale_factor  = 1,
  upscale_method  = "downsample",
  plot_land       = c("before", "after"),
  ...
) {

  col <- rlang::enquo(col)
  if (rlang::quo_is_null(col)) {
    geolist_cols <- find_geolist_cols(x)
  } else {
    geolist_cols <- gsub(
      "c\\(| |\\)",
      "",
      strsplit(rlang::quo_name(col), ",")[[1]]
    )
  }

  if (length(geolist_cols) > 1) {
    cli::cli_abort(c(
      "More than one {.cls geolist} columns in data frame.",
      "x" = "You can only plot one {.cls geolist} column.",
      "i" = "You can select which column to plot by passing one of",
      "i" = "{glue::glue_collapse(geolist_cols, last = ' or ')} to the {.arg col} argument."
    ))
  }

  if (rlang::quo_is_null(col)) {
    col <- rlang::sym(geolist_cols)
  }
  col_name <- colnames(x)[
    vapply(colnames(x), grepl, rlang::quo_name(col), FUN.VALUE = logical(1))
  ]

  facet_col      <- rlang::enquo(facet_col)
  facet_col_name <- rlang::quo_name(facet_col)

  plot_land <- match.arg(plot_land)

  if (!is.element(facet_col_name, colnames(x))) {
    cli::cli_abort(c(

    ))
  }

  land_map <- get_map(x, map = map_db, polygon = poly, col = {{col_name}})
  if (!is.element("group", colnames(land_map))) {
    poly = FALSE
  }


  gg <- ggplot2::ggplot(x, ggplot2::aes(geofield = !!col))

  if (poly && plot_land == "before") {
    gg <- gg + ggplot2::geom_polygon(
      ggplot2::aes(.data[["x"]], .data[["y"]], group = .data[["group"]]),
      land_map, fill = land_colour, colour = country_outline,
      inherit.aes = FALSE
    )
  }

  gg <- gg +
    geom_georaster(
      upscale_factor = upscale_factor, upscale_method = upscale_method, ...
    )

  if (poly) {

    if (plot_land == "after") {
      gg <- gg + ggplot2::geom_polygon(
        ggplot2::aes(.data[["x"]], .data[["y"]], group = .data[["group"]]),
        land_map, fill = land_colour, inherit.aes = FALSE
      )
    }

    gg <- gg + ggplot2::geom_path(
      ggplot2::aes(.data[["x"]], .data[["y"]], group = .data[["group"]]),
      land_map, colour = country_outline, inherit.aes = FALSE
    )

  } else {
    gg <- gg + ggplot2::geom_path(
      ggplot2::aes(.data[["x"]], .data[["y"]]),
      land_map, colour = country_outline, inherit.aes = FALSE
    )
  }

  if (nrow(x) > 1) {
    gg <- gg + ggplot2::facet_wrap(facet_col_name)
  }

  gg <- gg +
    ggplot2::scale_fill_viridis_c() +
    ggplot2::labs(fill = col_name)

  gg <- gg +
    ggplot2::coord_equal(expand = FALSE) +
    theme_harp_map()

  if (poly && !is.null(water_colour)) {
    gg <- gg +
      ggplot2::theme(
        panel.background = ggplot2::element_rect(fill = water_colour)
      )
  }

  gg

}

find_geolist_cols <- function(x) {
  names(which(sapply(x, harpCore::is_geolist)))
}

check_quo_col_length <- function(x, max_len = 1) {
  x <- rlang::enquo(x)
  cols <- strsplit(rlang::quo_name(x), ",")
  cols <- gsub("")
}


