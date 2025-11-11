#' Plot method for harp_grid_df data frames
#'
#' Plots rasters of the chosen column, `col`, faceted by `facet_col` with a map.
#' For faster plotting `upscale_factor` could be increased. This method uses
#' `ggplot` to make the plots so colour scales, labels, themes etc. can be
#' changed or added in the usual way for `ggplot`.
#'
#' In order to speed up plotting, data will be automatically upscaled before
#' plotting using the downsample method. This behaviour can be overridden by
#' setting `upscale_factor` to an integer value where `upscale_factor = 1` means
#' that no upscaling is done prior to plotting.
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
#' @param upscale_factor Set to an integer value to specify any upscaling of
#'   the data before plotting. For example if `upscale_factor = 2`, each pixel
#'   in the upscaled grid will represent 2 pixels in each direction from the
#'   original grid. The default behaviour (`upscale_factor = NULL`) is to apply
#'   an automatic upscaling that optimises plotting speed without losing too
#'   much detail.
#' @param plot_land If `poly = TRUE`, when to plot the land polygons in relation
#'   to the raster data. Can be "before" (the default) or "after". If "after",
#'   the land polygons will overlay the rasters. Can also be set to `"none"` to
#'   not plot any coastline or country borders.
#' @inheritParams geom_georaster
#' @param ... Other arguments to \code{\link{geom_georaster()}}.
#'
#' @export
#' @examples
#' \dontrun{
#'   # This example reads open data from the Norwegian Meteorological Institute.
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
#'   # For highest quality plotting, set upscale_factor = 1. This will impact
#'   # the plotting speed.
#'   plot(tt, upscale_factor = 1)
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
  upscale_factor  = NULL,
  upscale_method  = "downsample",
  plot_land       = c("before", "after", "none"),
  ...
) {

  col_quo <- rlang::enquo(col)
  if (rlang::quo_is_null(col_quo)) {
    geolist_cols <- find_geolist_cols(x)
  } else {
    geolist_cols <- gsub(
      "c\\(| |\\)",
      "",
      strsplit(rlang::quo_name(col_quo), ",")[[1]]
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

  if (rlang::quo_is_null(col_quo)) {
    col_quo <- rlang::sym(geolist_cols)
  }
  col_name <- colnames(x)[colnames(x) == rlang::quo_name(col_quo)]

  if (length(col_name) < 1) {
    cli::cli_abort(c(
      "Requested column not found in data!",
      "x" = "You supplied {.arg col} = {rlang::quo_name(col)}.",
      "i" = "{.arg col} should be one of {.or find_geolist_cols(x)}."
    ))
  }

  if (length(col_name) > 1) {
    cli::cli_abort(c(
      "Cannot plot data from more than one column!",
      "x" = "You supplied {.arg col} = {rlang::quo_name(col)}.",
      "i" = "{.arg col} should be ONE of {.or find_geolist_cols(x)}."
    ))
  }

  # For a 1 row data frame with no valid_dttm column, we can remove the
  # need to facet

  facet_col      <- rlang::enquo(facet_col)
  facet_col_name <- rlang::quo_name(facet_col)

  faceting <- TRUE
  if (!is.element("valid_dttm", colnames(x)) && facet_col_name == "valid_dttm") {
    faceting <- FALSE
  }
  if (rlang::quo_is_null(facet_col)) {
    faceting <- FALSE
  }

  if (faceting && !is.element(facet_col_name, colnames(x))) {
    faceting_cols <- setdiff(colnames(x), geolist_cols(x))
    if (length(faceting_cols) < 1) {
      info_msg <- paste(
        "Either filter your data to a single row, or add a column that can be",
        "used for faceting"
      )
    } else {
      info_msg <- "{.arg facet_col} should be one of {.or faceting_cols}."
    }
    cli::cli_abort(c(
      "Requested faceting column not found in data!",
      "x" = "You supplied {.arg facet_col} = {facet_col_name}",
      "i" = info_msg
    ))
  }

  plot_land <- match.arg(plot_land)

  land_map <- get_map(x, map = map_db, polygon = poly, col = !!col_quo)
  if (!is.element("group", colnames(land_map))) {
    poly = FALSE
  }

  if (nrow(land_map) < 2) {
    plot_land = "none"
  }


  gg <- ggplot2::ggplot()

  if (poly && plot_land == "before") {
    gg <- gg + ggplot2::geom_polygon(
      ggplot2::aes(.data[["x"]], .data[["y"]], group = .data[["group"]]),
      land_map, fill = land_colour, colour = country_outline
    )
  }

  # Automatically set the upscale factor - assume the maximum number of
  # pixels in each direction should be ~800.

  if (is.null(upscale_factor)) {
    facets_length  <- ceiling(sqrt(nrow(x)))
    dom            <- harpCore::get_domain(x[[col_name]])
    max_pixels     <- max(c(dom$nx * facets_length), c(dom$ny * facets_length))
    upscale_factor <- ceiling(max_pixels / 800)
    cli::cli_inform(
      paste(
        "Plotting with",
        cli::col_br_red("upscale_factor = {upscale_factor}")
      )
    )
  }

  gg <- gg +
    geom_georaster(
      mapping        = ggplot2::aes(geofield = .data[[col_name]]),
      data           = x,
      upscale_factor = upscale_factor,
      upscale_method = upscale_method,
      ...
    )

  if (plot_land != "none") {
    if (poly) {

      if (plot_land == "after") {
        gg <- gg + ggplot2::geom_polygon(
          ggplot2::aes(.data[["x"]], .data[["y"]], group = .data[["group"]]),
          land_map, fill = land_colour
        )
      }

      gg <- gg + ggplot2::geom_path(
        ggplot2::aes(.data[["x"]], .data[["y"]], group = .data[["group"]]),
        land_map, colour = country_outline
      )

    } else {
      gg <- gg + ggplot2::geom_path(
        ggplot2::aes(.data[["x"]], .data[["y"]]),
        land_map, colour = country_outline
      )
    }
  }

  if (faceting) {
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

#' Out of bounds handling
#'
#' These functions should mostly be used for handling out of bounds data
#' for colour scales fo \code{\link{geom_georaster()}}. Censored values outside
#' the limits are removed and squished values outside the limits are set to the
#' maxumum limits. The functions' main purpose is to be passed to the `oob`
#' argument in a ggplot scale function, and would be particularly useful for
#' plotting fields such as precipitation.
#'
#' @inheritParams scales::censor
#' @export
#' @examples
#' # Create a 2d array
#' z <- array(dim = c(100, 100))
#' for (i in 1:100) {
#'   for (j in 1:100) {
#'     z[i, j] <- sin(i / 10) + sin(j / 10)
#'   }
#' }
#'
#' # Define a domain
#' dom <- define_domain(11, 59, 100, 0.2, "longlat")
#'
#' df <- data.frame(valid_dttm = as_dttm(2024091700))
#' df$data <- geolist(geofield(z, domain = dom))
#' df <- as_harp_df(df)
#'
#' # Normal plot
#' plot(df)
#'
#' # Add scales to only show values between -1 and 1
#' # All values are censored
#' plot(df) + scale_fill_viridis_c(limits = c(-1, 1))
#'
#' # Censor the low values and squish the high values
#' plot(df) +
#'   scale_fill_viridis_c(limits = c(-1, 1), oob = censor_low_squish_high)
#'
#' # Censor high values and squish low values
#' plot(df) +
#'   scale_fill_viridis_c(limits = c(-1, 1), oob = squish_low_censor_high)
censor_low_squish_high <- function(x, range = c(0, 1), only.finite = TRUE) {
  force(range)
  finite <- if (only.finite)
    is.finite(x)
  else TRUE
  x[finite & x < range[1]] <- NA_real_
  x[finite & x > range[2]] <- range[2]
  x
}

#' @export
#' @rdname censor_low_squish_high
squish_low_censor_high <- function(x, range = c(0, 1), only.finite = TRUE) {
  force(range)
  finite <- if (only.finite)
    is.finite(x)
  else TRUE
  x[finite & x < range[1]] <- range[1]
  x[finite & x > range[2]] <- NA_real_
  x
}


#' The absolute range of a diverging sequence
#'
#' Given a sequence that contains both negative and positive numbers,
#' `abs_range()` returns negative and positive limits that are equidistant
#' from zero. The function can be passed to the `limits` argument of a _ggplot_
#' scale to ensure the limits are are equally distributed about zero.
#'
#' @param x A numeric vector
#'
#' @return Negative and postive limits that are
#' @export
#'
#' @examples
#' abs_range(seq(-3, 6))
#'
#' # Use in ggplot
#' library(ggplot2)
#' df <- expand.grid(x = seq_len(100), y = seq_len(100))
#' df$z <- sin(df$x / 10) + sin(df$y / 10) + 1
#'
#' # Standard scaling
#' ggplot(df, aes(x, y, fill = z)) +
#'   geom_raster() +
#'   scale_fill_gradient2()
#'
#' # Use abs_range to ensure whole scale is included in legend
#' ggplot(df, aes(x, y, fill = z)) +
#'   geom_raster() +
#'   scale_fill_gradient2(limits = abs_range)
#'
#' # Use abs_range to ensure colours in scale_fill_gradientn() are equally
#' # distributed about zero
#'
#' # Normal behaviour
#' ggplot(df, aes(x, y, fill = z)) +
#'   geom_raster() +
#'   scale_fill_gradientn(colours = c("darkgreen", "white", "deeppink4"))
#'
#' # Using abs_range to set limits
#' ggplot(df, aes(x, y, fill = z)) +
#'   geom_raster() +
#'   scale_fill_gradientn(
#'     colours = c("darkgreen", "white", "deeppink4"), limits = abs_range
#'   )
abs_range <- function(x) {
  stopifnot(is.numeric(x))
  c(-max(abs(x)), max(abs(x)))
}

# Scales

#' Special colour scales
#'
#' These colour scales modify _ggplot_ colour scales for specific purposes.
#' `scale_fill_diff` modifies \code{\link[ggplot2]{scale_fill_gradient2}} by
#' making the colour scale even both sides of zero, rather than the range each
#' side of zero. `scale_fill_precip` modifies
#' \code{\link[ggplot2]{scale_fill_gradientn}}, creating a continuous colour
#' scale that is suitable for showing precipitation on a map - it uses a log2
#' transformation such that the colours scale with powers of 2. It also sets
#' a minimum limit of 0.1 and squishes an upper limit of 32,
#' `scale_fill_precip2_b` is the binned variant of this scale and modifies
#' \code{\link[ggplot2]{scale_fill_stepsn}}.
#'
#' @name colour_scales
NULL

#' @inheritParams ggplot2::scale_fill_gradient2
#' @rdname colour_scales
#' @export
#'
#' @examples
#' # scale_fill_diff has an even colour bar
#' df <- expand.grid(x = 1:100, y = 1:100)
#' df$value <- rnorm(nrow(df), -2)
#'
#' p <- ggplot(df, aes(x, y, fill = value)) +
#'   geom_raster()
#'
#' p + scale_fill_diff()
#'
#' # Compare with legend for scale_fill_gradient2()
#' p + scale_fill_gradient2()
scale_fill_diff <- function(
  ...,
  name = "difference",
  limits = abs_range,
  low = scales::muted("red"),
  mid = "white",
  high = scales::muted("blue")
) {
  ggplot2::scale_fill_gradient2(
    ..., name = name, limits = limits, low = low, mid = mid, high = high
  )
}

#' @inheritParams ggplot2::scale_fill_gradientn
#' @rdname colour_scales
#' @export
#'
#' @examples
#' \dontrun{
#'   # Read in some precipitation data from the Norwegian Meteorological
#'   # Institute Thredds server
#'   library(harpIO)
#'   precip <- read_grid(
#'     "https://thredds.met.no/thredds/dodsC/metpparchive/2024/07/24/met_analysis_1_0km_nordic_20240724T15Z.nc",
#'     "precipitation_amount",
#'     file_format      = "netcdf",
#'     file_format_opts = netcdf_opts(proj4_var = "projection_lcc"),
#'     data_frame       = TRUE
#'   )
#'
#'   p <- plot(precip)
#'   p
#'
#'   # Use scale_fill_precip()
#'   p + scale_fill_precip()
#'
#'   # With the banded variant
#'   p + scale_fill_precip_b()
#' }
scale_fill_precip <- function(
  ...,
  name      = "mm",
  transform = "log2",
  colours   = viridisLite::viridis(256, option = "G", direction = -1),
  limits    = c(0.1, 32),
  oob       = censor_low_squish_high,
  na.value  = NA
) {
  scale_fill_gradientn(
    ..., name = name, transform = transform, colours = colours, limits = limits,
    oob = oob, na.value = na.value
  )
}

#' @inheritParams ggplot2::scale_fill_stepsn
#' @rdname colour_scales
#' @export
scale_fill_precip_b <- function(
  ...,
  name      = "mm",
  transform = "log2",
  colours   = viridisLite::viridis(256, option = "G", direction = -1),
  limits    = c(0.1, 32),
  oob       = censor_low_squish_high,
  na.value  = NA,
  n.breaks  = 8
) {
  scale_fill_stepsn(
    ..., name = name, transform = transform, colours = colours, limits = limits,
    oob = oob, na.value = na.value, n.breaks = n.breaks
  )
}
