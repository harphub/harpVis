################################################################################
# NATIVE COORDINATES ###########################################################
################################################################################

#' @export
StatXs <-  ggproto("StatXs", Stat,
  required_aes = "xs",

  default_aes = aes(
    x = after_stat(distance),
    y = after_stat(level_number),
    fill = after_stat(value)
  ),

  compute_panel = function(
    data, scales,
    scale_distance = 1,
    level_lims  = c(NA, NA)
  ) {

    if (nrow(data) > 1) {
      cli::cli_warn(c(
        "More than 1 row of data passed - only plotting first row.",
        "i" = "Filter your data, or use {.fn facet_wrap} or {.fn facet_grid} for separate panels."
      ))
    }

    out <- data.frame(
      distance = data$xs[[1]]$distance * scale_distance,
      level_number = data$xs[[1]]$level_number,
      value = data$xs[[1]]$value
    )

    if (any(!is.na(level_lims))) {
      if (!is.na(level_lims[1])) {
        out <- out[out$level_number >= level_lims[1], ]
      }
      if (!is.na(level_lims[2])) {
        out <- out[out$level_number <= level_lims[2], ]
      }
    }
    out
  }

)

#' @export
geom_xs <- function(
  mapping        = NULL,
  data           = NULL,
  na.rm          = FALSE,
  show.legend    = NA,
  inherit.aes    = TRUE,
  scale_distance = 1,
  level_lims     = c(NA, NA),
  interpolate    = FALSE,
  hjust          = 0.5,
  vjust          = 0.5,
  ...
) {
  layer(
    stat = StatXs,
    data = data,
    mapping = mapping,
    geom = GeomRaster,
    position = "identity",
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      scale_distance = scale_distance,
      level_lims     = level_lims,
      interpolate    = interpolate,
      hjust          = hjust,
      vjust          = vjust,
      na.rm          = na.rm,
      ...
    )
  )
}

################################################################################
# PRESSURE COORDINATES #########################################################
################################################################################

#' @export
StatXsPressure <- ggproto("StatXsPressure", Stat,
  required_aes = c("xs", "psfc"),

  setup_data = function(data, params) {
    data$out <- harpCore::hybrid_xs_to_grid(
      data$xs,
      data$psfc,
      vert_res      = params$vert_res,
      out_of_bounds = params$out_of_bounds,
      level_defs    = params$level_defs
    )

    data$topo <- lapply(
      data$psfc,
      function(x) data.frame(
        distance       = x$distance * params$scale_distance,
        topo           = x$value * params$scale_pressure,
        topo_fill      = params$topo_fill,
        topo_colour    = params$topo_colour,
        topo_linewidth = params$topo_linewidth
      )
    )
    data
  },

  default_aes = aes(
    x = after_stat(distance),
    y = after_stat(pressure),
    fill = after_stat(value)
  ),

  extra_params = c(
    "vert_res", "out_of_bounds", "level_defs",
    "topo_fill", "topo_colour", "topo_linewidth", "na.rm"
  ),

  compute_panel = function(
    data, scales,
    scale_pressure = 1,
    scale_distance = 1,
    pressure_lims  = c(NA, NA)
  ) {

    if (nrow(data) > 1) {
      cli::cli_warn(c(
        "More than 1 row of data passed - only plotting first row.",
        "i" = "Filter your data, or use {.fn facet_wrap} or {.fn facet_grid} for separate panels."
      ))
    }

    out <- data.frame(
      distance = data$out[[1]]$distance * scale_distance,
      pressure = data$out[[1]]$pressure * scale_pressure,
      value = data$out[[1]]$value
    )

    if (any(!is.na(pressure_lims))) {
      if (!is.na(pressure_lims[1])) {
        out <- out[out$pressure >= pressure_lims[1], ]
      }
      if (!is.na(pressure_lims[2])) {
        out <- out[out$pressure <= pressure_lims[2], ]
      }
    }

    merge(out, data$topo[[1]])
  }
)

#' 2d vertical cross section
#'
#' @description
#' These geoms visualise vertical cross sections as read in by harpIO
#' functions with `transformation = "xsection"`. For a full represenation of the
#' atmsophere, the data should be on vertical levels direct from the model.
#' Vertical interpolation to pressure levels or height levels is handled by the
#' geom.
#'
#' \itemize{
#'   \item{`geom_xs()`}{Cross section on native levels - i.e. no interpolation}
#'   \item{`geom_xs_pressure()`}{Cross section on model hybrid levels interpolated to pressure levels}
#'   \item{`geom_xs_height()`}{Cross section on model hybrid levels interpolated to height levels}
#' }
#'
#' @section Aesthetics: `geom_xs()` understands the following aesthetics
#'   (required aesthetics are in bold):
#' * \strong{xs} - the column with the cross section data
#' * alpha
#'
#' `geom_xs_pressure()` understands the following aesthetics (required
#' aesthetics are in bold):
#' * \strong{xs} - the column with the cross section data
#' * \strong{psfc} - the column with surface pressure for the cross section
#' * alpha
#'
#' `geom_xs_height()` understands the following aesthetics (required
#' aesthetics are in bold):
#' * \strong{xs} - the column with the cross section data
#' * \strong{psfc} - the column with surface pressure for the cross section
#' * \strong{temp} - the column with the cross section of air temperature
#' * \strong{spec_hum} - the column with the cross section of specific humidity of air
#' * \strong{topo} - the column a representation of the topography cross section
#' * alpha
#'
#' @export
geom_xs_pressure <- function(
  mapping        = NULL,
  data           = NULL,
  na.rm          = FALSE,
  show.legend    = NA,
  inherit.aes    = TRUE,
  vert_res       = 100,
  out_of_bounds  = "squish",
  level_defs     = harpCore::harmonie_levels$full65,
  scale_pressure = 1,
  scale_distance = 1,
  pressure_lims  = c(NA, NA),
  interpolate    = FALSE,
  hjust          = 0.5,
  vjust          = 0.5,
  topo_fill      = "grey20",
  topo_colour    = NA,
  topo_linewidth = 1,
  ...
) {
  layer(
    stat = StatXsPressure,
    data = data,
    mapping = mapping,
    geom = GeomXsPressure,
    position = "identity",
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      vert_res       = vert_res,
      out_of_bounds  = out_of_bounds,
      level_defs     = level_defs,
      scale_pressure = scale_pressure,
      scale_distance = scale_distance,
      pressure_lims  = pressure_lims,
      interpolate    = interpolate,
      hjust          = hjust,
      vjust          = vjust,
      topo_fill      = topo_fill,
      topo_colour    = topo_colour,
      topo_linewidth = topo_linewidth,
      na.rm          = na.rm,
      ...
    )
  )
}

#' @export
GeomXsPressure <- ggproto("GeomXsPressure", GeomRaster,

  draw_panel = function(data, panel_params, coord, interpolate = FALSE,
    hjust = 0.5, vjust = 0.5) {

    # Due to joining the topo data to the rest of the data, topo exists for all
    # levels, so we take a level from somewhere in the middle
    y_vals    <- sort(unique(data$y))
    middle_y  <- y_vals[ceiling(length(y_vals) / 2)]
    vert_res  <- unique(round(abs(diff(unique(data$y))), 5))
    topo_data <- extend_topo(
      data[data$y == middle_y, ], vert_res, TRUE
    )
    grid::gList(
      GeomRaster$draw_panel(
        data, panel_params, coord, interpolate = interpolate,
        hjust = hjust, vjust = vjust
      ),
      GeomRibbon$draw_group(transform(topo_data, alpha = NA), panel_params, coord)
    )
  }

)

################################################################################
# HEIGHT COORDINATES ###########################################################
################################################################################

#' @export
StatXsHeight <- ggproto("StatXsHeight", Stat,
  required_aes = c("xs", "psfc", "temp", "spec_hum", "topo"),

  setup_data = function(data, params) {
    data$out <- harpCore::hybrid_xs_to_grid(
      data$xs,
      data$psfc,
      temp          = data$temp,
      spec_hum      = data$spec_hum,
      topo          = data$topo,
      vert_res      = params$vert_res,
      out_of_bounds = params$out_of_bounds,
      level_defs    = params$level_defs,
      topo_is_geo   = params$topo_is_geo
    )

    if (params$topo_is_geo) {
      topo <- harpCore::geopotential_to_height(data$topo)
    } else {
      topo <- data$topo
    }
    data$topo <- lapply(
      topo,
      function(x) data.frame(
        distance       = x$distance * params$scale_distance,
        topo           = x$value * params$scale_height,
        topo_fill      = params$topo_fill,
        topo_colour    = params$topo_colour,
        topo_linewidth = params$topo_linewidth
      )
    )
    data
  },

  default_aes = aes(
    x = after_stat(distance),
    y = after_stat(height),
    fill = after_stat(value)
  ),

  extra_params = c(
    "vert_res", "out_of_bounds", "level_defs",
    "topo_fill", "topo_colour", "topo_linewidth", "topo_is_geo", "na.rm"
  ),

  compute_panel = function(
    data, scales,
    scale_height   = 1,
    scale_distance = 1,
    height_lims  = c(NA, NA)
  ) {

    if (nrow(data) > 1) {
      cli::cli_warn(c(
        "More than 1 row of data passed - only plotting first row.",
        "i" = "Filter your data, or use {.fn facet_wrap} or {.fn facet_grid} for separate panels."
      ))
    }

    out <- data.frame(
      distance = data$out[[1]]$distance * scale_distance,
      height   = data$out[[1]]$height * scale_height,
      value    = data$out[[1]]$value
    )

    if (any(!is.na(height_lims))) {
      if (!is.na(height_lims[1])) {
        out <- out[out$height >= height_lims[1], ]
      }
      if (!is.na(height_lims[2])) {
        out <- out[out$height <= height_lims[2], ]
      }
    }

    merge(out, data$topo[[1]])
  }
)


#' @export
geom_xs_height <- function(
  mapping        = NULL,
  data           = NULL,
  na.rm          = FALSE,
  show.legend    = NA,
  inherit.aes    = TRUE,
  vert_res       = 10,
  out_of_bounds  = "squish",
  level_defs     = harpCore::harmonie_levels$half65,
  scale_height   = 1,
  scale_distance = 1,
  height_lims    = c(NA, NA),
  interpolate    = FALSE,
  hjust          = 0.5,
  vjust          = 0.5,
  topo_fill      = "grey20",
  topo_colour    = NA,
  topo_linewidth = 1,
  topo_is_geo    = TRUE,
  ...
) {
  layer(
    stat = StatXsHeight,
    data = data,
    mapping = mapping,
    geom = GeomXsHeight,
    position = "identity",
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      vert_res       = vert_res,
      out_of_bounds  = out_of_bounds,
      level_defs     = level_defs,
      scale_height   = scale_height,
      scale_distance = scale_distance,
      height_lims    = height_lims,
      interpolate    = interpolate,
      hjust          = hjust,
      vjust          = vjust,
      topo_fill      = topo_fill,
      topo_colour    = topo_colour,
      topo_linewidth = topo_linewidth,
      topo_is_geo    = topo_is_geo,
      na.rm          = na.rm,
      ...
    )
  )
}

#' @export
GeomXsHeight <- ggproto("GeomXsHeight", GeomRaster,

  draw_panel = function(data, panel_params, coord, interpolate = FALSE,
    hjust = 0.5, vjust = 0.5) {

    # Due to joining the topo data to the rest of the data, topo exists for all
    # levels, so we take a level from somewhere in the middle
    y_vals    <- sort(unique(data$y))
    middle_y  <- y_vals[ceiling(length(y_vals) / 2)]
    vert_res  <- unique(round(abs(diff(unique(data$y))), 10))
    topo_data <- extend_topo(
      data[data$y == middle_y, ], vert_res, FALSE
    )

    grid::gList(
      GeomRaster$draw_panel(
        data, panel_params, coord, interpolate = interpolate,
        hjust = hjust, vjust = vjust
      ),
      GeomRibbon$draw_group(transform(topo_data, alpha = NA), panel_params, coord)
    )
  }

)

# Function to extend topography xsection to fill out the raster.
extend_topo <- function(data, vert_res, reverse) {

  x_extension       <- unique(diff(data$distance)) / 2
  y_gradient        <- diff(data$topo)
  y_extension_left  <- y_gradient[1] / 2
  y_extension_right <- y_gradient[length(y_gradient)] / 2

  panel <- unique(data$PANEL)

  # This is a hack - can only deal with reversed y-axis. Other scale transformations
  # need to somehow be done elsewhere, when we actually know about the scales.
  y_coord <- intersect(colnames(data), c("pressure", "height", "level_number"))
  if (unique(data$y) == -unique(data[[y_coord]])) {
    y_reverse <- -1
  } else {
    y_reverse <- 1
  }

  topo_fill      <- unique(data$topo_fill)
  topo_colour    <- unique(data$topo_colour)
  topo_linewidth <- unique(data$topo_linewidth)

  data <- rbind(
    data.frame(
      distance = data$distance[1] - x_extension,
      topo     = data$topo[1] - y_extension_left
    ),
    data[c("distance", "topo")],
    data.frame(
      distance = data$distance[nrow(data)] + x_extension,
      topo     = data$topo[nrow(data)] + y_extension_right
    )
  )

  if (reverse) {
    data$ymin <- data$topo
    data$ymax <- max(data$topo) + vert_res / 2
  } else {
    data$ymin <- min(data$topo) - vert_res / 2
    data$ymax <- data$topo
  }

  data.frame(
    x         = data$distance,
    ymin      = data$ymin * y_reverse,
    ymax      = data$ymax * y_reverse,
    PANEL     = panel,
    fill      = topo_fill,
    colour    = topo_colour,
    linewidth = topo_linewidth,
    group     = -1
  )
}


################################################################################
# SECTION MAP ##################################################################
################################################################################

#' @export
StatXsMap <- ggproto("StatXsMap", Stat,
  required_aes = "xs",

  setup_data = function(data, params) {

    if (
      meteogrid::is.geodomain(params$map_domain) ||
        meteogrid::is.geofield(params$map_domain)
    ) {
      dom <- rep(list(params$map_domain), nrow(data))
    } else {
      dom <- lapply(data$xs, function(x) attr(x, "domain"))
    }

    is_polygon <- !is.na(params$map_fill)

    data$map <- lapply(
      dom,
      function(x) get_map(x, params$map_data, is_polygon)
    )

    end_points <- lapply(data$xs, function(x) attr(x, "end_points"))
    end_points <- lapply(
      end_points,
      function(x) x[, grep("^x$|^y$", colnames(x), invert = TRUE)]
    )
    data$end_points <- mapply(
      harpCore::geo_reproject, end_points, dom, SIMPLIFY = FALSE
    )
    data
  },

  extra_params = c("map_domain", "map_data", "na.rm"),

  compute_panel = function(
    data, scales,
    map_fill,
    map_colour,
    section_colour,
    section_linewidth,
    section_labels,
    section_point_colour,
    section_point_size,
    section_point_shape,
    section_point_stroke,
    section_point_fill,
    section_label_size,
    section_label_colour,
    section_label_angle,
    section_label_hjust,
    section_label_vjust,
    section_label_family,
    section_label_fontface,
    section_label_lineheight
  ) {

    if (nrow(data) > 1) {
      cli::cli_warn(c(
        "More than 1 row of data passed - only plotting first row.",
        "i" = "Filter your data, or use {.fn facet_wrap} or {.fn facet_grid} for separate panels."
      ))
    }

    if (length(section_label_angle) < 2) {
      section_label_angle <- rep(section_label_angle, 2)
    }
    if (length(section_label_hjust) < 2) {
      section_label_hjust <- rep(section_label_hjust, 2)
    }
    if (length(section_label_vjust) < 2) {
      section_label_vjust <- rep(section_label_vjust, 2)
    }

    out            <- data$map[[1]]

    # get_map can fail to get polygons. In this case there will be no group
    # column so set map_fill to NA.
    if (is.null(out$group)) {
      map_fill <- NA
    }

    out$map_fill   <- map_fill
    out$map_colour <- map_colour

    out$Ax <- data$end_points[[1]]$x[data$end_points[[1]]$end == "a"]
    out$Bx <- data$end_points[[1]]$x[data$end_points[[1]]$end == "b"]
    out$Ay <- data$end_points[[1]]$y[data$end_points[[1]]$end == "a"]
    out$By <- data$end_points[[1]]$y[data$end_points[[1]]$end == "b"]

    out$section_colour           <- section_colour
    out$section_linewidth        <- section_linewidth
    out$section_labelA           <- section_labels[1]
    out$section_labelB           <- section_labels[2]
    out$section_point_colour     <- section_point_colour
    out$section_point_size       <- section_point_size
    out$section_point_shape      <- section_point_shape
    out$section_point_stroke     <- section_point_stroke
    out$section_point_fill       <- section_point_fill
    out$section_label_size       <- section_label_size
    out$section_label_colour     <- section_label_colour
    out$section_label_angleA     <- section_label_angle[1]
    out$section_label_angleB     <- section_label_angle[2]
    out$section_label_hjustA     <- section_label_hjust[1]
    out$section_label_hjustB     <- section_label_hjust[2]
    out$section_label_vjustA     <- section_label_vjust[1]
    out$section_label_vjustB     <- section_label_vjust[2]
    out$section_label_family     <- section_label_family
    out$section_label_fontface   <- section_label_fontface
    out$section_label_lineheight <- section_label_lineheight

    if (is.na(map_fill)) {
      out$group <- -1
    }

    out
  }

)

#' @export
geom_xs_map <- function(
  mapping                  = NULL,
  data                     = NULL,
  map_domain               = NA,
  map_data                 = "world",
  map_fill                 = NA,
  map_colour               = "grey20",
  section_colour           = "red",
  section_linewidth        = 1,
  section_point_colour     = "blue",
  section_point_size       = 3,
  section_point_shape      = 19,
  section_point_stroke     = 0.5,
  section_point_fill       = NA,
  section_labels           = c("A", "B"),
  section_label_size       = 6,
  section_label_colour     = "black",
  section_label_angle      = 0,
  section_label_hjust      = c(1.2, -0.2),
  section_label_vjust      = 0.5,
  section_label_family     = "",
  section_label_fontface   = 1,
  section_label_lineheight = 1.2,
  na.rm                    = FALSE,
  show.legend              = NA,
  inherit.aes              = TRUE,
  ...
) {
  layer(
    stat = StatXsMap,
    data = data,
    mapping = mapping,
    geom = GeomXsMap,
    position = "identity",
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      map_domain               = map_domain,
      map_data                 = map_data,
      map_fill                 = map_fill,
      map_colour               = map_colour,
      section_colour           = section_colour,
      section_linewidth        = section_linewidth,
      section_labels           = section_labels,
      section_point_colour     = section_point_colour,
      section_point_size       = section_point_size,
      section_point_shape      = section_point_shape,
      section_point_stroke     = section_point_stroke,
      section_point_fill       = section_point_fill,
      section_label_size       = section_label_size,
      section_label_colour     = section_label_colour,
      section_label_angle      = section_label_angle,
      section_label_hjust      = section_label_hjust,
      section_label_vjust      = section_label_vjust,
      section_label_family     = section_label_family,
      section_label_fontface   = section_label_fontface,
      section_label_lineheight = section_label_lineheight,
      na.rm                    = na.rm,
      ...
    )
  )
}

#' @export
GeomXsMap <- ggproto("GeomXsMap", GeomPath,

  draw_panel = function(data, panel_params, coord) {

    map_data <- data[c(
      "PANEL", "group", "x", "y", "map_fill", "map_colour",
      "linewidth", "linetype", "alpha"
    )]

    is_polygon <- !all(is.na(map_data$map_fill))

    colnames(map_data) <- gsub("map_", "", colnames(map_data))

    line_data <- data.frame(
      x = c(unique(data$Ax), unique(data$Bx)),
      y = c(unique(data$Ay), unique(data$By)),
      colour = unique(data$section_colour),
      linewidth = unique(data$section_linewidth),
      PANEL = unique(data$PANEL),
      group = -1,
      alpha = NA
    )

    point_data <- line_data[c("x", "y", "PANEL", "group", "alpha")]

    point_data$size   <- unique(data$section_point_size)
    point_data$colour <- unique(data$section_point_colour)
    point_data$shape  <- unique(data$section_point_shape)
    point_data$stroke <- unique(data$section_point_stroke)
    point_data$fill   <- unique(data$section_point_fill)

    label_data <- point_data[c("x", "y", "PANEL", "group", "alpha")]

    label_data$label  <- c(
      unique(data$section_labelA), unique(data$section_labelB)
    )

    label_data$colour <- unique(data$section_label_colour)
    label_data$size   <- unique(data$section_label_size)

    label_data$angle  <- c(
      unique(data$section_label_angleA), unique(data$section_label_angleB)
    )
    label_data$hjust  <- c(
      unique(data$section_label_hjustA), unique(data$section_label_hjustB)
    )
    label_data$vjust  <- c(
      unique(data$section_label_vjustA), unique(data$section_label_vjustB)
    )

    label_data$family     <- unique(data$section_label_family)
    label_data$fontface   <- unique(data$section_label_fontface)
    label_data$lineheight <- unique(data$section_label_lineheight)

    if (is_polygon) {
      grid::gList(
        GeomPolygon$draw_panel(map_data, panel_params, coord),
        GeomLine$draw_panel(line_data, panel_params, coord),
        GeomPoint$draw_panel(point_data, panel_params, coord),
        GeomText$draw_panel(label_data, panel_params, coord)
      )
    } else {
      grid::gList(
        GeomPath$draw_panel(map_data, panel_params, coord),
        GeomLine$draw_panel(line_data, panel_params, coord),
        GeomPoint$draw_panel(point_data, panel_params, coord),
        GeomText$draw_panel(label_data, panel_params, coord)
      )
    }
  }

)

