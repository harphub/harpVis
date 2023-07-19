StatGeocontour <- ggproto("StatGeocontour", StatContour,

  required_aes = "geofield",

  default_aes = aes(order = after_stat(level)),

  setup_params = function(data, params) {
    params$z.range <- range(unlist(lapply(data$geofield, range, na.rm = TRUE, finite = TRUE)))
    params
  },

  compute_group = function(
    data, scales, z.range,
    bins = NULL, binwidth = NULL, breaks = NULL, na.rm = FALSE,
    upscale_factor = 1, upscale_method = "mean", downsample_location = "bottom_left"
  ) {

    data <- prep_geofield(
      data$geofield[[1]],
      upscale_factor = upscale_factor,
      method         = upscale_method,
      location       = downsample_location,
      na.rm          = na.rm
    )

    breaks <- ggplot2:::contour_breaks(z.range, bins, binwidth, breaks)

    isolines <- ggplot2:::xyz_to_isolines(data, breaks)
    path_df <- ggplot2:::iso_to_path(isolines, data$group[1])

    path_df$level <- as.numeric(path_df$level)
    path_df$nlevel <- scales::rescale_max(path_df$level)

    path_df

  }
)

StatGeocontourFilled <- ggproto("StatGeocontourFilled", StatContour,

  required_aes = "geofield",

  default_aes = aes(order = after_stat(level), fill = after_stat(level)),

  setup_params = function(data, params) {
    params$z.range <- range(unlist(lapply(data$geofield, range, na.rm = TRUE, finite = TRUE)))
    params
  },

  compute_group = function(
    data, scales, z.range,
    bins = NULL, binwidth = NULL, breaks = NULL, na.rm = FALSE,
    upscale_factor = 1, upscale_method = "mean", downsample_location = "bottom_left"
  ) {

    data <- prep_geofield(
      data$geofield[[1]],
      upscale_factor = upscale_factor,
      method         = upscale_method,
      location       = downsample_location,
      na.rm          = na.rm
    )

    breaks <- ggplot2:::contour_breaks(z.range, bins, binwidth, breaks)

    isobands <- ggplot2:::xyz_to_isobands(data, breaks)
    names(isobands) <- ggplot2:::pretty_isoband_levels(names(isobands))
    path_df <- ggplot2:::iso_to_polygon(isobands, data$group[1])

    path_df$level <- ordered(path_df$level, levels = names(isobands))
    path_df$level_low <- breaks[as.numeric(path_df$level)]
    path_df$level_high <- breaks[as.numeric(path_df$level) + 1]
    path_df$level_mid <- 0.5*(path_df$level_low + path_df$level_high)
    path_df$nlevel <- scales::rescale_max(path_df$level_high)

    path_df

  }
)

