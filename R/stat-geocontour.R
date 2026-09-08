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
      upscale_factor      = upscale_factor,
      method              = upscale_method,
      downsample_location = downsample_location,
      na.rm               = na.rm
    )

    ggplot2::StatContour$compute_group(
      data = data, scales = scales, z.range = z.range, bins = bins,
      binwidth = binwidth, breaks = breaks, na.rm = na.rm
    )

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

    ggplot2::StatContourFilled$compute_group(
      data = data, scales = scales, z.range = z.range, bins = bins,
      binwidth = binwidth, breaks = breaks, na.rm = na.rm
    )

  }
)

