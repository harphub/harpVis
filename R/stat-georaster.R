StatGeoraster <- ggproto("StatGeoraster", Stat,

  required_aes = "geofield",

  default_aes = aes(x = after_stat(x), y = after_stat(y), fill = after_stat(z)),

  compute_panel = function(
    self, data, scales, params,
    upscale_factor = 1, upscale_method = "mean", downsample_location = "bottom_left", na.rm = FALSE
  ) {

    if (nrow(data) > 1) {
      rlang::abort(
        "More than one geofield in data. Use facet_wrap or facet_grid, or filter data before plotting."
      )
    }

    prep_geofield(
      data$geofield[[1]],
      upscale_factor = upscale_factor,
      method         = upscale_method,
      location       = downsample_location,
      na.rm          = na.rm
    )
  }

)
