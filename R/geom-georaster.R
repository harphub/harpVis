#' 2d image of a geofield
#'
#' @description
#' This function visualises `geofield` data as a raster in 2D. It is built
#' on top of \code{\link[ggplot2]{geom_raster}}.
#'
#' It requires data frames with a column containing
#' \code{\link[harpCore]{geofield}}s. This would normally be a
#' \code{\link[harpCore]{geolist}} column, but a standard list column containing
#' geofields will also work.
#'
#' `r lifecycle::badge("experimental")` Smoothing can be achieved by upscaling
#' the data before plotting. This can only be done using an integer scale factor
#' and the methods can be any function that summarises a vector into a single
#' value, for example "mean", "median", "min", "max", or "downsample".
#' Downsampling is simply sampling a pixel within the upscaled pixel - by
#' default this is the centre pixel. Downsampling is the fastest method and is
#' probably sufficient, especially for faceted plots.
#'
#' @inheritParams geom_geocontour
#' @inheritParams ggplot2::geom_raster
#'
#' @section Aesthetics:
#' `geom_raster()` understands the following aesthetics (required aesthetics are
#' in bold):
#' * \strong{geofield}
#' * alpha
#'
#' @export
geom_georaster <- function(
  mapping  = NULL,
  data     = NULL,
  position = "identity",
  ...,
  upscale_factor      = 1,
  upscale_method      = "mean",
  downsample_location = "centre",
  hjust               = 0.5,
  vjust               = 0.5,
  interpolate         = FALSE,
  na.rm               = FALSE,
  show.legend         = NA,
  inherit.aes         = TRUE
) {

  layer(
    data        = data,
    mapping     = mapping,
    stat        = "georaster",
    geom        = GeomRaster,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params      = list(
      upscale_factor      = upscale_factor,
      upscale_method      = upscale_method,
      downsample_location = downsample_location,
      hjust               = hjust,
      vjust               = vjust,
      interpolate         = interpolate,
      na.rm               = na.rm,
      ...
    )
  )


}

