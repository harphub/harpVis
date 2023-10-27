#' 2d contours of a geofield
#'
#' @description
#' These functions visualise `geofield` data as contours in 2D. They are built
#' on top of \code{\link[ggplot2]{geom_contour}} and
#' \code{\link[ggplot2]{geom_contour_filled}}.
#'
#' They require data frames with a column containing
#' \code{\link[harpCore]{geofield}}s. This would normally be a
#' \code{\link[harpCore]{geolist}} column, but a standard list column containing
#' geofields will also work.
#'
#' `r lifecycle::badge("experimental")` Smoothing can be achieved by upscaling
#' the data before plotting. This can only be done using an integer scale factor
#' and the methods can be any function that summarises a vector into a single
#' value, for example "mean", "median", "min", "max", or "downsample".
#' Downsampling is simply sampling a pixel within the upscaled pixel - by
#' default this is the centre.
#'
#' @inheritParams ggplot2::geom_contour
#' @param upscale_factor An integer by which to upscale the data before
#'   computing the contours. For example if this is 2, the upscaled grid will
#'   have 2 pixels in each direction from the grid before upscaling.
#' @param upscale_method The method used for upscaling. See
#'   \code{\link[harpCore]{geo_upscale}} for more details.
#' @inheritParams harpCore::geo_upscale
#'
#' @section Aesthetics: `geom_geocontour()` understands the following aesthetics
#'   (required aesthetics are in bold):
#' * \strong{geofield}
#' * colour
#' * alpha
#' * group
#' * linetype
#' * linewidth
#' * weight
#'
#' `geom_geocontour_filled()` understands the following aesthetics (required
#' aesthetics are in bold):
#' * \strong{geofield}
#' * colour
#' * fill
#' * alpha
#' * group
#' * linetype
#' * linewidth
#' * weight
#'
#' @inheritSection ggplot2::geom_contour Computed variables
#'
#' @export
#'
geom_geocontour <- function(
  mapping  = NULL,
  data     = NULL,
  position = "identity",
  ...,
  bins                = NULL,
  binwidth            = NULL,
  breaks              = NULL,
  lineend             = "butt",
  linejoin            = "round",
  linemitre           = 10,
  upscale_factor      = 1,
  upscale_method      = "mean",
  downsample_location = "centre",
  na.rm               = FALSE,
  show.legend         = NA,
  inherit.aes         = TRUE
) {

  layer(
    data        = data,
    mapping     = mapping,
    stat        = "geocontour",
    geom        = GeomContour,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params      = list(
      bins                = bins,
      binwidth            = binwidth,
      breaks              = breaks,
      upscale_factor      = upscale_factor,
      upscale_method      = upscale_method,
      downsample_location = downsample_location,
      na.rm               = na.rm,
      ...
    )
  )


}

#' @rdname geom_geocontour
#' @export
geom_geocontour_filled <- function(
  mapping  = NULL,
  data     = NULL,
  position = "identity",
  ...,
  bins                = NULL,
  binwidth            = NULL,
  breaks              = NULL,
  upscale_factor      = 1,
  upscale_method      = "mean",
  downsample_location = "centre",
  na.rm               = FALSE,
  show.legend         = NA,
  inherit.aes         = TRUE
) {

  layer(
    data        = data,
    mapping     = mapping,
    stat        = "geocontour_filled",
    geom        = GeomPolygon,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params      = list(
      bins                = bins,
      binwidth            = binwidth,
      breaks              = breaks,
      upscale_factor      = upscale_factor,
      upscale_method      = upscale_method,
      downsample_location = downsample_location,
      na.rm               = na.rm,
      ...
    )
  )


}

