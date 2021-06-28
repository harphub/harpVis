#' Title
#'
#' @param mapping
#' @param data
#' @param position
#' @param ...
#' @param upscale_factor
#' @param upscale_method
#' @param downsample_location
#' @param na.rm
#' @param show.legend
#' @param inherit.aes
#'
#' @return
#' @export
#'
#' @examples
geom_geocontour <- function(
  mapping  = NULL,
  data     = NULL,
  position = "identity",
  ...,
  upscale_factor      = 1,
  upscale_method      = "mean",
  downsample_location = "bottom_left",
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
      upscale_factor      = upscale_factor,
      upscale_method      = upscale_method,
      downsample_location = downsample_location,
      na.rm               = na.rm,
      ...
    )
  )


}

#' Title
#'
#' @param mapping
#' @param data
#' @param position
#' @param ...
#' @param upscale_factor
#' @param upscale_method
#' @param downsample_location
#' @param na.rm
#' @param show.legend
#' @param inherit.aes
#'
#' @return
#' @export
#'
#' @examples
geom_geocontour_filled <- function(
  mapping  = NULL,
  data     = NULL,
  position = "identity",
  ...,
  upscale_factor      = 1,
  upscale_method      = "mean",
  downsample_location = "bottom_left",
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
      upscale_factor      = upscale_factor,
      upscale_method      = upscale_method,
      downsample_location = downsample_location,
      na.rm               = na.rm,
      ...
    )
  )


}

