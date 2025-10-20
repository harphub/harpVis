StatGeowindvec <- ggproto("StatGeowindvec", Stat,

  required_aes = c("u", "v"),

  default_aes = aes(
    x = after_stat(.data$x), y = after_stat(.data$y), xend = after_stat(.data$xend), yend = after_stat(.data$yend)
  ),

  compute_panel = function(self, data, scales, params, rotate = FALSE, skip = 0, max_vec = 20, skip_from = "centre") {

    if (nrow(data) > 1) {
      rlang::abort(
        "More than one geofield rows in data. Use facet_wrap or facet_grid, or filter data before plotting."
      )
    }

    if (!(meteogrid::is.geofield(data$u[[1]]) && meteogrid::is.geofield(data$v[[1]]))) {
      cli::cli_abort(c(
        "{.arg u} and {.arg v} are not both geofields.",
        "x" = "You supplied columns of {.var u = {.cls {class(data$u[[1]])}}} and {.var v = {.cls {class(data$v[[1]])}}}.",
        "i" = "{.var geom_geowindvec()} requires the {.arg u} and {.arg v} aesthetics to map to {.cls geofield} columns."
      ), call = rlang::caller_env())
    }

    geofield_uv_vec_uv(
      data$u[[1]],
      data$v[[1]],
      rotate    = rotate,
      skip      = skip,
      max       = max_vec,
      skip_from = skip_from
    )
  }

)

#' Plot 2d wind vectors
#'
#' @description
#' This function visualises `geofield` _U_ and _V_ wind data as vectors. It is
#' built on top of \code{\link[ggplot2]{geom_segment}}.
#'
#' It requires data frames with columns containing
#' \code{\link[harpCore]{geofield}}s for both the u and v winds. This would
#' normally be a \code{\link[harpCore]{geolist}} column, but a standard list
#' column containing geofields will also work.
#'
#' Only one row of a data frame can be plotted without faceting.
#'
#' @details
#' Wind vectors can be plotted for every grid location in a `geofield`. However,
#' this often leads to vectors that are too close to one another to be seen
#' clearly in the plot. Therefore, you have the option to skip some vectors.
#' Currently only the same skipping can be applied in the x and y directions.
#' The length of the vectors are set so that the magnitude set in `max_vec`
#' results in a vector that spans `skip + 1` grid squares. That is to say that
#' if `max_vec = 10` and `skip = 0`, a vector with a magnitude of 10 will be
#' one grid square long. The lengths of all other vectors are calculated
#' relative to this.
#'
#'
#'
#' @param rotate Whether to rotate the U and V wind components from grid
#'   relative directions to earth relative directions prior to plotting. The
#'   default, `FALSE`, is to retain vectors that are relative to the grid.
#' @param skip The number of grid squares to skip between each vector.
#' @param max_vec The magnitude of a vector with a length equal to `skip + 1`
#'   grid squares.
#' @param skip_from The location of the grid square within a neighbourhood of
#'   skipped grid squares.
#' @inheritParams ggplot2::geom_segment
#'
#' @section Aesthetics:
#' `geom_geowindvec()` understands the following aesthetics (required aesthetics are
#' in bold):
#' * \code{\strong{u}}
#' * \code{\strong{v}}
#' * \code{\link[ggplot2:aes_colour_fill_alpha]{alpha}}
#' * \code{\link[ggplot2:aes_colour_fill_alpha]{colour}}
#' * \code{\link[ggplot2:aes_group_order]{group}}
#' * \code{\link[ggplot2:aes_linetype_size_shape]{linetype}}
#' * \code{\link[ggplot2:aes_linetype_size_shape]{linewidth}}
#'
#' @export
geom_geowindvec <- function(
  mapping  = NULL,
  data     = NULL,
  position = "identity",
  ...,
  rotate              = FALSE,
  skip                = 0,
  max_vec             = 10,
  skip_from           = "centre",
  arrow               = ggplot2::arrow(length = unit(0.15, "cm")),
  arrow.fill          = NULL,
  lineend             = "butt",
  linejoin            = "round",
  na.rm               = FALSE,
  show.legend         = NA,
  inherit.aes         = TRUE
) {

  layer(
    data        = data,
    mapping     = mapping,
    stat        = "geowindvec",
    geom        = GeomSegment,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params      = list(
      rotate              = rotate,
      skip                = skip,
      max_vec             = max_vec,
      skip_from           = skip_from,
      arrow               = arrow,
      arrow.fill          = arrow.fill,
      lineend             = lineend,
      linejoin            = linejoin,
      na.rm               = na.rm,
      ...
    )
  )

}

geofield_uv_vec_uv <- function(geo_u, geo_v, rotate, skip, max, skip_from = "centre") {
  if (!meteogrid::compare.geodomain(
    harpCore::get_domain(geo_u),
    harpCore::get_domain(geo_v)
  )) {
    cli::cli_abort(c(
      "Domain mismatch between U and V.",
      "x" = "You provided columns with U and V geolists on different domains.",
      "i" = "U and V geolist columns must be on the same domain.",
      "i" = "You can use harpCore's geo_*() family of functions to transform",
      "i" = "geolists onto a common domain - e.g geo_regrid()."
    ))
  }

  max_wind_len <- (skip + 1) * harpCore::get_domain(geo_u)[["dx"]]

  orig_extent <- meteogrid::DomainExtent(geo_u)

  geo_spd <- sqrt(geo_u ^ 2 + geo_v ^ 2)

  geo_u <- harpCore::geo_upscale(
    geo_u, factor = (skip + 1), method = "downsample",
    downsample_location = skip_from
  )
  geo_v <- harpCore::geo_upscale(
    geo_v, factor = (skip + 1), method = "downsample",
    downsample_location = skip_from
  )

  if (rotate) {
    winds <- meteogrid::geowind(geo_u, geo_v)
    geo_u <- winds$U
    geo_v <- winds$V
  }

  geo_spd <- sqrt(geo_u ^ 2 + geo_v ^ 2)
  geo_u   <- max_wind_len * geo_u / max
  geo_v   <- max_wind_len * geo_v / max

  # Fudge to make DomainExtent understand geofields with "longlat" as the
  # projection to actually be longlat

  if (attr(geo_u, "domain")[["projection"]][["proj"]] == "longlat") {
    attr(geo_u, "domain")[["projection"]][["proj"]] <- "latlong"
  }

  geo_extent <- meteogrid::DomainExtent(geo_u)

  df   <- expand.grid(
    x = seq(geo_extent$x0, geo_extent$x1, geo_extent$dx),
    y = seq(geo_extent$y0, geo_extent$y1, geo_extent$dy)
  )
  df$u    <- as.vector(geo_u)
  df$xend <- df$x + df$u
  df$v    <- as.vector(geo_v)
  df$yend <- df$y + df$v
  df$mag  <- as.vector(geo_spd)

  # Don't want arrows going out of the plot so remove outer rows and columns
  # where xend or yend go out of the domain

  df

  # dplyr::filter(
  #   df,
  #   .data$x > min(.data$x) & .data$xend > orig_extent$x0,
  #   .data$x < max(.data$x) & .data$xend < orig_extent$x1,
  #   .data$y > min(.data$y) & .data$yend > orig_extent$y0,
  #   .data$y < max(.data$y) & .data$yend < orig_extent$y1
  # )
}

