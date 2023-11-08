#' Connect control points/observations with an X-spline
#'
#' Draw an X-spline line - a curve drawn relative to control
#' points/observations. Patterned after \code{\link[ggplot2]{geom_line}} in that
#' it orders the points by \code{x} first before computing the splines.
#'
#' @section Aesthetics: \code{geom_linespline} understands the following
#'   aesthetics (required aesthetics are in bold): \itemize{ \item
#'   \strong{\code{x}} \item \strong{\code{y}} \item \code{alpha}
#'   \item \code{color} \item \strong{\code{fill}} \item
#'   \strong{\code{group}} \item \code{linetype} \item \code{size} }
#'
#' @seealso \code{\link[ggplot2]{geom_line}}: Connect observations;
#'   \code{\link[ggplot2]{geom_path}}: Connect observations;
#'   \code{\link[ggplot2]{geom_polygon}}: Filled paths (polygons);
#'   \code{\link[ggplot2]{geom_segment}}: Line segments;
#'
#' @details An X-spline is a line drawn relative to control points. For each
#'   control point, the line may pass through (interpolate) the control point or
#'   it may only approach (approximate) the control point; the behaviour is
#'   determined by a shape parameter for each control point.
#'
#'   If the shape parameter is greater than zero, the spline approximates the
#'   control points (and is very similar to a cubic B-spline when the shape is
#'   1). If the shape parameter is less than zero, the spline interpolates the
#'   control points (and is very similar to a Catmull-Rom spline when the shape
#'   is -1). If the shape parameter is 0, the spline forms a sharp corner at
#'   that control point.
#'
#'   For open X-splines, the start and end control points must have a shape of 0
#'   (and non-zero values are silently converted to zero).
#'
#'   For open X-splines, by default the start and end control points are
#'   replicated before the curve is drawn. A curve is drawn between
#'   (interpolating or approximating) the second and third of each set of four
#'   control points, so this default behaviour ensures that the resulting curve
#'   starts at the first control point you have specified and ends at the last
#'   control point. The default behaviour can be turned off via the repEnds
#'   argument.
#'
#' @inheritParams ggplot2::geom_line
#' @param geom,stat Use to override the default connection between
#'   \code{geom_linespline} and \code{stat_linespline}.
#' @param spline_shape A numeric vector of values between -1 and 1, which
#'   control the shape of the spline relative to the control points.
#' @param open A logical value indicating whether the spline is an open or a
#'   closed shape.
#' @param rep_ends For open X-splines, a logical value indicating whether the
#'   first and last control points should be replicated for drawing the curve.
#'   Ignored for closed X-splines.
#' @references Blanc, C. and Schlick, C. (1995), "X-splines : A Spline Model
#'   Designed for the End User", in \emph{Proceedings of SIGGRAPH 95}, pp.
#'   377-386. \url{http://dept-info.labri.fr/~schlick/DOC/sig1.html}
#' @export
#' @family xspline implementations
geom_linespline <- function(mapping = NULL, data = NULL, stat = "linespline",
  position = "identity", na.rm = TRUE, show.legend = NA,
  inherit.aes = TRUE,
  spline_shape=-0.15, open=TRUE, rep_ends=TRUE, ...) {
  layer(
    geom = GeomLinespline,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      spline_shape = spline_shape,
      open = open,
      na.rm = na.rm,
      rep_ends = rep_ends,
      ...
    )
  )
}

#' Geom Proto
#' @rdname harpVis-ggproto
#' @format NULL
#' @usage NULL
#' @keywords internal
#' @export
GeomLinespline <- ggproto("GeomLinespline", GeomLine,
  required_aes = c("x", "y"),
  default_aes = aes(colour = "black", fill = NA, linewidth = 0.5, linetype = 1, alpha = NA)
)

#' @export
#' @rdname geom_linespline
#' @section Computed variables:
#' \itemize{
#'   \item{x}
#'   \item{y}
#' }
stat_linespline <- function(mapping = NULL, data = NULL, geom = "line",
  position = "identity", na.rm = TRUE, show.legend = NA, inherit.aes = TRUE,
  spline_shape=-0.25, open=TRUE, rep_ends=TRUE, ...) {
  layer(
    stat = StatLinespline,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(spline_shape=spline_shape,
      open=open,
      na.rm = na.rm,
      rep_ends=rep_ends,
      ...
    )
  )
}

#' @rdname harpVis-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatLinespline <- ggproto("StatLinespline", Stat,

  required_aes = c("x", "y"),

  compute_group = function(self, data, scales, params,
    spline_shape=-0.15, open=TRUE, rep_ends=TRUE) {

    tf <- tempfile(fileext=".png")
    png(tf)
    plot.new()
    tmp <- graphics::xspline(
      data$x, data$y, spline_shape, open, rep_ends, draw=FALSE, NA, NA
    )
    invisible(dev.off())
    unlink(tf)

    data.frame(x=tmp$x, y=tmp$y)
  }

)
