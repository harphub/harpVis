#' Plot a harp_fcst 2d field
#'
#' @param .fcst A harp_fcst object or harp_spatial_fcst data frame
#' @param .name The name of the model to plot. This selects the model from the
#'   harp_fcst list to plot. Can be omitted if there is only one model in the
#'   harp_fcst list.
#' @param col The column of data to plot from. Should be unquoted, or if a
#'   variable, wrapped in \{\{\}\}
#' @param fcdate The forecast date to plot in "YYYYMMDDhh" or similar format.
#'   Can be omitted if there is only one date in the data.
#' @param lead_time The lead time to plot. Can be omitted if there is only one
#'   lead time in the data.
#' @param ... Other filtering rules
#' @param palette Colour palette to use. This should be a vector of colours.
#' @param num_breaks Number of coloyur breaks to use in the plot.
#' @param breaks The values to use for colour breaks. If not NULL, breaks has
#'   priority over num_breaks.
#'
#' @return A plot
#' @export
#'
#' @examples
plot_field <- function(
  .fcst,
  .name,
  col,
  fcdate,
  lead_time,
  ...,
  palette    = viridis::viridis(255),
  num_breaks = 15,
  breaks     = NULL
) {
  UseMethod("plot_field")
}

#' @export
plot_field.harp_spatial_fcst <- function(
  .fcst,
  .name,
  col,
  fcdate,
  lead_time,
  ...,
  palette    = viridis::viridis(255),
  num_breaks = 15,
  breaks     = NULL
) {

  col <- rlang::enquo(col)

  if (missing(fcdate) && length(unique(.fcst[["fcdate"]])) == 1) {
    fcdate <- harpIO::unixtime_to_str_datetime(as.numeric(unique(.fcst[["fcdate"]])), harpIO::YMDhms)
  }

  if (missing(lead_time) && length(unique(.fcst[["lead_time"]])) == 1) {
    lead_time <- unique(.fcst[["lead_time"]])
  }

  if (!all(sapply(dplyr::pull(.fcst, !!col), meteogrid::is.geofield))) {
    stop("Selected col: '", col, "' does not contain geofield objects.", call. = FALSE)
  }

  fcdate_filter    <- harpIO::str_datetime_to_datetime(fcdate)
  lead_time_filter <- lead_time

  .fcst <- dplyr::filter(
    .fcst,
    .data[["fcdate"]]    == fcdate_filter,
    .data[["lead_time"]] == lead_time_filter,
    ...
  )

  if (nrow(.fcst) < 1) {
    stop("Nothing to plot", call. = FALSE)
  }

  if (nrow(.fcst) > 1) {
    message("Filtered data to plot:")
    print(.fcst)
    stop("Can only plot one field at a time", call. = FALSE)
  }

  .field     <- dplyr::pull(.fcst, !!col)[[1]]
  field_info <- attr(.field, "info")

  field_info[["name"]] <- paste(.name, field_info[["name"]], sep = ": ")
  attr(.field, "info") <- field_info

  if (is.null(breaks)) {
    breaks <- pretty(.field, num_breaks)
  }

  plot_colours <- colorRampPalette(palette)(length(breaks) - 1)

  meteogrid::iview(.field, legend = TRUE, col = plot_colours, levels = breaks)

}

#' @export
plot_field.harp_fcst <- function(
  .fcst,
  .name,
  col,
  fcdate,
  lead_time,
  ...,
  palette    = viridis::viridis(255),
  num_breaks = 15,
  breaks     = NULL
) {

  if (missing(.name) && length(.fcst) == 1) {
    .name = names(.fcst)
  }

  if (!is.element(.name, names(.fcst))) {
    stop ("'", .name, "' not found in .fcst.", call. = FALSE)
  }

  plot_field(
    .fcst      = .fcst[[.name]],
    .name      = .name,
    col        = !!rlang::enquo(col),
    fcdate     = fcdate,
    lead_time  = lead_time,
    ...,
    palette    = palette,
    num_breaks = num_breaks,
    breaks     = breaks
  )

}
