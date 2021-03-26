#' Plot a harp_fcst 2d field
#'
#' @param .fcst A harp_fcst list or harp_spatial_fcst data frame.
#' @param fcst_model The name of the model to plot. This selects the model from
#'   the harp_fcst list to plot. Can be omitted if there is only one model in
#'   the harp_fcst list. For plotting directly from a harp_spatial_fcst data
#'   frame, as returned by \code{\link{read_grid}}, this is the name to be used
#'   in the plot title.
#' @param plot_col The column of data to plot from. Should be unquoted, or if a
#'   variable, wrapped in double curly brackets: \{\{\}\}.
#' @param fcdate The forecast date to plot in "YYYYMMDDhh" or similar format.
#'   Can be omitted if there is only one date in the data.
#' @param lead_time The lead time to plot. Can be omitted if there is only one
#'   lead time in the data.
#' @param filter_by Expressions that return a logical value wrapped inside the
#'   \code{vars} function for filtering the data prior to plotting. Only a
#'   single row should be left in the data frame for \code{plot_field} to work.
#' @param palette Colour palette to use. This should be a vector of colours.
#' @param num_breaks Number of colour breaks to use in the plot.
#' @param breaks The values to use for colour breaks. If not NULL, breaks has
#'   priority over num_breaks.
#'
#' @return A plot
#' @export
#'
#' @examples
plot_field <- function(
  .fcst,
  fcst_model,
  plot_col,
  fcdate,
  lead_time,
  filter_by  = NULL,
  palette    = viridis::viridis(255),
  num_breaks = 15,
  breaks     = NULL,
  legend     = TRUE,
  title      = "auto"
) {
  UseMethod("plot_field")
}

#' @export
plot_field.harp_spatial_fcst <- function(
  .fcst,
  fcst_model,
  plot_col,
  fcdate,
  lead_time,
  filter_by  = NULL,
  palette    = viridis::viridis(255),
  num_breaks = 15,
  breaks     = NULL,
  legend     = TRUE,
  title      = "auto"
) {

  col <- rlang::enquo(plot_col)

  if (rlang::quo_is_null(col) || rlang::quo_is_missing(col)) {
    stop("plot_col must be supplied as an argument.")
  }

  if (missing(fcdate) && length(unique(.fcst[["fcdate"]])) == 1) {
    fcdate <- harpIO::unixtime_to_str_datetime(as.numeric(unique(.fcst[["fcdate"]])), harpIO::YMDhms)
  }

  if (missing(lead_time) && length(unique(.fcst[["lead_time"]])) == 1) {
    lead_time <- unique(.fcst[["lead_time"]])
  }

  if (missing(fcst_model)) {
    fcst_model <- ""
  }

  if (!all(sapply(dplyr::pull(.fcst, !!col), meteogrid::is.geofield))) {
    stop("Selected col: '", col, "' does not contain geofield objects.", call. = FALSE)
  }

  fcdate_filter    <- harpIO::str_datetime_to_datetime(fcdate)
  lead_time_filter <- lead_time

  filter_by_err  <- paste(
    "filter_by must be wrapped in vars and unquoted,\n",
    "e.g. filter_by = vars(members == 0)."
  )
  filter_by_null <- try(is.null(filter_by), silent = TRUE)

  if (inherits(filter_by_null, "try-error")) {
    stop(filter_by_err, call. = FALSE)
  } else {
    if (filter_by_null) {
      filtering <- FALSE
    } else {
      if (inherits(filter_by, "quosures")) {
        filtering <- TRUE
      } else {
        stop(filter_by_err, call. = FALSE)
      }
    }
  }


  .fcst <- dplyr::filter(
    .fcst,
    .data[["fcdate"]]    == fcdate_filter,
    .data[["lead_time"]] == lead_time_filter
  )

  if (filtering) {
    .fcst <- dplyr::filter(.fcst, !!!filter_by)
  }

  if (nrow(.fcst) < 1) {
    stop("Nothing to plot", call. = FALSE)
  }

  if (nrow(.fcst) > 1) {
    message("Filtered data to plot:")
    print(.fcst)
    stop("Can only plot one field at a time. Use filter_by to filter to a single row.", call. = FALSE)
  }

  .field     <- dplyr::pull(.fcst, !!col)[[1]]
  field_info <- attr(.field, "info")

  if (is.element("parameter", colnames(.fcst))) {
    field_info[["name"]] <- dplyr::pull(.fcst, .data[["parameter"]])
    if (is.element("units", colnames(.fcst))) {
      field_info[["name"]] <- paste0(field_info[["name"]], " [", dplyr::pull(.fcst, .data[["units"]]), "]")
    }
  }

  if (is.element("lead_time", colnames(.fcst))) {
    lt <- dplyr::pull(.fcst, .data[["lead_time"]])
    if (is.numeric(lt)) {
      lt <- paste0(lt, "h")
    }
    field_info[["time"]][["leadtime"]] <- lt
  }

  field_info[["name"]] <- paste(fcst_model, field_info[["name"]], sep = ": ")
  attr(.field, "info") <- field_info

  plot_field(.field, palette, num_breaks, breaks, legend, title)

}

#' @export
plot_field.harp_fcst <- function(
  .fcst,
  fcst_model,
  plot_col,
  fcdate,
  lead_time,
  filter_by  = NULL,
  palette    = viridis::viridis(255),
  num_breaks = 15,
  breaks     = NULL,
  legend     = TRUE,
  title      = "auto"
) {

  if (is.null(fcst_model) && length(.fcst) == 1) {
    fcst_model = names(.fcst)
  }

  if (length(fcst_model) > 1) {
    stop("Only one fcst_model can be specified.", call. = FALSE)
  }

  if (!is.element(fcst_model, names(.fcst))) {
    stop ("'", fcst_model, "' not found in .fcst.", call. = FALSE)
  }

  plot_field(
    .fcst      = .fcst[[fcst_model]],
    fcst_model = fcst_model,
    plot_col   = !!rlang::enquo(plot_col),
    fcdate     = fcdate,
    lead_time  = lead_time,
    filter_by  = filter_by,
    palette    = palette,
    num_breaks = num_breaks,
    breaks     = breaks,
    legend     = legend,
    title      = title
  )

}

#' @export
plot_field.geofield <- function(
  .fcst,
  palette    = viridis::viridis(255),
  num_breaks = 15,
  breaks     = NULL,
  legend     = TRUE,
  title      = "auto",
  ...
) {

  if (is.null(breaks)) {
    breaks <- pretty(.fcst, num_breaks)
  }

  plot_colours <- colorRampPalette(palette)(length(breaks) - 1)

  if (title == "auto") {
    title <- paste(
      attr(.fcst,"info")$name,
      "\n",
      format(attr(.fcst, "info")$time$basedate, "%H:%M %d %b %Y"),
      "+",
      paste0(
        attr(.fcst,"info")$time$leadtime, attr(.fcst,"info")$time$stepUnit
      )
    )
  }

  meteogrid::iview(
    .fcst,
    legend = legend,
    col    = plot_colours,
    levels = breaks,
    title  = title
  )

}
