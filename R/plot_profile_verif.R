#' Plot verification scores for vertical profiles
#'
#' This is a wrapper for \link{plot_point_verif} that ensures the coordinate
#' system is properly set up for plots of verification scores for vertical
#' profiles.
#'
#' @param verif_data The verification data.
#' @param score The score to be plotted. Must be unquoted.
#' @param y_axis The y axis for the plot. Must be unquoted. Be default this is
#'   p, for pressure on the y axis, otherwise it should be set to the column
#'   name in \code{verif_data} that you wish to use for the y axis.
#' @param ... Other arguments for \link{plot_point_verif}.
#' @param lead_time The leadtime (or leadtimes) to plot. By default all lead
#'   times in the data are plotted. If more than one lead time is to be plotted,
#'   they must be separated by setting \code{colour_by = leadtime} and / or
#'   \code{facet_by = vars(leadtime)}. See \link{plot_point_verif} for details
#'   of these arguments.
#' @param plot_num_cases Logical of whether to inlclude a panel for the number
#'   of cases. Note that the number of cases will not be plotted if
#'   \code{facet_by} is set.
#' @param num_cases_position The position of the number of cases panel relative
#'   to the score panel. For profile scores, this can only be "left", or
#'   "right".
#'
#' @return A \link[ggplot2]{ggplot} object of vertical profiles of verification
#'   scores.
#' @export
#'
#' @examples
plot_profile_verif <- function(
  verif_data,
  score,
  y_axis             = "p",
  lead_time          = NA,
  plot_num_cases     = TRUE,
  num_cases_position = c("right", "left"),
  ...
) {

  score  <- rlang::ensym(score)
  y_axis <- rlang::ensym(y_axis)

  if (all(!is.na(lead_time))) {
    # Ensure lead_time var isn't a column in the data
    .lt <- lead_time
    lead_time_col <- intersect(
      c("lead_time", "leadtime"),
      Reduce(union, lapply(verif_data, colnames))
    )
    verif_attributes <- attributes(verif_data)
    verif_data <- purrr::map(
      verif_data,
      dplyr::filter,
      .data[[lead_time_col]] %in% .lt
    )
    attributes(verif_data) <- verif_attributes
  }

  if (all(vapply(verif_data, nrow, integer(1)) < 1)) {
    cli::cli_abort(c(
      "No data to plot",
      "x" = "{.arg lead_time} == {lead_time} not found in {.arg verif_data}"
    ))
  }

  plot_point_verif(
    verif_data,
    score              = !! score,
    x_axis             = !! y_axis,
    plot_num_cases     = plot_num_cases,
    num_cases_position = match.arg(num_cases_position),
    flip_axes          = TRUE,
    ...
  )
}
