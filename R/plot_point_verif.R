#' Plot verification scores
#'
#' \code{plot_point_verif} is used to plot verification scores computed by
#' functions from the harpPoint package. The function uses non standard
#' evaluation (NSE) meaning that none of the arguments should be quoted. For
#' arguments that can take more than one value, (\code{facet_by} and
#' \code{filter_by}), the arguments should be wrapped inside the
#' \link[dplyr]{vars} function.
#'
#' @param verif_data Output from \link[harpPoint]{ens_verif},
#'   \link[harpPoint]{det_verif}, or a harpPoint verification function for
#'   individual scores.
#' @param score The score to plot. Should be the name of one of the columns in
#'   the verification tables or the name of a dervived score, such as
#'   \code{spread_skill}, \code{spread_skill_ratio}, or
#'   \code{brier_score_decomposition}.
#' @param x_axis The x-axis for the plot. The default is leadtime, but could
#'   also be threshold. For some scores this is overrided.
#' @param y_axis The y-axis for the plot. The default is to take the same as the
#'   score input, and for most scores this is overrided.
#' @param colour_by The column to colour the plot lines or bars by. The default
#'   is mname, for the model name. Set to NULL for all lines / bars to have the
#'   same colour.
#' @param facet_by The column(s) to facet the plot by. Faceting is a term used
#'   for generating plot panels. The argument must be wrapped inside the
#'   \link[dplyr]{vars} function - e.g. \code{facet_by = vars(threshold)}.
#' @param linetype_by The column to set the line types of the plot by.
#' @param filter_by Filter the data before plotting. Must be wrapped inside the
#'   \link[dplyr]{vars} function. This can be useful for making a single plot
#'   where there are many groups. For example, for reliability there should be
#'   one plot for each lead time and threshold, so the data can be filtered with
#'   e.g. \code{filter_by = vars(leadtime == 12, threshold == 280)}.
#'
#' @return A plot. Can be saved with \link[ggplot2]{ggsave}.
#' @export
#'
#' @examples
#' plot_point_verif(ens_verif_data, crps)
#' plot_point_verif(ens_verif_data, spread_skill)
#' plot_point_verif(det_verif_data, equitable_threat_score, facet_by = vars(threshold))
#' plot_point_verif(ens_verif_data, reliability, filter_by = vars(leadtime == 12, threshold == 280))

plot_point_verif <- function(
  verif_data,
  score,
  x_axis      = leadtime,
  y_axis      = rlang::enquo(score),
  colour_by   = mname,
  facet_by    = NULL,
  linetype_by = NULL,
  filter_by   = NULL
) {

  ###########################################################################
  # TIDYEVAL CHECKS
  ###########################################################################

  # Score to plot
  score_quo_exists <- try(exists(score, envir = sys.frame()), silent = TRUE)
  if (inherits(score_quo_exists, "try-error")) {
    score_quo  <- rlang::enquo(score)
    score_name <- rlang::quo_name(score_quo)
  } else {
    stop ("score badly formed - it must not be quoted.", call. = FALSE)
  }

  # x axis - the default is lead time
  x_axis_exists <- try(exists(x_axis, envir = sys.frame()), silent = TRUE)
  if (inherits(x_axis_exists, "try-error")) {
    x_axis_quo  <- rlang::enquo(x_axis)
    x_axis_name <- rlang::quo_name(x_axis_quo)
  } else {
    stop ("x_axis badly formed - it must not be quoted.", call. = FALSE)
  }

  # y axis - the defualt is the score, but depending on the score this can change later
  is_y_axis_quo <- try(rlang::is_quosure(y_axis), silent = TRUE)
  if (inherits(is_y_axis_quo, "try-error")) {
    y_axis_quo <- rlang::enquo(y_axis)
  } else if (is_y_axis_quo) {
    y_axis_quo <- y_axis
  } else {
    stop ("y_axis badly formed - it must not be quoted.", call. = FALSE)
  }
  y_axis_name <- rlang::quo_name(y_axis_quo)

  # the column to colour lines by - the default is mname
  colour_exists <- try(exists(colour_by, envir = sys.frame()), silent = TRUE)
  if (inherits(colour_exists, "try-error")) {
    colour_by_quo  <- rlang::enquo(colour_by)
    colour_by_name <- rlang::quo_name(colour_by_quo)
  } else {
    stop ("y_axis badly formed - it must not be quoted.", call. = FALSE)
  }

  # the column(s) to facet the plot by. Default is null.
  facet_by_err  <- "facet_by must be wrapped in vars and unquoted, e.g. facet_by = vars(a, b, c)."
  facet_by_null <- try(is.null(facet_by), silent = TRUE)
  if (inherits(facet_by_null, "try-error")) {
    stop(facet_by_err, call. = FALSE)
  } else {
    if (facet_by_null) {
      faceting <- FALSE
    } else {
      if (inherits(facet_by, "quosures")) {
        faceting <- TRUE
      } else {
        stop(facet_by_err, call. = FALSE)
      }
    }
  }

  # the column(s) to filter data for before plotting
  filter_by_err  <- paste("filter_by must be wrapped in vars and unquoted,\n",
    "e.g. filter_by = vars(leadtime == 12, threshold == 280).")
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

  # The column to control the linetype. Default is null.
  linetype_by_null <- try(is.null(linetype_by), silent = TRUE)
  if (inherits(linetype_by_null, "try-error")) {
    linetyping       <- TRUE
    linetype_by_quo  <- rlang::enquo(linetype_by)
    linetype_by_name <- rlang::quo_name(linetype_by_quo)
  } else if (linetype_by_null) {
    linetyping       <- FALSE
  } else {
    stop ("linetype_by badly formed - it must not be quoted.", call. = FALSE)
  }

  ###########################################################################
  # CHECK IF SCORE IS VALID
  ###########################################################################

  score_tables       <- names(verif_data)
  if (any(grepl("det_", score_tables))) {
    fcst_type <- "det"
  } else if (any(grepl("ens_", score_tables))) {
    fcst_type <- "ens"
  } else {
    stop("Input does not look like a harpPoint verification.", call. = FALSE)
  }

  summary_table      <- verif_data[[score_tables[grep("summary", score_tables)]]]
  thresh_table       <- verif_data[[score_tables[grep("threshold", score_tables)]]]

  summary_scores     <- names(summary_table)
  thresh_scores      <- names(thresh_table)
  if (fcst_type == "ens") {
    derived_summary_scores <- c("spread_skill", "spread_skill_ratio", "decomposed_brier_score")
    derived_thresh_scores  <- c("decomposed_brier_score")
  } else {
    derived_summary_scores <- ""
    derived_thresh_scores  <- ""
  }

  if (is.element(score_name, c(summary_scores, derived_summary_scores))) {
    plot_data <- summary_table
  } else if (is.element(score_name, c(thresh_scores, derived_thresh_scores))) {
    plot_data <- thresh_table
  } else {
    stop("score: ", score_name, " not found in data. Note that arguments are case sensitive.", call. = FALSE)
  }


  ###########################################################################
  # PREP DATA FOR PLOTTING
  ###########################################################################

  if (filtering) {
    plot_data <- dplyr::filter(plot_data, !!! filter_by)
  }

  switch(score_name,
    "spread_skill" = {
      plot_data       <- tidyr::gather(plot_data, .data$rmse, .data$spread, key = "component", value = "spread ; skill")
      y_axis_name     <- "spread ; skill"
      y_axis_quo      <- rlang::sym(y_axis_name)
      linetype_by_quo <- rlang::quo(component)
      linetyping      <- TRUE
    },
    "reliability" = {
      plot_data       <- tidyr::unnest(plot_data, !! score_quo)
      x_axis_quo      <- rlang::quo(forecast_probability)
      y_axis_quo      <- rlang::quo(observed_frequency)
    }
  )
  print(score_name)
  print(x_axis_name)
  print(y_axis_name)
  print(colour_by_name)
  print(faceting)
  print(linetyping)
#  if (linetyping) print(linetype_by_name)

  gg <- ggplot2::ggplot(plot_data, ggplot2::aes(!! x_axis_quo, !! y_axis_quo, colour = !! colour_by_quo))

  if (linetyping) {
    gg <- gg + ggplot2::geom_line(ggplot2::aes(lty = !! linetype_by_quo))
  } else {
    gg <- gg + ggplot2::geom_line()
  }

  if (faceting) {
    gg <- gg + ggplot2::facet_wrap(facet_by)
  }

  print(gg)

}
