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
#'   \code{decomposed_brier_score}.
#' @param x_axis The x-axis for the plot. The default is leadtime, but could
#'   also be threshold. For some scores this is overrided.
#' @param y_axis The y-axis for the plot. The default is to take the same as the
#'   score input, and for most scores this is overrided.
#' @param colour_by The column to colour the plot lines or bars by. The default
#'   is mname, for the model name. Set to NULL for all lines / bars to have the
#'   same colour.
#' @param extend_y_to_zero Logical. Whether to extend the y-axis to include
#'   zero.
#' @param facet_by The column(s) to facet the plot by. Faceting is a term used
#'   for generating plot panels. The argument must be wrapped inside the
#'   \link[dplyr]{vars} function - e.g. \code{facet_by = vars(threshold)}.
#' @param num_facet_cols Number of columns in the faceted plot.
#' @param linetype_by The column to set the line types of the plot by.
#' @param line_width The width of lines to plot. The default is 1.1.
#' @param point_size The size of points to plot. Set to 0 for no points. The
#'   default is 2.
#' @param filter_by Filter the data before plotting. Must be wrapped inside the
#'   \link[dplyr]{vars} function. This can be useful for making a single plot
#'   where there are many groups. For example, for reliability there should be
#'   one plot for each lead time and threshold, so the data can be filtered with
#'   e.g. \code{filter_by = vars(leadtime == 12, threshold == 280)}.
#' @param plot_title Title for the plot. Set to "auto" to automatically generate
#'   the title. Set to "none" for no title. Anything else inside quotes will be
#'   used as the plot title.
#' @param plot_subtitle Subtitle for the plot. Set to "auto" to automatically
#'   generate the subtitle. Set to "none" for no subtitle. Anything else inside
#'   quotes will be used as the plot subtitle.
#' @param plot_caption Caption for the plot. Set to "auto" to automatically
#'   generate the caption Set to "none" for no caption. Anything else inside
#'   quotes will be used as the plot caption.
#' @param x_label Label for the x-axis. Set to "auto" to automatically generate
#'   the label from the data. Set to "none" for no label. Anything else inside
#'   quotes will be used as the x-axis label.
#' @param y_label Label for the y-axis. Set to "auto" to automatically generate
#'   the label from the data. Set to "none" for no label. Anything else inside
#'   quotes will be used as the y-axis label.
#' @param legend_position The position of legends ("none", "left", "right",
#'   "bottom", "top", or two-element numeric vector).
#' @param num_legend_rows The maximum number of rows in the legend.
#' @param log_scale_x Logical - whether to plot the x-axis on a log scale.
#' @param log_scale_y Logical - whether to plot the y-axis on a log scale.
#' @param ... Arguments to \link[ggplot2]{theme}
#'
#' @return A plot. Can be saved with \link[ggplot2]{ggsave}.
#' @export
#'
#' @examples
#' plot_point_verif(ens_verif_data, crps)
#' plot_point_verif(ens_verif_data, spread_skill)
#' plot_point_verif(det_verif_data, equitable_threat_score, facet_by = vars(threshold))
#' plot_point_verif(ens_verif_data, reliability, filter_by = vars(leadtime == 12, threshold == 290))

plot_point_verif <- function(
  verif_data,
  score,
  x_axis           = leadtime,
  y_axis           = rlang::enquo(score),
  colour_by        = mname,
  extend_y_to_zero = TRUE,
  facet_by         = NULL,
  num_facet_cols   = 3,
  linetype_by      = NULL,
  line_width       = 1.1,
  point_size       = 2,
  filter_by        = NULL,
  plot_title       = "auto",
  plot_subtitle    = "auto",
  plot_caption     = "auto",
  x_label          = "auto",
  y_label          = "auto",
  legend_position  = "bottom",
  num_legend_rows  = 3,
  log_scale_x      = FALSE,
  log_scale_y      = FALSE,
  ...
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
    stop ("colour_by badly formed - it must not be quoted.", call. = FALSE)
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
    derived_summary_scores <- c("spread_skill", "spread_skill_ratio")
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

  plot_geom <- "line"

  switch(score_name,
    "spread_skill" = {
      plot_data       <- tidyr::gather(plot_data, .data$rmse, .data$spread, key = "component", value = "spread ; skill")
      y_axis_name     <- "spread ; skill"
      y_axis_quo      <- rlang::sym(y_axis_name)
      linetype_by_quo <- rlang::quo(component)
      linetyping      <- TRUE
    },
    "spread_skill_ratio" = {
      plot_data       <- dplyr::mutate(plot_data, !! rlang::sym(score_name) := .data$spread / .data$rmse)
    },
    "rank_histogram" = {
      plot_data       <- tidyr::unnest(plot_data, !! score_quo)
      if (!faceting & !filtering) {
        plot_data     <- dplyr::group_by(plot_data, .data$mname, .data$rank) %>%
          dplyr::summarise(rank_count = sum(.data$rank_count))
      }
      plot_data       <- dplyr::mutate(plot_data, rank = formatC(.data$rank, width = 3, flag = "0"))
      x_axis_quo      <- rlang::quo(rank)
      y_axis_quo      <- rlang::quo(rank_count)
      plot_geom       <- "bar"
    },
    "reliability" = {
      plot_data       <- tidyr::unnest(plot_data, !! score_quo) %>%
        dplyr::mutate(no_skill = (.data$forecast_probability - .data$climatology) / 2 + .data$climatology)
      x_axis_quo      <- rlang::quo(forecast_probability)
      y_axis_quo      <- rlang::quo(observed_frequency)
    },
    "economic_value" = {
      plot_data       <- tidyr::unnest(plot_data, !! score_quo)
      x_axis_quo      <- rlang::quo(cl)
      y_axis_quo      <- rlang::quo(value)
    },
    "roc" = {
      plot_data       <- tidyr::unnest(plot_data, !! score_quo)
      x_axis_quo      <- rlang::quo(FAR)
      y_axis_quo      <- rlang::quo(HR)
    },
    "decomposed_brier_score" = {
      plot_data       <- tidyr::gather(
        plot_data,
        .data$brier_score_reliability,
        .data$brier_score_resolution,
        .data$brier_score_uncertainty,
        key   = "component",
        value = "contribution_to_brier_score"
      ) %>%
        dplyr::mutate(component = gsub("brier_score_", "", .data$component))
      y_axis_quo      <- rlang::quo(contribution_to_brier_score)
      linetype_by_quo <- rlang::quo(component)
      linetyping      <- TRUE
    }

  )

  ###########################################################################
  # SET UP THE BASIC PLOT SPACE
  ###########################################################################

  aspect1_score   <- score_name %in% c("reliability", "roc", "economic_value")
  plot_diagonal   <- score_name %in% c("reliability", "roc")
  plot_attributes <- score_name %in% c("reliability")

  # Labeling
  x_label <- switch(tolower(x_label),
    "auto" = totitle(gsub("_", " ", rlang::quo_name(x_axis_quo))),
    "none" = "",
    x_label
  )
  y_label <- switch(tolower(y_label),
    "auto" = totitle(gsub("_", " ", rlang::quo_name(y_axis_quo))),
    "none" = "",
    y_label
  )
  plot_title <- switch(tolower(plot_title),
    "auto" = paste(
      totitle(gsub("_", " ", score_name)),
      ":",
      paste0(attr(verif_data, "start_date"), "-", attr(verif_data, "end_date"))
    ),
    "none" = "",
    plot_title
  )
  plot_subtitle <- switch(tolower(plot_subtitle),
    "auto" = paste(attr(verif_data, "num_stations"), "stations"),
    "none" = "",
    plot_subtitle
  )
  plot_caption <- switch(tolower(plot_caption),
    "auto" = paste("Verification for", attr(verif_data, "parameter")),
    "none" = "",
    plot_caption
  )

  # Plot background
  if (tolower(colour_by_name == "none")) {
    gg <- ggplot2::ggplot(plot_data, ggplot2::aes(!! x_axis_quo, !! y_axis_quo))
  } else {
    gg <- ggplot2::ggplot(plot_data, ggplot2::aes(!! x_axis_quo, !! y_axis_quo, colour = !! colour_by_quo, fill = !! colour_by_quo))
  }

  gg <- gg + ggplot2::theme_bw()
  gg <- gg + ggplot2::xlab(x_label)
  gg <- gg + ggplot2::ylab(y_label)
  gg <- gg + ggplot2::theme(legend.position = legend_position, ...)
  gg <- gg + ggplot2::guides(
    colour   = ggplot2::guide_legend(title = NULL, nrow = num_legend_rows, byrow = TRUE),
    shape    = ggplot2::guide_legend(title = NULL, nrow = num_legend_rows, byrow = TRUE),
    fill     = ggplot2::guide_legend(title = NULL, nrow = num_legend_rows, byrow = TRUE),
    linetype = ggplot2::guide_legend(title = NULL)
  )
  if (nchar(plot_title) > 0)    gg <- gg + ggplot2::labs(title    = plot_title)
  if (nchar(plot_subtitle) > 0) gg <- gg + ggplot2::labs(subtitle = plot_subtitle)
  if (nchar(plot_caption) > 0)  gg <- gg + ggplot2::labs(caption  = plot_caption)

  # Axes
  if (log_scale_x) {
    gg <- gg + ggplot2::scale_x_log10()
  } else {
    if (rlang::quo_name(x_axis_quo) == "leadtime") {
      gg <- gg + ggplot2::scale_x_continuous(breaks = seq(0, 1800, 6))
    }
  }
  if (log_scale_y) {
    gg <- gg + ggplot2::scale_y_log10()
  }
  if (aspect1_score) {
    gg <- gg +
      ggplot2::scale_x_continuous(limits = c(0, 1)) +
      ggplot2::scale_y_continuous(limits = c(0, 1)) +
      ggplot2::coord_fixed(1, c(0, 1), c(0, 1), expand = FALSE)
  }

  y_values <- dplyr::pull(plot_data, !! y_axis_quo)
  range_y  <- range(y_values, na.rm = TRUE)
  min_y    <- range_y[1]
  max_y    <- range_y[2]
  if (extend_y_to_zero & plot_geom == "line" & !aspect1_score) {
    if (range_y[1] > 0) {
      min_y <- 0
      max_y <- ifelse(grepl("ratio", score_name), max(1, range_y[2]), range_y[2])
    }
    if (range_y[2] < 0) {
      min_y <- range_y[1]
      max_y <- 0
    }
  }
  if (!log_scale_y & !aspect1_score) {
    gg <- gg + ggplot2::coord_cartesian(ylim = c(min_y, max_y))
  }

  ###########################################################################
  # GEOMS
  ###########################################################################

  if (plot_geom == "line") {

    if (plot_diagonal) {
      gg <- gg + ggplot2::geom_abline(slope = 1, intercept = 0, colour = "grey80", size = line_width * 0.8)
    }

    if (plot_attributes) {
      gg <- gg +
        ggplot2::geom_smooth(
          ggplot2::aes(y = .data$climatology),
          method = "lm",
          se = FALSE,
          fullrange = TRUE,
          colour = "grey80",
          size = line_width * 0.8,
          lty = 2
        ) +
        ggplot2::geom_smooth(
          ggplot2::aes(y = .data$no_skill),
          method = "lm",
          se = FALSE,
          fullrange = TRUE,
          colour = "grey80",
          size = line_width * 0.8,
          lty = 3
        )
    }

    if (linetyping) {
      gg <- gg + ggplot2::geom_line(ggplot2::aes(lty = !! linetype_by_quo), size = line_width)
    } else {
      gg <- gg + ggplot2::geom_line(size = line_width)
    }

    if (point_size > 0) {
      gg <- gg + ggplot2::geom_point(size = point_size)
    }

  } else if (plot_geom == "bar") {

    gg <- gg + ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge(), colour = "grey30")

  } else {

    stop(paste("Unknown geom:", plot_geom), call. = FALSE)

  }

  if (faceting) {
    gg <- gg + ggplot2::facet_wrap(facet_by, ncol = num_facet_cols)
  }

  print(gg)

}

# Function to convert to title case
totitle <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
    {s <- substring(s, 2); if(strict) tolower(s) else s},
    sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}
