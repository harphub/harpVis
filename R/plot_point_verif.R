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
#' @param verif_type The type of verification to plot for ensemble verification
#'   data. The default is "ens", but set to "det" to plot verification scores
#'   for members. If set to "det", you should also set colour_by = member.
#' @param x_axis The x-axis for the plot. The default is lead_time, but could
#'   also be threshold. For some scores this is overrided. Note that leadtime
#'   will be treated exactly the same as lead_time for compatibility with older
#'   versions.
#' @param y_axis The y-axis for the plot. The default is to take the same as the
#'   score input, and for most scores this is overrided.
#' @param rank_is_relative Logical. If TRUE rank histograms are plotted with the
#'   relative rank (between 0 and 1) on the x-axis. The default is FALSE.
#' @param rank_hist_type For rank histograms, the plot can be done as a bar
#'   chart, lollipop chart or a line chart.
#' @param colour_by The column to colour the plot lines or bars by. The default
#'   is mname, for the model name. Set to NULL for all lines / bars to have the
#'   same colour.
#' @param extend_y_to_zero Logical. Whether to extend the y-axis to include
#'   zero.
#' @param facet_by The column(s) to facet the plot by. Faceting is a term used
#'   for generating plot panels. The argument must be wrapped inside the
#'   \link[dplyr]{vars} function - e.g. \code{facet_by = vars(threshold)}.
#' @param num_facet_cols Number of columns in the faceted plot.
#' @param facet_scales Should facet scales be fixed ("fixed", the default), free
#'   ("free"), or free in one dimension ("free_x", "free_y")?
#' @param linetype_by The column to set the line types of the plot by.
#' @param line_width The width of lines to plot. The default is 1.1.
#' @param point_size The size of points to plot. Set to 0 for no points. The
#'   default is 2.
#' @param filter_by Filter the data before plotting. Must be wrapped inside the
#'   \link[dplyr]{vars} function. This can be useful for making a single plot
#'   where there are many groups. For example, for reliability there should be
#'   one plot for each lead time and threshold, so the data can be filtered with
#'   e.g. \code{filter_by = vars(lead_time == 12, threshold == 280)}.
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
#' @param colour_theme The colour theme for the plot - can be any ggplot2 theme
#'   (see \link[ggplo2]{theme_grey}), or "theme_harp_grey",
#'   "theme_harp_midnight", or "theme_harp_black".
#' @param ... Arguments to \link[ggplot2]{aes} e.g. group = ...
#' @param colour_table A data frame with column names equal to the value of
#'   \code{colour_by} and "colour". The colour column should contain colour
#'   names or hex codes. There should be one row for each value in the
#'   \code{colour_by} column. If set to NULL, the default colour table is used.
#' @param plot_num_cases Logical of whether to inlcude the number of cases as a
#'   panel in the plot. Only currently works for summary scores, and if
#'   \code{facet_by} is set, the number of cases panel is not drawn since it
#'   will clutter the plot.
#' @param extend_num_cases_to_zero Logical of whether to extend the axis for the
#'   number of cases to zero. The default behaviour (FALSE) is to have the axis
#'   limits set to the minimum and maximum number of cases.
#' @param num_cases_position The position of the number of cases panel relative
#'   to the score panel. Can be "below" (the default), "above", "left", or
#'   "right". Typically only "below" and "above" will work unless plotting
#'   vertical profile scores with \link{plot_profile_verif}, where only "left"
#'   and "right" can be chosen.
#' @param facet_labeller The function used to label the title strip. Typically
#'   this will always be "label_value", but if the column used for
#'   \code{facet_by} contains plotmath expressions, "label_parsed" should be
#'   used. See \link[ggplot2]{labellers} for more information.
#' @param flip_axes Logical of whether to swap the x and y axes. This is
#'   typically used when this function is called by \link{plot_profile_verif}.
#' @param base_size base font size.
#' @param base_family base font family.
#' @param base_line_size base size for line elements.
#' @param base_rect_size base size for rect elements.
#'
#' @return A plot. Can be saved with \link[ggplot2]{ggsave}.
#' @import ggplot2
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
  verif_type               = c("ens", "det"),
  x_axis                   = lead_time,
  y_axis                   = rlang::enquo(score),
  rank_is_relative         = FALSE,
  rank_hist_type           = c("bar", "lollipop", "line"),
  colour_by                = fcst_model,
  colour_table             = NULL,
  extend_y_to_zero         = TRUE,
  plot_num_cases           = TRUE,
  extend_num_cases_to_zero = FALSE,
  num_cases_position       = c("below", "right", "above", "left"),
  facet_by                 = NULL,
  num_facet_cols           = 3,
  facet_scales             = "fixed",
  facet_labeller           = "label_value",
  linetype_by              = NULL,
  line_width               = 1.1,
  point_size               = 2,
  filter_by                = NULL,
  plot_title               = "auto",
  plot_subtitle            = "auto",
  plot_caption             = "auto",
  x_label                  = "auto",
  y_label                  = "auto",
  legend_position          = "bottom",
  num_legend_rows          = 1,
  log_scale_x              = FALSE,
  log_scale_y              = FALSE,
  flip_axes                = FALSE,
  colour_theme             = "bw",
  base_size                = 11,
  base_family              = "",
  base_line_size           = base_size / 22,
  base_rect_size           = base_size / 22,
  ...
) {

  ###########################################################################
  # TIDYEVAL CHECKS
  ###########################################################################

  # Score to plot
  y_axis_done <- FALSE
  score_quo   <- rlang::enquo(score)
  score_expr  <- rlang::quo_get_expr(score_quo)
  if (is.character(score_expr)) {
    score_quo  <- rlang::sym(score)
    if (rlang::is_quosure(y_axis)) {
      y_axis_name <- rlang::quo_name(y_axis)
      y_axis_quo  <- rlang::sym(y_axis_name)
      y_axis_done <- TRUE
    }
  }
  score_name <- rlang::quo_name(score_quo)

  # x axis - the default is lead time
  x_axis_quo  <- rlang::enquo(x_axis)
  x_axis_expr <- rlang::quo_get_expr(x_axis_quo)
  if (is.character(x_axis_expr)) {
    x_axis_quo <- rlang::sym(x_axis)
  }
  x_axis_name <- rlang::quo_name(x_axis_quo)

  # y axis - the defualt is the score, but depending on the score this can change later
  if (!y_axis_done) {
    if (rlang::is_quosure(y_axis)) {
      y_axis_quo  <- y_axis
    } else {
      y_axis_quo  <- rlang::enquo(y_axis)
    }
    y_axis_expr <- rlang::quo_get_expr(y_axis_quo)
    if (is.character(y_axis_expr)) {
      y_axis_quo <- rlang::sym(y_axis)
    }
    y_axis_name <- rlang::quo_name(y_axis_quo)
  }

  # the column to colour lines by - the default is mname
  colour_by_quo  <- rlang::enquo(colour_by)
  colour_by_expr <- rlang::quo_get_expr(colour_by_quo)
  if (is.character(colour_by_expr)) {
    colour_by_quo <- rlang::sym(colour_by)
  }
  colour_by_name <- rlang::quo_name(colour_by_quo)

  # the column(s) to facet the plot by. Default is null.
  facet_by_err  <- "facet_by must be wrapped in vars and unquoted, e.g. facet_by = vars(a, b, c)."
  facet_by_null <- try(is.null(facet_by), silent = TRUE)
  facet_vars    <- ""
  if (inherits(facet_by_null, "try-error")) {
    stop(facet_by_err, call. = FALSE)
  } else {
    if (facet_by_null) {
      faceting <- FALSE
    } else {
      if (inherits(facet_by, "quosures")) {
        faceting <- TRUE
        if (plot_num_cases) {
          warning(
            "plot_num_cases = TRUE cannot be used with facet_by. ",
            "plot_num_cases set to FALSE.",
            call. = FALSE
          )
          plot_num_cases <- FALSE
        }
        facet_vars <- purrr::map_chr(rlang::eval_tidy(facet_by), rlang::quo_name)
      } else {
        stop(facet_by_err, call. = FALSE)
      }
    }
  }

  # the column(s) to filter data for before plotting
  filter_by_err  <- paste("filter_by must be wrapped in vars and unquoted,\n",
    "e.g. filter_by = vars(lead_time == 12, threshold == 280).")
  filter_by_null <- try(is.null(filter_by), silent = TRUE)
  filter_vars    <- ""
  if (inherits(filter_by_null, "try-error")) {
    stop(filter_by_err, call. = FALSE)
  } else {
    if (filter_by_null) {
      filtering <- FALSE
    } else {
      if (inherits(filter_by, "quosures")) {
        filtering <- TRUE
        filter_expr <- purrr::map_chr(
          rlang::eval_tidy(filter_by),
          rlang::quo_name
        )
        filter_vars <- expand.grid(
          union(
            c("lead_time", "leadtime", "mname", "fcst_model"),
            unique(unlist(lapply(verif_data, colnames)))
          ),
          filter_expr, stringsAsFactors = FALSE
        ) %>%
          dplyr::filter(mapply(grepl, .data[["Var1"]], .data[["Var2"]])) %>%
          dplyr::pull(.data[["Var1"]])
      } else {
        stop(filter_by_err, call. = FALSE)
      }
    }
  }

  # The column to control the linetype. Default is null.
  linetype_by_quo  <- rlang::enquo(linetype_by)
  linetype_by_expr <- rlang::quo_get_expr(linetype_by_quo)
  if (is.null(linetype_by_expr)) {
    linetyping <- FALSE
  } else if (is.character(linetype_by_expr)) {
    linetype_by_quo  <- rlang::sym(linetype_by)
    linetype_by_name <- rlang::quo_name(linetype_by_quo)
    linetyping       <- TRUE
  } else {
    linetype_by_name <- rlang::quo_name(linetype_by_quo)
    linetyping       <- TRUE
  }

  ###########################################################################
  # CHECK IF SCORE IS VALID
  ###########################################################################

  verif_type <- match.arg(verif_type)

  verif_attributes <- attributes(verif_data)

  verif_data <- purrr::map(verif_data, ~ if(!is.null(.x)) dplyr::ungroup(.x))

  tables_with_data <- names(verif_data)[purrr::map_lgl(verif_data, ~!is.null(.x))]
  verif_data <- purrr::map_at(
    verif_data,
    tables_with_data,
    dplyr::mutate_if,
    is.numeric,
    inf_to_na
  )
  attributes(verif_data) <- verif_attributes

  score_tables <- names(verif_data)

  if (all(grepl("det_", score_tables))) {
    fcst_type <- "det"
  } else if (any(grepl("ens_", score_tables))) {
    fcst_type <- ifelse(verif_type == "det", "det", "ens")
  } else {
    stop("Input does not look like a harpPoint verification.", call. = FALSE)
  }

  summary_table      <- purrr::pluck(verif_data, paste0(fcst_type, "_summary_scores"))
  thresh_table       <- purrr::pluck(verif_data, paste0(fcst_type, "_threshold_scores"))

  summary_scores     <- names(summary_table)
  thresh_scores      <- names(thresh_table)
  if (fcst_type == "ens") {
    derived_summary_scores <- c(
      "spread_skill", "spread_skill_ratio", "spread_skill_with_dropped",
      "spread_skill_dropped_only", "spread_skill_ratio_with_dropped",
      "spread_skill_ratio_dropped_only", "normalized_rank_histogram"
    )
    derived_thresh_scores  <- c("brier_score_decomposition", "sharpness")
  } else {
    derived_summary_scores <- ""
    derived_thresh_scores  <- ""
  }

  if (is.element(score_name, c(summary_scores, derived_summary_scores))) {
    plot_data  <- summary_table
    score_type <- "summary"
  } else if (is.element(score_name, c(thresh_scores, derived_thresh_scores))) {
    plot_data  <- thresh_table
    score_type <- "thresh"
  } else {
    stop("score: ", score_name, " not found in data. Note that arguments are case sensitive.", call. = FALSE)
  }

  if (nrow(plot_data) == 0) {
    cli::cli_warn("No data to plot.")
    return()
  }

  ### Compatibility between old "leadtime" and new "lead_time" column names
  if (is.element("leadtime", colnames(plot_data))) {
    plot_data <- dplyr::rename(plot_data, lead_time = .data[["leadtime"]])
  }
  if (x_axis_name == "leadtime") {
    x_axis_name <- "lead_time"
    x_axis_quo  <- rlang::sym("lead_time")
  }

  ### Compatibility between old "mname" and new "fcst_model" column names
  if (is.element("mname", colnames(plot_data))) {
    plot_data <- dplyr::rename(plot_data, fcst_model = .data[["mname"]])
  }
  if (colour_by_name == "mname") {
    colour_by_name <- "fcst_model"
    colour_by_quo  <- rlang::sym("fcst_model")
  }

  ###########################################################################
  # PREP DATA FOR PLOTTING
  ###########################################################################

  plot_data <- filter_for_x(plot_data, x_axis_name)


  if (filtering) {
    has_leadtime <- FALSE
    has_mname <- FALSE
    if (is.element("leadtime", filter_vars)) {
      plot_data <- dplyr::rename(plot_data, leadtime = .data[["lead_time"]])
      has_leadtime <- TRUE
    }
    if (is.element("mname", filter_vars)) {
      plot_data <- dplyr::rename(plot_data, mname = .data[["fcst_model"]])
      has_mname <- TRUE
    }
    plot_data <- dplyr::filter(plot_data, !!! filter_by)
    if (has_leadtime) {
      plot_data <- dplyr::rename(plot_data, lead_time = .data[["leadtime"]])
    }
    if (has_mname) {
      plot_data <- dplyr::rename(plot_data, fcst_model = .data[["mname"]])
    }
  }

  if (nrow(plot_data) < 1) {
    cli::cli_warn("No data to plot after filtering.")
    return()
  }

  plot_geom <- "line"

  if (grepl("rank_histogram", score_name) && nrow(plot_data) > 0 && is.element("rank_histogram", colnames(plot_data))) {
    plot_num_cases <- FALSE
    add_rel_rank <- function(rank_hist) {
      if (!is.element("relative_rank", colnames(rank_hist))) {
        rank_hist <- dplyr::mutate(
          rank_hist,
          relative_rank = (.data$rank - min(.data$rank)) / (max(.data$rank) - min(.data$rank))
        )
      }
      rank_hist
    }
    plot_data <- dplyr::mutate(
      plot_data,
      rank_histogram = lapply(.data$rank_histogram, add_rel_rank)
    )
  }

  has_lead <- function(x) {
    length(intersect(c("leadtime", "lead_time"), x)) > 9
  }

  switch(

    score_name,

    "num_cases" = {
      plot_num_cases <- FALSE
    },

    "spread_skill" = {
      plot_data        <- tidyr::gather(plot_data, .data$rmse, .data$spread, key = "component", value = "spread ; skill")
      y_axis_name      <- "spread ; skill"
      y_axis_quo       <- rlang::sym(y_axis_name)
      linetype_by_quo  <- rlang::quo(component)
      linetype_by_name <- rlang::quo_name(linetype_by_quo)
      linetyping       <- TRUE
    },

    "spread_skill_with_dropped" = {
      plot_data        <- dplyr::rename(plot_data, spread_dropped_members = .data$dropped_members_spread)
      plot_data        <- tidyr::gather(
        plot_data, .data$rmse, .data$spread, .data$spread_dropped_members, key = "component", value = "spread ; skill"
      )
      y_axis_name      <- "spread ; skill"
      y_axis_quo       <- rlang::sym(y_axis_name)
      linetype_by_quo  <- rlang::quo(component)
      linetype_by_name <- rlang::quo_name(linetype_by_quo)
      linetyping       <- TRUE
    },

    "spread_skill_dropped_only" = {
      plot_data        <- dplyr::rename(plot_data, spread_dropped_members = .data$dropped_members_spread)
      plot_data        <- tidyr::gather(
        plot_data, .data$rmse, .data$spread_dropped_members, key = "component", value = "spread ; skill"
      )
      y_axis_name      <- "spread ; skill"
      y_axis_quo       <- rlang::sym(y_axis_name)
      linetype_by_quo  <- rlang::quo(component)
      linetype_by_name <- rlang::quo_name(linetype_by_quo)
      linetyping       <- TRUE
    },

    "spread_skill_ratio" = {
      if (!is.element("spread_skill_ratio", colnames(plot_data))) {
        plot_data <- dplyr::mutate(plot_data, !! rlang::sym(score_name) := .data$spread / .data$rmse)
      }
    },

    "spread_skill_ratio_with_dropped" = {
      if (!is.element("dropped_members_spread_skill_ratio", colnames(plot_data))) {
        plot_data <- dplyr::mutate(plot_data, spread_skill_ratio_with_dropped = .data$dropped_members_spread / .data$rmse)
      } else {
        plot_data <- dplyr::rename(plot_data, spread_skill_ratio_with_dropped = .data$dropped_members_spread_skill_ratio)
      }
      if (!is.element("spread_skill_ratio", colnames(plot_data))) {
        plot_data <- dplyr::mutate(plot_data, spread_skill_ratio = .data$dropped_members_spread / .data$rmse)
      }
      plot_data        <- tidyr::gather(
        plot_data, .data$spread_skill_ratio, .data$spread_skill_ratio_with_dropped,
        key = "component", value = "Spread Skill Ratio"
      )
      y_axis_name      <- "Spread Skill Ratio"
      y_axis_quo       <- rlang::sym(y_axis_name)
      linetype_by_quo  <- rlang::quo(component)
      linetype_by_name <- rlang::quo_name(linetype_by_quo)
      linetyping       <- TRUE
    },

    "spread_skill_ratio_dropped_only" = {
      if (!is.element("dropped_members_spread_skill_ratio", colnames(plot_data))) {
        plot_data <- dplyr::mutate(plot_data, !! rlang::sym(score_name) := .data$dropped_members_spread / .data$rmse)
      } else {
        plot_data[["spread_skill_ratio_dropped_only"]] <- plot_data[["dropped_members_spread_skill_ratio"]]
      }
    },

    "rank_histogram" = {
      plot_data        <- tidyr::unnest(plot_data, !! score_quo)
      if (!has_lead(facet_vars)) {#& !has_lead(filter_vars)) {
        grouping_vars  <- rlang::syms(gsub(
          "leadtime", "lead_time",
          c(colour_by_name, facet_vars[nchar(facet_vars) > 0])
        ))
        plot_data      <- dplyr::group_by(plot_data, !!!grouping_vars, .data$rank, .data$relative_rank) %>%
          dplyr::summarise(rank_count = sum(.data$rank_count)) %>%
          dplyr::ungroup()
      }
      #plot_data        <- dplyr::mutate(plot_data, rank = formatC(.data$rank, width = 3, flag = "0"))
      x_axis_quo       <- rlang::quo(rank)
      if (rank_is_relative) x_axis_quo <- rlang::quo(relative_rank)
      y_axis_quo       <- rlang::quo(rank_count)
      plot_geom        <- match.arg(rank_hist_type)
    },

    "normalized_rank_histogram" = {
      data_column      <- rlang::sym("rank_histogram")
      plot_data        <- tidyr::unnest(plot_data, !! data_column)
      if (!has_lead(facet_vars)) {#} & !has_lead(filter_vars)) {
        grouping_vars  <- rlang::syms(gsub(
          "leadtime", "lead_time",
          c(colour_by_name, facet_vars[nchar(facet_vars) > 0])
        ))
        plot_data      <- dplyr::group_by(plot_data, !!!grouping_vars, .data$rank, .data$relative_rank) %>%
          dplyr::summarise(rank_count = sum(.data$rank_count))
      } else {
        grouping_vars  <- rlang::syms(union("lead_time", c(colour_by_name, facet_vars[nchar(facet_vars) > 0])))
      }
      plot_data        <- plot_data %>%
        dplyr::group_by(!!!grouping_vars) %>%
        dplyr::mutate(
          mean_count           = mean(.data$rank_count),
          normalized_frequency = .data$rank_count / .data$mean_count
        )

      #plot_data        <- dplyr::mutate(plot_data, rank = formatC(.data$rank, width = 3, flag = "0"))
      x_axis_quo       <- rlang::quo(rank)
      if (rank_is_relative) x_axis_quo <- rlang::quo(relative_rank)
      x_axis_name      <- rlang::as_name(x_axis_quo)
      y_axis_quo       <- rlang::quo(normalized_frequency)
      plot_geom        <- match.arg(rank_hist_type)
    },

    "reliability" = {
      plot_data        <- tidyr::unnest(plot_data, !! score_quo) %>%
        dplyr::mutate(no_skill = (.data$forecast_probability - .data$bss_ref_climatology) / 2 + .data$bss_ref_climatology)
      x_axis_quo       <- rlang::quo(forecast_probability)
      x_axis_name      <- "forecast_probability"
      y_axis_quo       <- rlang::quo(observed_frequency)
      y_axis_name      <- "observed_frequency"
    },

    "sharpness" = {
      data_column      <- rlang::sym("reliability")
      plot_data        <- tidyr::unnest(plot_data, !! data_column)
      x_axis_quo       <- rlang::quo(forecast_probability)
      x_axis_name      <- "forecast_probability"
      y_axis_quo       <- rlang::quo(proportion_occurred)
      y_axis_name      <- "proportion_occurred"
      plot_geom        <- "bar"
    },

    "economic_value" = {
      plot_data        <- tidyr::unnest(plot_data, !! score_quo)
      x_axis_quo       <- rlang::quo(cost_loss_ratio)
      x_axis_name      <- "cost_loss_ratio"
      y_axis_quo       <- rlang::quo(value)
      y_axis_name      <- "value"
    },

    "roc" = {
      plot_data        <- tidyr::unnest(plot_data, !! score_quo)
      x_axis_quo       <- rlang::quo(false_alarm_rate)
      x_axis_name      <- "false_alarm_rate"
      y_axis_quo       <- rlang::quo(hit_rate)
      y_axis_name      <- "hit_rate"
    },

    "brier_score_decomposition" = {
      plot_data        <- tidyr::gather(
        plot_data,
        .data$brier_score_reliability,
        .data$brier_score_resolution,
        .data$brier_score_uncertainty,
        key   = "component",
        value = "contribution_to_brier_score"
      ) %>%
        dplyr::mutate(component = gsub("brier_score_", "", .data$component))
      y_axis_quo       <- rlang::quo(contribution_to_brier_score)
      linetype_by_quo  <- rlang::quo(component)
      linetype_by_name <- rlang::quo_name(linetype_by_quo)
      linetyping       <- TRUE
    }

  )

  if (linetyping) {
    if (is.numeric(plot_data[[linetype_by_name]])) {
      plot_data[[linetype_by_name]] <- factor(plot_data[[linetype_by_name]])
    }
  }

  ### Ensure that x and y axes are numeric

  if (!grepl("rank_histogram", score_name)  && x_axis_name == "lead_time") {
    plot_data <- dplyr::mutate(
      plot_data,
      {{x_axis_name}} := as.numeric(!! x_axis_quo),
      {{y_axis_name}} := as.numeric(!! y_axis_quo)
    )
  }

  ###########################################################################
  # COLOURS
  ###########################################################################

  col_factors          <- unique(plot_data[[colour_by_name]])
  num_factors          <- length(col_factors)
  num_colours          <- num_factors
  if (num_colours < 3) {
    num_colours <- 3
  }
  colour_by_sym <- rlang::sym(colour_by_name)
  if (num_colours > 8) {
    num_colours <- 8
  }
  colours <- rep(RColorBrewer::brewer.pal(num_colours, "Dark2"), 7)
  default_colour_table <- data.frame(
    col_factor        = col_factors,
    colour            = colours[1:num_factors]
  )
  colnames(default_colour_table) <- c(colour_by_name, "colour")

  if (is.null(colour_table)) {

    if (num_factors > 8) {
      warning("The default colour table has 8 colours. Recycling colours.", call. = FALSE)
    }
    colour_table <- default_colour_table

  } else {

    colour_table <- dplyr::rename_with(
      colour_table, ~gsub("mname", "fcst_model", .x)
    )

    if (!all(c(colour_by_name, "colour") %in% tolower(names(colour_table)))) {
      warning(paste0(
        "colour_table must include columns with names `", colour_by_name, "` and `colour`.\n",
        "  Assigning colours automatically."
      ))
      if (num_factors > 8) {
        warning("The default colour table has 8 colours. Recycling colours.", call. = FALSE)
      }
      colour_table <- default_colour_table
    }

    colour_table <- dplyr::filter(colour_table, !! colour_by_quo %in% col_factors)

    if (!all(as.character(plot_data[[colour_by_name]]) %in% as.character(colour_table[[colour_by_name]]))){
      warning(paste0(
        "Not all ", colour_by_name, " entries in data have been assigned colours in colour_table.\n",
        "  Assigning colours automatically"
      ))
      colour_table <- default_colour_table
    } else if (!is.character(colour_table$colour) & !is.factor(colour_table$colour)) {
      warning(paste0(
        "Colours in colour_table must be strings - e.g. \"red\" or \"#FF6542\".\n",
        "  Assigning colours automatcally."
      ))
      colour_table <- default_colour_table
    }

  }

  colour_table <- dplyr::arrange(colour_table, !! colour_by_quo)
  colour_table[[colour_by_name]] <- factor(colour_table[[colour_by_name]])
  colour_table$colour            <- as.character(colour_table$colour)
  plot_data[[colour_by_name]]    <- factor(plot_data[[colour_by_name]], levels = levels(colour_table[[colour_by_name]]))

  ###########################################################################
  # SET UP THE BASIC PLOT SPACE
  ###########################################################################

  aspect1_score   <- score_name %in% c("reliability", "roc", "economic_value")
  plot_diagonal   <- score_name %in% c("reliability", "roc")
  plot_attributes <- score_name %in% c("reliability")

  # If all are NA, they can be logical instead of numeric
  if (aspect1_score) {
    plot_data[[x_axis_name]] <- as.numeric(plot_data[[x_axis_name]])
    plot_data[[y_axis_name]] <- as.numeric(plot_data[[y_axis_name]])
  }

  attrs <- get_attrs(verif_data)

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
      "::",
      attrs[["dttm"]]
    ),
    "none" = "",
    plot_title
  )
  plot_subtitle <- switch(tolower(plot_subtitle),
    "auto" = attrs[["num_stations"]],
    "none" = "",
    plot_subtitle
  )
  plot_caption <- switch(tolower(plot_caption),
    "auto" = attrs[["param"]],
    "none" = "",
    plot_caption
  )

  # Special faceted plot if plot_num_cases == TRUE
  if (score_type == "summary" & plot_num_cases & plot_geom == "line") {
    plot_data <- plot_data %>%
      tidyr::gather("panel", !! y_axis_quo, .data$num_cases, !! y_axis_quo) %>%
      dplyr::mutate(
        panel = dplyr::case_when(
          .data$panel == "num_cases" ~ "Number of Cases",
          TRUE                       ~ totitle(gsub("_", " ", rlang::quo_name(y_axis_quo)))
        ),
        panel = factor(
          .data$panel,
          levels = c(totitle(gsub("_", " ", rlang::quo_name(y_axis_quo))), "Number of Cases")
        )
      )
  }


  # Plot background
  if (tolower(colour_by_name == "none")) {
    gg <- ggplot2::ggplot(plot_data, ggplot2::aes(!! x_axis_quo, !! y_axis_quo, ...))
  } else {
    gg <- ggplot2::ggplot(plot_data, ggplot2::aes(!! x_axis_quo, !! y_axis_quo, colour = !! colour_by_quo, fill = !! colour_by_quo, ...))
  }

  if (is.function(colour_theme)) {
    theme_func <- colour_theme
  } else {
    if (!grepl("^theme_[[:alpha:]]", colour_theme)) colour_theme <- paste0("theme_", colour_theme)
    if (grepl("harp", colour_theme)) {
      function_env <- "harpVis"
    } else {
      function_env <- "ggplot2"
    }
    theme_func <- get(colour_theme, envir = asNamespace(function_env))
  }

  gg <- gg + theme_func(
    base_size      = base_size,
    base_family    = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  )
  gg <- gg + ggplot2::xlab(x_label)
  gg <- gg + ggplot2::ylab(y_label)
  gg <- gg + ggplot2::theme(legend.position = legend_position)
  gg <- gg + ggplot2::guides(
    colour   = ggplot2::guide_legend(title = NULL, nrow = num_legend_rows, byrow = TRUE),
    shape    = ggplot2::guide_legend(title = NULL, nrow = num_legend_rows, byrow = TRUE),
    fill     = ggplot2::guide_legend(title = NULL, nrow = num_legend_rows, byrow = TRUE),
    linetype = ggplot2::guide_legend(title = NULL)
  )

  # Axes
  if (log_scale_x) {
    gg <- gg + ggplot2::scale_x_log10()
  } else {
    if (x_axis_name %in% c("lead_time", "valid_hour")) {
      break_step <- switch(
        x_axis_name,
        "lead_time"  = 6,
        "valid_hour" = 3
      )
      gg <- gg + ggplot2::scale_x_continuous(breaks = seq(0, 1800, break_step))
    }
  }
  if (log_scale_y) {
    gg <- gg + ggplot2::scale_y_log10()
  }
  if (aspect1_score) {
    gg <- gg +
      ggplot2::scale_x_continuous(limits = c(-0.1, 1.1)) +
      ggplot2::scale_y_continuous(limits = c(-0.1, 1.1)) +
      ggplot2::coord_fixed(1, c(0, 1), c(0, 1), expand = FALSE)
  }

  if (score_type == "summary" & plot_num_cases & plot_geom == "line") {
    y_values <- dplyr::filter(plot_data, panel != "Number of Cases") %>%
      dplyr::pull(!! y_axis_quo)
  } else {
    y_values <- dplyr::pull(plot_data, !! y_axis_quo)
  }
  range_y  <- range(y_values, na.rm = TRUE)
  min_y    <- NA_real_
  max_y    <- NA_real_
  if (extend_y_to_zero & !aspect1_score) { #& plot_geom == "line"
    if (range_y[1] > 0) {
      min_y <- 0
    }
    if (range_y[2] < 0) {
      max_y <- 0
    }
  }
  if (!log_scale_y & !aspect1_score) {
    gg <- gg + ggplot2::scale_y_continuous(limits = c(min_y, max_y))
  }

  ###########################################################################
  # GEOMS
  ###########################################################################

  colour_vec <- colour_table[["colour"]]
  names(colour_vec) <- colour_table[[colour_by_name]]
  if (plot_geom %in% c("line", "lollipop")) {
    gg                <- gg + ggplot2::scale_colour_manual(values = colour_vec)#table$colour)
  } else {
    gg                <- gg + ggplot2::scale_fill_manual(values = colour_vec)#table$colour)
  }

  if (nchar(gsub("[[:space:]]", "", plot_title)) > 0) {
    gg <- gg + ggplot2::labs(title    = plot_title)
  }
  if (nchar(gsub("[[:space:]]", "", plot_subtitle)) > 0) {
    gg <- gg + ggplot2::labs(subtitle = plot_subtitle)
  }
  if (nchar(gsub("[[:space:]]", "", plot_caption)) > 0) {
    gg <- gg + ggplot2::labs(caption  = plot_caption)
  }

  if (plot_geom == "line") {

    if (plot_diagonal) {
      gg <- gg + ggplot2::geom_abline(slope = 1, intercept = 0, colour = "grey80", size = line_width * 0.5)
    }

    if (plot_attributes) {
      gg <- gg +
        ggplot2::geom_smooth(
          ggplot2::aes(y = .data$bss_ref_climatology),
          method    = "lm",
          formula   = y ~ x,
          se        = FALSE,
          fullrange = TRUE,
          colour    = "grey80",
          size      = line_width * 0.5,
          lty       = 2
        ) +
        ggplot2::geom_smooth(
          ggplot2::aes(y = .data$no_skill),
          method    = "lm",
          formula   = y ~ x,
          se        = FALSE,
          fullrange = TRUE,
          colour    = "grey80",
          size      = line_width * 0.5,
          lty       = 3
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

    gg <- gg + ggplot2::geom_col(
      ggplot2::aes(x = factor(!!x_axis_quo)),
      position = ggplot2::position_dodge(preserve = "single"), colour = "transparent"
    ) +
      ggplot2::scale_x_discrete(breaks = pretty(dplyr::pull(plot_data, !!x_axis_quo), n = 10))

  } else if (plot_geom == "lollipop") {

    gg <- gg +
      ggplot2::geom_point(
        aes(x = factor(!!x_axis_quo)), size = point_size, position = position_dodge(width = 1)) +
      ggplot2::geom_linerange(
        ggplot2::aes(x = factor(!!x_axis_quo), ymin = 0, ymax = !!y_axis_quo),
        size      = line_width,
        position  = position_dodge(width = 1),
        key_glyph = "point"
      ) +
      ggplot2::scale_x_discrete(breaks = pretty(dplyr::pull(plot_data, !!x_axis_quo)))

  } else {

    stop(paste("Unknown geom:", plot_geom), call. = FALSE)

  }

  facet_vars <- suppressWarnings(harpCore::psub(
    facet_vars,
    c("^leadtime$", "^mname$"),
    c("lead_time", "fcst_model")
  ))
  if (faceting) {
    gg <- gg + ggplot2::facet_wrap(
      facet_vars,
      ncol     = num_facet_cols,
      scales   = facet_scales,
      labeller = facet_labeller
    )
  }

  if (flip_axes) {
    gg         <- gg + ggplot2::coord_flip() + ggplot2::scale_x_reverse()
    free_scale <- "x"
  } else {
    free_scale <- "y"
  }

  if (score_type == "summary" & plot_num_cases & plot_geom == "line") {

    num_cases_position <- match.arg(num_cases_position)

    if (num_cases_position == "below") {
      ncol_num_cases <- 1
      override_which <- 2
      panel_position <- "1-1"
      scale_function <- ggplot2::scale_x_continuous
    }

    if (num_cases_position == "right") {
      ncol_num_cases <- 2
      override_which <- 2
      panel_position <- "1-1"
      scale_function <- ggplot2::scale_x_continuous
    }

    if (num_cases_position == "above") {
      ncol_num_cases <- 1
      override_which <- 1
      panel_position <- "1-2"
      scale_function <- ggplot2::scale_x_continuous
      gg$data        <- dplyr::mutate(
        gg$data,
        panel = factor(.data$panel, levels = rev(levels(.data$panel)))
      )
    }

    if (num_cases_position == "left") {
      ncol_num_cases <- 2
      override_which <- 1
      panel_position <- "2-1"
      scale_function <- ggplot2::scale_y_continuous
      gg$data        <- dplyr::mutate(
        gg$data,
        panel = factor(.data$panel, levels = rev(levels(.data$panel)))
      )
    }

    min_y_limit <- ifelse(extend_num_cases_to_zero, 0, NA)

    gg <- gg + facet_wrap_custom(
      "panel",
      scales          = paste0("free_", free_scale),
      ncol            = ncol_num_cases,
      scale_overrides = list(
        scale_override(override_which, ggplot2::scale_y_continuous(limits = c(min_y_limit, NA)))
      )
    )

    gt <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(gg))
    if (num_cases_position %in% c("above", "below")) {
      score_panel <- gt$layout$t[grep(paste0("panel-", panel_position), gt$layout$name)]
      gt$heights[score_panel] <- 5 * gt$heights[score_panel]
    } else {
      score_panel <- gt$layout$l[grep(paste0("panel-", panel_position), gt$layout$name)]
      gt$widths[score_panel] <- 5 * gt$widths[score_panel]
    }
    grid::grid.draw(gt)

  } else {

    gg

  }

}

# Function to convert to title case
totitle <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
    {s <- substring(s, 2); if(strict) tolower(s) else s},
    sep = "", collapse = " " )
  res <- sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
  special_names <- c("Roc", "Crps", "Rmse", "Stde", "Mae")
  for (special_name in special_names) {
    res <- gsub(special_name, toupper(special_name), res)
  }
  res
}

# Function to convert Infinite values to NA
inf_to_na <- Vectorize(function(x) if (is.infinite(x)) { NA } else { x })

# Function to convert date to a nice format
date_to_char <- function(date_in) {
  suppressMessages(harpCore::as_dttm(date_in)) %>%
    format("%H:%M %d %b %Y")
}

# Compatibility for different attributes in new and old point verif outputs
# for automatic plot title, subtitle and caption generation
get_attrs <- function(x) {
  UseMethod("get_attrs")
}

get_attrs.default <- function(x) {
  list(
    dttm = paste0(
      date_to_char(attr(x, "start_date")),
      " - ",
      date_to_char(attr(x, "end_date"))
    ),
    num_stations = paste(attr(x, "num_stations"), "stations"),
    param = paste("Verification for", attr(x, "parameter"))
  )
}

get_attrs.harp_verif <- function(x) {
  list(
    dttm = paste(
      date_to_char(
        harpCore::as_str_dttm(
          sort(harpCore::as_dttm(range(attr(x, "dttm"))))
        )
      ),
      collapse = " - "
    ),
    num_stations = paste(
      length(Reduce(union, attr(x, "stations"))), "stations"
    ),
    param = paste("Verification for", attr(x, "parameter"))
  )
}

# There can be multiple possible x axes in the data depending on the
# grouping. We need to make sure that the axes are numeric, or in a date-time
# format - removing all rows with an "All" entry

filter_for_x <- function(plot_data, x_axis_name) {
  possible_x_axes <- intersect(
    c("lead_time", "valid_dttm", "fcst_dttm", "valid_hour"),
    colnames(plot_data)
  )

  if (length(possible_x_axes) > 1) {
    other_x_names <- possible_x_axes[possible_x_axes != x_axis_name]
    plot_data <- dplyr::filter(plot_data, .data[[x_axis_name]] != "All")
    if (grepl("dttm", x_axis_name)) {
      plot_data[[x_axis_name]] <- do.call(
        c, lapply(plot_data[[x_axis_name]], as.POSIXct, tz = "UTC")
      )
    } else {
      plot_data[[x_axis_name]] <- as.numeric(plot_data[[x_axis_name]])
    }
    plot_data <- dplyr::filter(
      plot_data,
      dplyr::if_all(other_x_names, ~ .x == "All")
    )
  }
  plot_data
}
