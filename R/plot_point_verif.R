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
#' @param colour_theme The colour theme for the plot - can be any ggplot2 theme
#'   (see \link[ggplo2]{theme_grey}), or "theme_harp_grey",
#'   "theme_harp_midnight", or "theme_harp_black".
#' @param ... Arguments to \link[ggplot2]{theme}
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
  verif_type       = c("ens", "det"),
  x_axis           = leadtime,
  y_axis           = rlang::enquo(score),
  colour_by        = mname,
  colour_table     = NULL,
  extend_y_to_zero = TRUE,
  plot_num_cases   = TRUE,
  facet_by         = NULL,
  num_facet_cols   = 3,
  facet_scales     = "fixed",
  facet_labeller   = "label_value",
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
  num_legend_rows  = 1,
  log_scale_x      = FALSE,
  log_scale_y      = FALSE,
  colour_theme     = "bw",
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
          plot_num_cases = FALSE
        }
        facet_vars <- purrr::map_chr(rlang::eval_tidy(facet_by), rlang::quo_name)
      } else {
        stop(facet_by_err, call. = FALSE)
      }
    }
  }

  # the column(s) to filter data for before plotting
  filter_by_err  <- paste("filter_by must be wrapped in vars and unquoted,\n",
    "e.g. filter_by = vars(leadtime == 12, threshold == 280).")
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
        filter_vars <- names(purrr::map_chr(rlang::eval_tidy(filter_by), rlang::quo_name))
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

  tables_with_data <- names(verif_data)[map_lgl(verif_data, ~!is.null(.x))]
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
    derived_summary_scores <- c("spread_skill", "spread_skill_ratio", "normalized_rank_histogram")
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

  if (nrow(plot_data) == 0) return()
  ###########################################################################
  # PREP DATA FOR PLOTTING
  ###########################################################################

  if (filtering) {
    plot_data <- dplyr::filter(plot_data, !!! filter_by)
  }

  plot_geom <- "line"

  switch(

    score_name,

    "spread_skill" = {
      plot_data        <- tidyr::gather(plot_data, .data$rmse, .data$spread, key = "component", value = "spread ; skill")
      y_axis_name      <- "spread ; skill"
      y_axis_quo       <- rlang::sym(y_axis_name)
      linetype_by_quo  <- rlang::quo(component)
      linetype_by_name <- rlang::quo_name(linetype_by_quo)
      linetyping       <- TRUE
    },

    "spread_skill_ratio" = {
      plot_data       <- dplyr::mutate(plot_data, !! rlang::sym(score_name) := .data$spread / .data$rmse)
    },

    "rank_histogram" = {
      plot_data        <- tidyr::unnest(plot_data, !! score_quo)
      if (!is.element("leadtime", facet_vars) & !is.element("leadtime", filter_vars)) {
        grouping_vars  <- rlang::syms(c(colour_by_name, facet_vars[nchar(facet_vars) > 0]))
        plot_data      <- dplyr::group_by(plot_data, !!!grouping_vars, .data$rank) %>%
          dplyr::summarise(rank_count = sum(.data$rank_count))
      }
      plot_data        <- dplyr::mutate(plot_data, rank = formatC(.data$rank, width = 3, flag = "0"))
      x_axis_quo       <- rlang::quo(rank)
      y_axis_quo       <- rlang::quo(rank_count)
      plot_geom        <- "bar"
    },

    "normalized_rank_histogram" = {
      data_column      <- rlang::sym("rank_histogram")
      plot_data        <- tidyr::unnest(plot_data, !! data_column)
      if (!is.element("leadtime", facet_vars) & !is.element("leadtime", filter_vars)) {
        grouping_vars  <- rlang::syms(c(colour_by_name, facet_vars[nchar(facet_vars) > 0]))
        plot_data      <- dplyr::group_by(plot_data, !!!grouping_vars, .data$rank) %>%
          dplyr::summarise(rank_count = sum(.data$rank_count))
      } else {
        grouping_vars  <- rlang::syms(c(colour_by_name, "leadtime", facet_vars[nchar(facet_vars) > 0]))
      }
      plot_data        <- plot_data %>%
        dplyr::group_by(!!!grouping_vars) %>%
        dplyr::mutate(
          mean_count           = mean(.data$rank_count),
          normalized_frequency = .data$rank_count / .data$mean_count
        )

      plot_data        <- dplyr::mutate(plot_data, rank = formatC(.data$rank, width = 3, flag = "0"))
      x_axis_quo       <- rlang::quo(rank)
      y_axis_quo       <- rlang::quo(normalized_frequency)
      plot_geom        <- "bar"
    },

    "reliability" = {
      plot_data        <- tidyr::unnest(plot_data, !! score_quo) %>%
        dplyr::mutate(no_skill = (.data$forecast_probability - .data$bss_ref_climatology) / 2 + .data$bss_ref_climatology)
      x_axis_quo       <- rlang::quo(forecast_probability)
      y_axis_quo       <- rlang::quo(observed_frequency)
    },

    "sharpness" = {
      data_column      <- rlang::sym("reliability")
      plot_data        <- tidyr::unnest(plot_data, !! data_column)
      x_axis_quo       <- rlang::quo(forecast_probability)
      y_axis_quo       <- rlang::quo(proportion_occurred)
      plot_geom        <- "bar"
    },

    "economic_value" = {
      plot_data        <- tidyr::unnest(plot_data, !! score_quo)
      x_axis_quo       <- rlang::quo(cost_loss_ratio)
      y_axis_quo       <- rlang::quo(value)
    },

    "roc" = {
      plot_data        <- tidyr::unnest(plot_data, !! score_quo)
      x_axis_quo       <- rlang::quo(false_alarm_rate)
      y_axis_quo       <- rlang::quo(hit_rate)
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
      paste0(date_to_char(attr(verif_data, "start_date")), " - ", date_to_char(attr(verif_data, "end_date")))
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

  gg <- gg + theme_func()
  gg <- gg + ggplot2::xlab(x_label)
  gg <- gg + ggplot2::ylab(y_label)
  gg <- gg + ggplot2::theme(legend.position = legend_position, ...)
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
    if (rlang::quo_name(x_axis_quo) == "leadtime") {
      gg <- gg + ggplot2::scale_x_continuous(breaks = seq(0, 1800, 6))
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

  y_values <- dplyr::pull(plot_data, !! y_axis_quo)
  range_y  <- range(y_values, na.rm = TRUE)
  min_y    <- range_y[1]
  max_y    <- range_y[2]
  if (extend_y_to_zero & !aspect1_score) { #& plot_geom == "line"
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
    gg                <- gg + ggplot2::scale_colour_manual(values = colour_table$colour)
  } else {
    gg                <- gg + ggplot2::scale_fill_manual(values = colour_table$colour)
  }

  if (score_type == "summary" & plot_num_cases & plot_geom == "line") {
    if (legend_position  == "bottom") gg <- gg + ggplot2::theme(legend.position = "none")
    hh <- gg
    gg <- gg + ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.text.x  = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      plot.margin  = ggplot2::unit(c(5.5, 5.5, 0.5, 5.5), "pt")
    )
  }

  if (nchar(plot_title) > 0)    gg <- gg + ggplot2::labs(title    = plot_title)
  if (nchar(plot_subtitle) > 0) gg <- gg + ggplot2::labs(subtitle = plot_subtitle)
  if (nchar(plot_caption) > 0 & !plot_num_cases)  gg <- gg + ggplot2::labs(caption  = plot_caption)

  if (plot_geom == "line") {

    if (plot_diagonal) {
      gg <- gg + ggplot2::geom_abline(slope = 1, intercept = 0, colour = "grey80", size = line_width * 0.5)
    }

    if (plot_attributes) {
      gg <- gg +
        ggplot2::geom_smooth(
          ggplot2::aes(y = .data$bss_ref_climatology),
          method    = "lm",
          se        = FALSE,
          fullrange = TRUE,
          colour    = "grey80",
          size      = line_width * 0.5,
          lty       = 2
        ) +
        ggplot2::geom_smooth(
          ggplot2::aes(y = .data$no_skill),
          method    = "lm",
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

    if (score_type == "summary" & plot_num_cases & plot_geom == "line") {
      hh <- hh +
        ggplot2::geom_line(ggplot2::aes(y = .data$num_cases), size = line_width) +
        ggplot2::coord_cartesian() +
        ggplot2::ylab("Num Cases") +
        ggplot2::theme(plot.margin = ggplot2::unit(c(0.5, 5.5, 5.5, 5.5), "pt"))
      if (legend_position == "bottom") {
        hh <- hh + ggplot2::theme(legend.position = "bottom")
        if (nchar(plot_caption > 0)) hh <- hh + ggplot2::labs(caption = plot_caption)
        height_ratio <- c(2.25, 1)
      } else {
        hh <- hh + ggplot2::theme(legend.position = "none")
        height_ratio <- c(3, 1)
      }
      gg <- cowplot::plot_grid(gg, hh, nrow = 2, rel_heights = height_ratio, align = "v", axis = "lr")
    }

  } else if (plot_geom == "bar") {

    gg <- gg + ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge(), colour = "grey30")

  } else {

    stop(paste("Unknown geom:", plot_geom), call. = FALSE)

  }

  if (faceting) {
    gg <- gg + ggplot2::facet_wrap(
      facet_by,
      ncol   = num_facet_cols,
      scales = facet_scales,
      labeller = facet_labeller
    )
  }

  gg

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
  suppressMessages(harpIO::str_datetime_to_unixtime(date_in)) %>%
    as.POSIXct(origin = "1970-01-01 00:00:00") %>%
    format("%H:%M %d %b %Y")
}
