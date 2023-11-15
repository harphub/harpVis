#' Plot a scorecard from bootstrapped verification
#'
#' This function is used to plot a score card from the output of a bootstrap
#' verification function such as \link{bootstrap_verify}.
#'
#' @param bootstrap_data The output from \link{bootstrap_verify}.
#' @param fcst_model The forecast model of interest.
#' @param ref_model The forecast model to be used as reference.
#' @param scores Vector of scores to include in the plot.
#' @param facet_by Column(s) to facet panels by - should be unquoted and wrapped
#'   in the \code{vars} function. Defaults to score.
#' @param filter_by Filtering function - column names should be unquoted and
#'   everything wrapped inside the \code{vars} function.
#' @param significance_breaks The breaks to for statistical significance of
#'   differences. Default is \code{c(-1.1, -0.997, -0.95, -0.68, 0.68, 0.95,
#'   0.997, 1.1)}.
#' @param colours Vector of outline colours used for the scorecard symbols.
#'   Should be one fewer then \code{significance_breaks}. Can be colour names or
#'   HEX colours. Default is \code{c("#CA0020", "#CA0020", "#CA0020", "grey70",
#'   "#0571B0", "#0571B0", "#0571B0")}.
#' @param fills Vector of fill colours used for the scorecard symbols. Should be
#'   one fewer then \code{significance_breaks}. Can be colour names or HEX
#'   colours. Default is \code{c("#CA0020", "#F4A582", NA, "grey70", NA,
#'   "#92C5DE", "#0571B0")}.
#' @param shapes Vector of shapes used for the scorecard symbols. Should be one
#'   fewer then \code{significance_breaks}. Default is \code{c(25, 25, 25, 22,
#'   24, 24, 24)}.
#' @param sizes Vector of sizes used for the scorecard symbols. Should be one
#'   fewer then \code{significance_breaks}. Default is \code{c(3, 2, 1, 0.5, 1,
#'   2, 3)}.
#' @param legend_labels Character vector of labels to be used for the scorecard
#'   symbols. Should be one fewer then \code{significance_breaks}. Set to "auto"
#'   to automatically generate labels based on \code{significance_breaks}
#' @param num_facet_cols Number of panel columns in the score card.
#'
#' @return A score card plot. Can be saved with \link[ggplot2]{ggsave}
#' @export
plot_scorecard <- function(
  bootstrap_data,
  fcst_model,
  ref_model,
  scores,
  facet_by            = vars(score),
  num_facet_rows      = 1,
  grid_facets         = FALSE,
  filter_by           = NULL,
  significance_breaks = c(-1.1, -0.997, -0.95, -0.68, 0.68, 0.95, 0.997, 1.1),
  colours             = c("#CA0020", "#CA0020", "#CA0020", "grey70", "#0571B0", "#0571B0", "#0571B0"),
  fills               = c("#CA0020", "#F4A582", NA, "grey70", NA, "#92C5DE", "#0571B0"),
  shapes              = c(25, 25, 25, 22, 24, 24, 24),
  sizes               = c(3, 2, 1, 0.5, 1, 2, 3),
  legend_labels       = "auto",
  num_facet_cols      = length(scores),
  ...
) {

  if (inherits(bootstrap_data, "harp_bootstrap")) {

    sc_data <- bootstrap_data[["confidence_of_differences"]]
    sc_data[["percent_better"]] <- abs(sc_data[["pc_diff"]]) *
      sc_data[["better"]]

  } else {

    possible_names <- c(
      "det_summary_scores",
      "det_threshold_scores",
      "ens_summary_scores",
      "ens_threshold_scores"
    )

    if (length(intersect(names(bootstrap_data), possible_names)) < 1) {
      stop("Do not know how to plot `bootstrap_data`.")
    }

    sc_data <- dplyr::bind_rows(harpCore::deharp(bootstrap_data)) %>%
      tidyr::drop_na()

    if (!is.element("parameter", colnames(sc_data))) {
      if (is.null(attr(bootstrap_data, "parameter"))) {
        stop("`bootstrap_data` is missing parameter names.")
      }
      sc_data[["parameter"]] <- attr(bootstrap_data, "parameter")
    }

    if (!is.element("percent_better", colnames(sc_data))) {
      stop("Do not know how to plot `bootstrap_data`")
    }

    sc_data[["percent_better"]][sc_data[["percent_better"]] < 0.5] <-
      sc_data[["percent_better"]][sc_data[["percent_better"]] < 0.5] - 1

  }

  sc_data <- sc_data[sc_data[["fcst_model"]] == fcst_model, ]
  if (nrow(sc_data) < 1) {
    stop(
      "fcst_model: '", fcst_model,
      "' not found in 'bootstrap_data'.",
      call. = FALSE
    )
  }

  sc_data <- sc_data[sc_data[["ref_model"]] == ref_model, ]
  if (nrow(sc_data) < 1) {
    stop(
      "ref_model: '", ref_model,
      "' not found in 'bootstrap_data' for fcst_model: '",
      fcst_model, "'.",
      call. = FALSE
    )
  }

  missing_scores <- setdiff(scores, sc_data[["score"]][sc_data[["score"]] %in% scores])
  missing_scores <- sapply(missing_scores, score_lookup)
  missing_scores <- missing_scores[missing_scores != names(missing_scores)]

  scores[tolower(scores) == names(missing_scores)] <- missing_scores

  missing_scores <- setdiff(scores, sc_data[["score"]][sc_data[["score"]] %in% scores])

  if (length(missing_scores) > 0) {
    warning(
      "'", paste(missing_scores, collapse = ",'"),
      "' not found in 'bootstrap_data'."
    )
  }

  sc_data <- sc_data[sc_data[["score"]] %in% scores, ]
  if (nrow(sc_data) < 1) {
    stop("None of the requested scores are in 'bootstrap_data'")
  }

  sc_data[["score"]] <- toupper(gsub("_", " ", sc_data[["score"]]))
  sc_data[["score"]] <- factor(
    sc_data[["score"]],
    levels = toupper(gsub("_", " ", scores))
  )

  check_length <- function(vec, vec1) {
    vec_name  <- deparse(substitute(vec))
    vec1_name <- deparse(substitute(vec1))
    if (length(vec1) != (length(vec) - 1)) {
      stop(
        "'", vec1_name, "' [length: ", length(vec1), "] ",
        "must have 1 fewer elements than '", vec_name,
        "' [length: ", length(vec), "].",
        call. = FALSE
      )
    }
  }

  check_length(significance_breaks, colours)
  check_length(significance_breaks, fills)
  check_length(significance_breaks, shapes)
  check_length(significance_breaks, sizes)

  if (length(legend_labels == 1) && legend_labels == "auto") {
    legend_labels <- generate_labels(fcst_model, ref_model, significance_breaks)
  } else {
    check_length(significance_breaks, legend_labels)
  }

  classes <- cut(
    sc_data[["percent_better"]],
    breaks = significance_breaks
  )
  names(legend_labels) <- levels(classes)

  sc_data[["class"]] <- forcats::fct_relabel(classes, ~legend_labels[.x])

  if (grid_facets) {
    if (length(facet_by) != 2) {
      warning("Need two facet variables for facet_grid = TRUE. Wrapping facets.")
      grid_facets <- FALSE
    }
  }

  if (!is.null(filter_by) && rlang::is_quosures(filter_by)) {
    sc_data <- dplyr::filter(sc_data, !!!filter_by)
  }

  gg <- ggplot2::ggplot(
    sc_data,
    ggplot2::aes(
      x      = forcats::fct_inseq(factor(.data[["lead_time"]])),
      y      = forcats::fct_rev(forcats::fct_inorder(.data[["parameter"]])),
      fill   = .data[["class"]],
      colour = .data[["class"]],
      shape  = .data[["class"]],
      size   = .data[["class"]]
    )
  ) +
    ggplot2::geom_point() +
    ggplot2::scale_fill_manual(values = fills, breaks = legend_labels, drop = FALSE) +
    ggplot2::scale_colour_manual(values = colours, breaks = legend_labels, drop = FALSE) +
    ggplot2::scale_shape_manual(values = shapes, breaks = legend_labels, drop = FALSE) +
    ggplot2::scale_size_manual(values = sizes, breaks = legend_labels, drop = FALSE) +
    ggplot2::guides(
      fill   = ggplot2::guide_legend(NULL, nrow = ceiling(length(fills) / 2)),
      colour = ggplot2::guide_legend(NULL, nrow = ceiling(length(fills) / 2)),
      shape  = ggplot2::guide_legend(NULL, nrow = ceiling(length(fills) / 2)),
      size   = ggplot2::guide_legend(NULL, nrow = ceiling(length(fills) / 2))
    ) +
    ggplot2::labs(x = "Lead Time [h]", y = NULL) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.border     = ggplot2::element_rect(colour = "grey70", fill = NA),
      panel.grid.major = ggplot2::element_blank(),
      legend.position  = "bottom",
      strip.background = ggplot2::element_rect(colour = "grey70", fill = NA),
      axis.ticks.x     = ggplot2::element_line(colour = "grey80")
    )

  if (grid_facets) {
    gg + ggplot2::facet_grid(facet_by[1], facet_by[2])
  } else {
    gg + ggplot2::facet_wrap(facet_by, nrow = num_facet_rows)
  }

}

generate_labels <- function(fcst_model, ref_model, breaks) {

  breaks_labels <- list()
  for (i in 1:(length(breaks) - 1)) {
    if (breaks[i] < 0 && breaks[i + 1] < 0) {
      breaks_labels[[i]] <- paste(
        fcst_model, "worse than", ref_model,
        "with signifcance >", paste0(abs(breaks[i + 1] * 100), "%")
      )
    } else if (breaks[i] > 0 && breaks[i + 1] > 0) {
      breaks_labels[[i]] <- paste(
        fcst_model, "better than", ref_model,
        "with signifcance >", paste0(breaks[i] * 100, "%")
      )
    } else {
      breaks_labels[[i]] <- paste(
        "No significant difference between",
        fcst_model, "and", ref_model
      )
    }
  }
  unlist(breaks_labels)
}
