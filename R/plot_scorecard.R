#' Title
#'
#' @param bootstrap_data
#' @param fcst_model
#' @param ref_model
#' @param scores
#' @param facet_by
#' @param filter_by
#' @param significance_breaks
#' @param colours
#' @param fills
#' @param shapes
#' @param sizes
#' @param legend_labels
#' @param num_facet_cols
#'
#' @return
#' @export
#'
#' @examples
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

  stopifnot(inherits(bootstrap_data, "harp_bootstrap"))

  sc_data <- bootstrap_data[["confidence_of_differences"]]

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

  sc_data[["class"]] <- factor(
    as.numeric(
      cut(
        abs(sc_data[["pc_diff"]]) * sc_data[["better"]],
        breaks = significance_breaks
      )
    ),
    labels = legend_labels
  )

  if (grid_facets) {
    if (length(facet_by) != 2) {
      warning("Need to facet variables for facet_grid = TRUE. Wrapping facets.")
      grid_facets <- FALSE
    }
  }

  if (!is.null(filter_by) && rlang::is_quosures(filter_by)) {
    sc_data <- dplyr::filter(sc_data, !!!filter_by)
  }

  gg <- ggplot2::ggplot(
    sc_data,
    ggplot2::aes(
      x      = factor(.data[["leadtime"]]),
      y      = forcats::fct_rev(forcats::fct_inorder(.data[["parameter"]])),
      fill   = .data[["class"]],
      colour = .data[["class"]],
      shape  = .data[["class"]],
      size   = .data[["class"]]
    )
  ) +
    ggplot2::geom_point() +
    ggplot2::scale_fill_manual(values = fills, breaks = legend_labels) +
    ggplot2::scale_colour_manual(values = colours, breaks = legend_labels) +
    ggplot2::scale_shape_manual(values = shapes, breaks = legend_labels) +
    ggplot2::scale_size_manual(values = sizes, breaks = legend_labels) +
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
