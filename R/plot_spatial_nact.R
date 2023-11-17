#' Plot scores from neighborhood based contingency tables
#' based on Stein and Stoop article, 2019 Jan
#' Neighborhood-Based Contingency Tables Including Errors Compensation
#'
#' @param plot_data A tibble from \code{plot_spatial_verif} that contains necessary scores.
#' @param score_name In this context, table name. Technically not really necessary
#'   but makes it more intuitive when calling from \code{plot_spatial_verif}.
#' @param point_size The size of points to plot.
#' @param extend_y_to_zero Logical. Whether to extend the y-axis to include
#'   zero.
#' @param line_width The width of lines to plot. The default is 1.
#' @param y_label Label for the y-axis. Set to "auto" to use "Score". Anything else inside
#'   quotes will be used as the y-axis label.
#' @param x_label Label for the x-axis. Set to "auto" to use threshold
#'   or scale depending on the data. Anything else inside
#'   quotes will be used as the x-axis label.
#' @param flip_axes Logical of whether to swap the x and y axes.
#' @param nact_scores Actual scores to plot.
#'   Available scores based on NACT are "fbias", "pod", "far", "pss", "hss", "sedi".
#' @param colour_by The column to colour the plot lines by.
#'   Can be an option between scale or threshold.
#' @param num_facet_cols Number of columns in the faceted plot.

plot_spatial_nact <- function(
  plot_data,
  score_name        = "NACT",
  point_size        = 1.2,
  extend_y_to_zero  = TRUE,
  line_width        = 1,
  y_label           = "auto",
  x_label           = "auto",
  flip_axes         = FALSE,
  nact_scores       = list("all"),
  colour_by         = "scale",
  num_facet_cols    = NULL,

  ...) {

  if (is.null(plot_data)) stop("No data found.")
  if (any(!is.element(c("threshold", "scale", "hit", "fa", "miss", "cr"),
                      names(plot_data)))) {
    stop("plot_data must have columns named threshold, scale, hit, fa, miss and cr!")
  }
  if ("all" %in% nact_scores || is.na(nact_scores) || length(nact_scores) < 1) {
    #list all available scores
    nact_scores <- list("fbias",
                        "pod",
                        "far",
                        "pss",
                        "hss",
                        "sedi")
  }
  if (colour_by == "scale" || colour_by == "auto") {
    colour_by   <- "scale"
    x_data      <- "threshold"
  } else if (colour_by == "threshold") {
    x_data      <- "scale"
  } else {
    message(paste("colour_by should either be 'scale' or 'threshold'"))
    colour_by   <- "scale"
    x_data      <- "threshold"
  }

  message("Plotting score: ", paste(nact_scores, collapse = ", "))

  plot_data <- plot_data %>%
               dplyr::group_by(model,
                               prm,
                               threshold,
                               scale) %>%
               dplyr::summarise(hit  = mean(hit,  na.rm = TRUE),
                                fa   = mean(fa,   na.rm = TRUE),
                                miss = mean(miss, na.rm = TRUE),
                                cr   = mean(cr,   na.rm = TRUE))

  A <- plot_data$hit
  B <- plot_data$fa
  C <- plot_data$miss
  D <- plot_data$cr

  if ("fbias" %in% nact_scores) {
      nact_score <- "fbias" # Frequency bias
      plot_data[nact_score] <- (A + B) / (A + C)
  }
  if ("pod" %in% nact_scores) {
      nact_score <- "pod"   # Probability of detection
      plot_data[nact_score] <- A / (A + C)
  }
  if ("far" %in% nact_scores) {
      nact_score <- "far"   # False-alarm ratio
      plot_data[nact_score] <- B  / (A + B)
  }
  if ("pss" %in% nact_scores) {
      nact_score <- "pss"   # Pierce skill score
      plot_data[nact_score] <- ((A / (A + C)) -
                                (B / (B + D)))
  }
  if ("hss" %in% nact_scores) {
      nact_score <- "hss"   # Heidke skill score
      Ar <- ((A + B) * (A + C)) / (A + B + C + D)
      Dr <- ((B + D) * (C + D)) / (A + B + C + D)

      plot_data[nact_score] <- ((A + D - Ar - Dr) /
                                (A + B + C + D - Ar - Dr))
  }
  if ("sedi" %in% nact_scores) {
      nact_score <- "sedi"  # Symmetric extremal dependency index
      plot_data[nact_score] <-
      ((log(B / (B + D)) - log(A / (A + C)) + log(C / (A + C)) - log(D / (B + D))) /
       (log(B / (B + D)) + log(A / (A + C)) + log(C / (A + C)) + log(D / (B + D))))
  }

  ## At this point we will have a table of
  ## N-number of scores (columns) from above

  #convert to long table for facet_wrap
  plot_data <- plot_data %>%
               gather("score",
                      "value",
                      c(paste(nact_scores)))

  plot_data$value <- replace(plot_data$value, is.na(plot_data$value), NA)       # NaN to NA
  plot_data$value <- replace(plot_data$value, is.infinite(plot_data$value), NA) # Inf to NA

  if (grepl("hira_", score_name, fixed = TRUE)) {
        score_name <- switch(score_name,
            "hira_me"   = "HiRA Multi Event",
            "hira_pph"  = "HiRA Practically Perfect Hindcast",
            # "hira_pragm"   = "HiRA Pragmatic method",
            # "hira_crss"    = "HiRA Conditional square root for RPS",
        )
  } else {
      score_name <- toupper(score_name)
  }

  gg <- ggplot2::ggplot(plot_data, aes(x = get(x_data),
                                       y = value,
                                       colour = as.character(get(colour_by)))) +
        ggplot2::scale_x_continuous(breaks = unique(plot_data$threshold)) +
        ggplot2::geom_line(size = line_width) +
        ggplot2::geom_point(size = point_size) +
        ggplot2::labs(title = paste("Scores from", score_name,
                                    ", Param: ", unique(plot_data$prm)),
                      y = y_label,
                      x = x_label,
                      colour = str_to_title(colour_by)) +
        facet_wrap(. ~ score,
                   ncol = num_facet_cols,
                   labeller = labeller(score = toupper))

  ## Other settings

  if (extend_y_to_zero) {gg <- gg + ggplot2::ylim(-0.025, NA)}
  if (flip_axes) {gg <- gg + ggplot2::coord_flip()}
  if (y_label == "auto") {gg <- gg + ggplot2::labs(y = str_to_title("Score"))}
  if (x_label == "auto") {gg <- gg + ggplot2::labs(x = str_to_title(x_data))}

  gg
}
