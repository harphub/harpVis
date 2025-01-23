#' Plot "basic" scores that have only one value column such as MSE, bias, MAE etc.
#'
#' @param plot_data A tibble from \code{plot_spatial_verif} that contains necessary scores.
#' @param score_name Name of the score to plot. Necessary (compared to other plotting functions)
#'   since it will be used to select the respective table that contains the scores.
#' @param point_size The size of points to plot.
#' @param extend_y_to_zero Logical. Whether to extend the y-axis to include
#'   zero.
#' @param line_width The width of lines to plot. The default is 1.
#' @param y_label Label for the y-axis. Set to "auto" to use score name. Anything else inside
#'   quotes will be used as the y-axis label.
#' @param x_label Label for the x-axis. Set to "auto" to use "Forecast length".
#'   Anything else inside quotes will be used as the x-axis label.
#' @param flip_axes Logical of whether to swap the x and y axes.

plot_spatial_line <- function(
  plot_data,
  score_name,
  point_size        = 1.2,
  extend_y_to_zero  = TRUE,
  line_width        = 1,
  y_label           = "auto",
  x_label           = "auto",
  flip_axes         = FALSE,

  ...) {

  if (is.null(plot_data)) stop("No data found.")
  if (is.null(score_name)) stop("No score_name given.")
  if (any(!is.element(score_name, names(plot_data)))) {
    stop("plot_data must have columns named ", score_name, " !")
  }

  message("Plotting score: ", score_name)
  ### grouping across all fcdates by leadtime, parameter and model
  plot_data <- plot_data %>%
               group_by(model, prm, leadtime) %>%
               summarise(score_name = mean(get(score_name), na.rm = TRUE))

  if (score_name %in% c("mse", "mae", "rmse", "stde")) {
       score_lab <- toupper(score_name)
  } else {
       score_lab <- str_to_title(score_name)
  }

  gg <- ggplot2::ggplot(plot_data, aes(x = leadtime,
                                       y = score_name,
                                       colour = model)) +
        ggplot2::geom_line(size = line_width) +
        ggplot2::geom_point(size = point_size) +
        ggplot2::scale_x_continuous(
            breaks = seq(min(plot_data$leadtime),
                         max(plot_data$leadtime, na.rm = TRUE),
                         by = plot_data$leadtime[2] - plot_data$leadtime[1])) +
        ggplot2::labs(title = paste("Score: ", score_lab,
                                  ", Param: ", unique(plot_data$prm)),
                      y = y_label,
                      x = x_label,
                      colour = "Model")

  ## Other settings

  if (extend_y_to_zero) {gg <- gg + ggplot2::ylim(min(min(plot_data$score_name)-0.025,-0.025), NA)}
  if (flip_axes) {gg <- gg + ggplot2::coord_flip()}
  if (y_label == "auto") {gg <- gg + ggplot2::labs(y = score_lab)}
  if (x_label == "auto") {gg <- gg + ggplot2::labs(x = "Forecast length")}

  gg
}
