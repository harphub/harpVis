#' Plot SAL scores
#'
#' @param plot_data A tibble from \code{plot_spatial_verif} that contains necessary scores.
#' @param score_name Name of the score to plot. Technically not really necessary
#'   but makes it more intuitive when calling from \code{plot_spatial_verif}.
#' @param point_size The size of points to plot.
#' @param extend_y_to_zero Logical. Whether to extend the y-axis to include
#' @param line_width The width of lines to plot. The default is 1.
#' @param quantiles A vector of quantiles that will be used to highlight an area of S/A values within those quantiles
#' @param xylim Plot x/y axis shared limit. Default "auto" will generate limits based on plot data.
#' @param show_stats An option to show median and mean values of S, A and L as a table next to the plot. Default FALSE.
#'
#' @export 

plot_spatial_sal <- function(
  plot_data, 
  score_name="SAL", 
  point_size=4, 
  extend_y_to_zero = TRUE, 
  line_width = 1, 
  quantiles=seq(0.25,0.75,0.05), 
  xylim = "auto", 
  show_stats = FALSE,
  ...) {
  
  if (is.null(plot_data)) stop("No data found.")
  # check that you have columns S, A, L
  if (any(!is.element(c("S", "A", "L"), names(plot_data)))) stop("plot_data must have columns named S, A and L !")

  # at this point: only 1 model and 1 prm should be in the data
  # in fact, the function should also allow input data without these columns!
  if (is.element("model", names(plot_data))) {
    if (length(unique(plot_data$model)) > 1) stop("You are mixing several models in a single SAL plot.")
  }

  if (is.element("prm", names(plot_data))) {
    if (length(unique(plot_data$prm)) > 1) stop("You are mixing several parameters in a single SAL plot.")
  }
  
  if (xylim == "auto") {
    xylim <- max(pmax(plot_data$S,plot_data$A,plot_data$L, na.rm = TRUE), na.rm = TRUE) + 0.25
  }
  
  message("Plotting score: ",score_name)
  
  ### Assumption is made that days without precipitation will have NA values, 
  ### since it does not work well with a large majority of zero values 
  ### when there are neither observed or forecasted precipitation

  S_percs <- quantile(plot_data$S, probs = quantiles, na.rm=TRUE)
  A_percs <- quantile(plot_data$A, probs = quantiles, na.rm=TRUE)
  #L_percs <- quantile(plot_data$L, probs = quantiles, na.rm=TRUE)

  medianS <- median(plot_data$S, na.rm=TRUE)
  medianA <- median(plot_data$A, na.rm=TRUE)
  medianL <- median(plot_data$L, na.rm=TRUE)

  meanS <- mean(plot_data$S, na.rm=TRUE)
  meanA <- mean(plot_data$A, na.rm=TRUE)
  meanL <- mean(plot_data$L, na.rm=TRUE)
  
  mytable <- cbind(data.frame(stats=c("median S","median A","median L","mean S","mean A","mean L"),value=sprintf("%.03f",c(medianS,medianA,medianL,meanS,meanA,meanL)))) # for a table in the plot, not implemented yet
  
  gg <- ggplot2::ggplot(plot_data, aes(x = S, y = A, colour = L))
  gg <- gg + 
            ggplot2::geom_rect(aes(xmin = min(S_percs),xmax = max(S_percs), ymin =  min(A_percs),ymax = max(A_percs), fill = paste("quantiles:",quantiles[1],"-",quantiles[length(quantiles)])), colour = NA, alpha = 0.04) +
            ggplot2::geom_hline(yintercept = 0, colour = "grey80") + 
            ggplot2::geom_vline(xintercept = 0, colour = "grey80") +
            ggplot2::geom_hline(aes(yintercept = medianA, linetype="median"), colour = "grey80") + 
            ggplot2::geom_vline(aes(xintercept = medianS), linetype="dashed", colour = "grey80") +
            ggplot2::scale_linetype_manual(name="", values = c(2, 2),guide = guide_legend(override.aes = list(color = c("grey80")))) + 
            ggplot2::scale_fill_manual('', values = 'grey80', guide = guide_legend(override.aes = list(alpha = 0.3))) +
            ggplot2::geom_point(size=point_size) + 
            ggplot2::xlim(-xylim, xylim) + ggplot2::ylim(-xylim, xylim) + 
            ggplot2::labs(y = "A", x = "S", colour="L") +
            ggplot2::scale_colour_gradient2(low = "darkblue", mid = "yellow", high = "red") +
            ggplot2::labs(
            title=paste("Score: ",score_name,", Model: ",unique(plot_data$model),", Param: ",unique(plot_data$prm))) +  coord_cartesian(clip="off")
            if (show_stats) {
                library(gridExtra)
                gg <- gg + 
                ggplot2::annotation_custom(tableGrob(mytable, rows = NULL, cols = NULL, theme = ttheme_default(base_size = 7)), xmin=xylim*1.725, xmax=xylim, ymin=-xylim*1.06, ymax=-xylim)
            }
  gg
}