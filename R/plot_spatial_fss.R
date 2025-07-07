#' Plot FSS scores
#'
#' @param plot_data A tibble from \code{plot_spatial_verif} that contains necessary scores.
#' @param score_name Name of the score to plot. Technically not really necessary
#'   but makes it more intuitive when calling from \code{plot_spatial_verif}.
#' @param flip_axes Logical of whether to swap the x and y axes.
#' @param colour_by The column to colour the plot lines by.
#'   Can be an option between the score ("fss"), scale or threshold.
#' @param point_size The size of points to plot.
#' @param line_width The width of lines to plot. The default is 1.
#' @param num_facet_cols Number of columns in the faceted plot.

plot_spatial_fss <- function(
  plot_data,
  score_name        = "FSS",
  flip_axes         = FALSE,
  colour_by         = "fss",
  point_size        = 1.2,
  line_width        = 1,
  num_facet_cols    = NULL,
  ref_model         = NULL,

  ...) {

  if (is.null(plot_data)) stop("No data found.")
  if (any(!is.element(c("threshold", "scale", "fss"), names(plot_data)))) {
    stop("plot_data must have columns named threshold, scale and fss !")
  }

  if (colour_by == "fss" || colour_by == "auto" || is.na(colour_by)) {
    colour_by   <- "fss"
    x_data      <- "threshold"
    y_data      <- "scale"
    plot_type   <- "area"
  } else if (colour_by == "threshold") {
    x_data      <- "scale"
    y_data      <- "fss"
    plot_type   <- "line"
  } else if (colour_by == "scale") {
    x_data      <- "threshold"
    y_data      <- "fss"
    plot_type   <- "line"
  } else {
    message(paste("colour_by should either be 'scale' or 'threshold'"))
    colour_by   <- "fss"
    x_data      <- "threshold"
    y_data      <- "scale"
    plot_type   <- "area"
  }

  ### calculate mean of every threshold/scale pair
  plot_data <- plot_data %>%
               dplyr::group_by(model, prm, threshold, scale) %>%
               dplyr::mutate(num_cases = n()) %>%
               dplyr::group_by(model, prm, threshold, scale, num_cases) %>%
               dplyr::summarize_at("fss", mean, na.rm = TRUE)
  
  pt <- paste0(unique(plot_data$model),collapse=",")
  if (plot_type == "area") {
    
    # Is ref_model provided and valid?
    plot_diff <- F
    if (!is.null(ref_model)) {
      if ((ref_model %in% unique(plot_data$model)) & (length(unique(plot_data$model)) > 1)) {
            plot_diff <- T
          }
    }
    
    if (plot_diff) {
      ref_data  <- plot_data %>% dplyr::filter(model == ref_model) %>%
        dplyr::mutate(fss_ref = fss, num_cases_ref = num_cases) %>% dplyr::ungroup() %>% 
        dplyr::select(-model,-fss,-num_cases)
      plot_data <- plot_data %>% dplyr::filter(model != ref_model)
      plot_data <- dplyr::inner_join(plot_data,
                                     ref_data,
                                     by=setdiff(names(plot_data),c("fss","model","num_cases")),
                                     relationship = "many-to-many") %>% 
        dplyr::mutate(fss_diff = fss - fss_ref,
                      num_cases_diff = num_cases - num_cases_ref)
      c_low <- "red"
      c_mid <- "white"
      c_hig <- "blue"
      c_lim <- c(-0.5,0.5)
      mid   <- 0
      pt    <- paste0(paste0(unique(plot_data$model),collapse=","),
                      " compared to ",ref_model)
    } else {
      c_low <- "red"
      c_mid <- "yellow"
      c_hig <- "darkgreen"
      c_lim <- c(0,1)
      mid   <- 0.5
    }
  }
  
  if (score_name == "num_cases") {
    if (plot_diff) {
      score <- "num_cases_diff"
      score_name <- "FSS cases diff"
    } else {
      score <- "num_cases"
      score_name <- "FSS cases"
    }
    c_lim <- c(NA,NA)
    tl    <- "%1.0f"
    ts    <- 4
    # Check if number of cases is constant for each forecast model
    # If so, just filter to one scale for readability
    qwe <- plot_data %>% 
      group_by(model, prm, threshold, scale) %>% 
      summarise(nu = length(unique(num_cases)))
    if (length(unique(qwe$nu)) <= length(unique(qwe$model))) {
      plot_data <- plot_data %>% ungroup("scale") %>% filter(scale == min(scale))
    }
  } else {
    if (plot_diff) {
      score <- "fss_diff"
      score_name <- paste0(score_name," diff")
    } else {
      score <- "fss"
    }
    tl <- "%1.2f"
    ts <- 4
  }
  
  if (plot_type == "area") {
        gg <- ggplot2::ggplot(plot_data, aes(x = as.factor(get(x_data)),
                                             y = as.factor(get(y_data)),
                                             fill = get(score),
                                             label = sprintf(tl, get(score)))) +
              ggplot2::geom_tile() +
              ggplot2::geom_text(colour = "black",
                                 size   = ts) +
              ggplot2::scale_fill_gradient2(low      = c_low,
                                            mid      = c_mid,
                                            high     = c_hig,
                                            limits   = c_lim,
                                            midpoint = mid,
                                            oob      = scales::squish,
                                            name     = score_name)
        if (length(unique(plot_data$model)) > 1) {
          gg <- gg + ggplot2::facet_wrap(~model)
        }
  }
  if (plot_type == "line") {
        gg <- ggplot2::ggplot(plot_data, aes(x = get(x_data),
                                             y = get(y_data),
                                             colour = as.character(get(colour_by)))) +
              ggplot2::geom_line(size = line_width) +
              ggplot2::scale_x_continuous(breaks = unique(plot_data[[x_data]])) +
              ggplot2::geom_point(size = point_size) +
              ggplot2::labs(colour = stringr::str_to_title(colour_by))
  }

  gg <- gg + ggplot2::labs(title = paste("Score: ", score_name,
                                       ", Model: ", pt,
                                       ", Param: ", unique(plot_data$prm)),
                           x = stringr::str_to_title(x_data),
                           y = stringr::str_to_title(y_data))

  ## Other settings

  if (flip_axes) {gg <- gg + ggplot2::coord_flip()}

  gg
}
