#' Scatter plot of forecast vs observations
#'
#' @param .fcst A \code{harp_fcst} type object.
#' @param fcst_model The model in the \code{harp_fcst} object to make a scatter
#'   plot for - must not be quoted.
#' @param parameter The parameter to plot - used to identify the observations
#'   column in \code{.fcst}. Must not be quoted.
#' @param members The member(s) to include in the forecast statistics if
#'   \code{.fcst} is for EPS forecasts. Must be either a numeric vector of
#'   member numbers or "all" to include all members.
#' @param binwidth A 2 element numeric vector giving the width of the bins to
#'   plot for the observed and forecast directions respectively. Set to NULL
#'   (the default) to use the deafult of the width that gives 30 bins.
#' @param colours A chracter vector of colours used to generate a colour
#'   gradient.
#' @param ... Options for \link[ggplot2]{scale_colour_gradientn}
#'
#' @return A hexbin plot.
#' @export
#'
#' @examples
plot_scatter <- function(.fcst, fcst_model, parameter, members = "all", facet_members = TRUE, binwidth = NULL, colours = NULL, ...) {

  fcst_model_quo   <- rlang::enquo(fcst_model)
  fcst_model_expr  <- rlang::quo_get_expr(fcst_model_quo)
  if (is.character(fcst_model_expr)) {
    fcst_model_quo <- rlang::sym(fcst_model)
  }
  fcst_model_name  <- rlang::quo_name(fcst_model_quo)

  if (!fcst_model_name %in% names(.fcst)) {
    stop (fcst_model_name, " not found in .fcst.", call. = FALSE)
  }

  parameter_quo   <- rlang::enquo(parameter)
  parameter_expr  <- rlang::quo_get_expr(parameter_quo)
  if (is.character(parameter_expr)) {
    parameter_quo <- rlang::sym(parameter)
  }
  parameter_name  <- rlang::quo_name(parameter_quo)

  plot_data <- .fcst[[fcst_model_name]]

  if (!parameter_name %in% names(plot_data)) {
    stop (parameter_name, " observations not found in .fcst[['", fcst_model_name, "']]", call. = FALSE)
  }

  if (any(grepl("_mbr", names(plot_data)))) {
    attr(plot_data, "dataframe_format") <- "wide"
    plot_data <- harpPoint::gather_members(plot_data)
    if (is.numeric(members)) {
      members   <- paste0("mbr", formatC(members, width = 3, flag = "0"))
      plot_data <- dplyr::filter(plot_data, .data$member %in% members)
    } else if (members != "all") {
      stop("'member' must be a numeric vector or 'all'.", call. = FALSE)
    }
  } else {
    if (any(grepl("_det", names(plot_data)))) {
      det_names <- names(plot_data)[grep("_det", names(plot_data))]
      if (length(det_names) > 1) {
        stop("Yo! We ain't having you have more than 1 deterministic model, fool!", call. = FALSE)
      }
      plot_data <- dplyr::rename(plot_data, forecast = .data[[det_names]])
    }
  }

  plot_data  <- dplyr::rename(plot_data, observed = !! parameter_quo)
  data_range <- range(c(plot_data$forecast, plot_data$observed))

  x_axis     <- rlang::sym("observed")
  y_axis     <- rlang::sym("forecast")

  gg <- ggplot2::ggplot(plot_data, ggplot2::aes(!! x_axis, !! y_axis)) +
    ggplot2::geom_hex(binwidth = binwidth) +
    ggplot2::geom_abline(slope = 1, intercept = 0, colour = "grey50") +
    ggplot2::coord_fixed(1, data_range, data_range) +
    ggplot2::theme_bw()

  if (!is.null(colours)) {
    gg <- gg + ggplot2::scale_fill_gradientn(colours = colours, ...)
  } else {
    gg <- gg + ggplot2::scale_fill_viridis_c(option = "C")
  }

  num_members <- length(unique(plot_data$member))
  if (num_members > 1 && facet_members) {
    gg <- gg + ggplot2::facet_wrap("member", ncol = ceiling(sqrt(num_members)))
  }

  gg

}
