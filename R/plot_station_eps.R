#' Title
#'
#' @param .fcst
#' @param SID
#' @param fcdate
#' @param quantiles
#' @param tz
#' @param control_member
#' @param type
#' @param x_axis
#' @param stack_quantiles
#' @param best_guess_line
#' @param ribbon_colours
#' @param line_colour
#' @param quantile_colours
#' @param stack_type
#' @param bar_width
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
plot_station_eps <- function(
  .fcst,
  SID,
  fcdate,
  type             = c("ribbon", "boxplot", "violin", "stacked_prob", "ridge"),
  x_axis           = c("leadtime", "validtime"),
  parameter        = "",
  quantiles        = c(0.05, 0.25, 0.75, 0.95),
  stack_quantiles  = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1),
  tz               = "",
  obs_column       = NULL,
  control_member   = "mbr000",
  best_guess_line  = ens_median,
  ribbon_colours   = NULL,
  line_colour      = "black",
  quantile_colours = NULL,
  stack_type       = c("column", "area"),
  bar_width        = 5,
  ncol             = NULL,
  ...
) {

  # Check inputs
  SID_quo     <- rlang::enquo(SID)
  fcdate_quo  <- rlang::enquo(fcdate)
  fcdate_expr <- rlang::quo_get_expr(fcdate_quo)
  fcdate      <- suppressMessages(harpIO::str_datetime_to_unixtime(fcdate_expr))
  fcdate_quo  <- rlang::quo(fcdate)

  best_guess_quo  <- rlang::enquo(best_guess_line)
  best_guess_expr <- rlang::quo_get_expr(best_guess_quo)
  if (is.character(best_guess_expr)) {
    best_guess_quo <- rlang::sym(best_guess_expr)
  }
  best_guess_name <- rlang::quo_name(best_guess_quo)

  type       <- match.arg(type)
  stack_type <- match.arg(stack_type)

  x_axis     <- match.arg(x_axis)
  x_axis_sym <- rlang::ensym(x_axis)

  parameter_quo <- rlang::enquo(parameter)
  parameter     <- rlang::quo_name(parameter)

  obs_column_quo <- rlang::enquo(obs_column)
  if (rlang::quo_is_null(obs_column_quo)) {
    plot_obs <- FALSE
  } else {
    plot_obs <- TRUE
    obs_column_expr <- rlang::quo_get_expr(obs_column_quo)
    if (is.character(obs_column_expr)) {
      obs_column_quo <- rlang::sym(obs_column_expr)
    }
  }

  # Filter the data
  plot_data <- purrr::map(
    .fcst,
    dplyr::filter,
    .data$SID    == rlang::eval_tidy(SID_quo),
    .data$fcdate == rlang::eval_tidy(fcdate_quo)
  )
  if (any(check_rows(plot_data) == 0)) {
    missing_fcst <- which(check_rows(plot_data) == 0)
    for (fcst_model in names(.fcst)[missing_fcst]) {
      warning("SID = ", SID, ", fcdate = ", fcdate_expr, " not found for forecast model: ", fcst_model, call. = FALSE, immediate. = TRUE)
    }
  }
  if (all(check_rows(plot_data) == 0)) {
    stop("No data to plot", call. = FALSE)
  }

  # Reshape data to tidy format plot
  plot_data <- plot_data %>%
    harpPoint:::new_harp_fcst() %>%
    harpPoint::gather_members() %>%
    unclass() %>%
    dplyr::bind_rows(.id = "mname")

  # Set the correct valid time for the timezone and set the x axis
  plot_data <- plot_data %>%
    dplyr::mutate(validtime = as.POSIXct(.data$validdate, origin = "1970-01-01 00:00:00", tz = tz)) %>%
    dplyr::rename(x = !! x_axis_sym)

  # Run the plot function
  eps_plot <- switch(type,
    "ribbon" = eps_ribbon_plot(
      plot_data,
      quantiles,
      control_member,
      !! best_guess_quo,
      ribbon_colours,
      line_colour,
      ...
    ),
    "boxplot" = eps_dist_plot(
      plot_data,
      dist = "boxplot",
      ...
    ),
    "violin" = eps_dist_plot(
      plot_data,
      dist = "violin",
      ...
    ),
    "stacked_prob" = eps_stacked_prob_plot(
      plot_data,
      stack_quantiles,
      stack_type,
      bar_width,
      ...
    ),
    "ridge" = eps_ridge_plot(
      plot_data,
      ...
    )
  )

  if (plot_obs) {
    func_args <- list(...)
    geom_args <- intersect(names(func_args), c(std_aes(), names(formals(geom_point))))
    ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$x, y = .data$forecast, group = .data$x)) +
      do.call(geom_func, func_args[geom_args])
  }

  if (is.null(ncol)) {
    if (type == "ridge") {
      eps_plot <- eps_plot + ggplot2::facet_wrap("mname", nrow = 1)
    } else {
      eps_plot <- eps_plot + ggplot2::facet_wrap("mname", ncol = 1)
    }
  }

  eps_plot +
    ggplot2::xlab(x_axis) +
    ggplot2::ylab(parameter) +
    ggplot2::theme_bw()

}

# Function to check the number of rows in each data frame of a list
check_rows <- function(df) {
  purrr::map_int(df, nrow)
}

####################################################
# PLOT FUNCTIONS
####################################################

# Function for eps ribbon plots
eps_ribbon_plot <- function(
  plot_data,
  quantiles,
  control_member,
  best_guess_line,
  ribbon_colours,
  line_colour,
  ...
) {

  # Quote arguments
  best_guess_quo  <- rlang::enquo(best_guess_line)
  best_guess_expr <- rlang::quo_get_expr(best_guess_quo)
  if (is.character(best_guess_expr)) {
    best_guess_quo <- rlang::sym(best_guess_expr)
  }
  best_guess_name <- rlang::quo_name(best_guess_quo)

  # Check the quantiles input
  quantiles     <- sort(quantiles)
  num_quantiles <- length(quantiles)
  if (num_quantiles %% 2 != 0) {
    stop("An even number of quantiles must be given", call. = FALSE)
  }

  # Set up ribbons
  num_ribbons      <- num_quantiles / 2
  ribbon_quantiles <- list()
  for (ribbon_number in 1:num_ribbons) {
    ribbon_quantiles[[ribbon_number]] <- c(
      paste0("q", quantiles[ribbon_number] * 100, "%"),
      paste0("q", quantiles[num_quantiles + 1 - ribbon_number] * 100, "%")
    )
  }

  if (is.null(ribbon_colours)) {
    n <- ifelse(num_ribbons < 3, 3, num_ribbons + 1)
    ribbon_colours <- RColorBrewer::brewer.pal(n, "Blues")
    ribbon_colours <- ribbon_colours[2: n]
  } else {
    if (length(ribbon_colours) < num_ribbons) {
      stop("Only ", length(ribbon_colours), " colours supplied for ", num_ribbons, " ribbons.", call. = FALSE)
    }
  }

  # Compute quantiles
  plot_data <- plot_data %>%
    dplyr::group_by(.data$mname, .data$x) %>%
    tidyr::nest() %>%
    dplyr::transmute(
      .data$mname,
      .data$x,
      ens_mean    = purrr::map_dbl(.data$data, ~ mean(.x$forecast)),
      ens_median  = purrr::map_dbl(.data$data, ~ median(.x$forecast)),
      ens_control = purrr::map_dbl(.data$data, ~ .x$forecast[.x$member == control_member]),
      quantiles   = purrr::map(.data$data, ~ as.list(quantile(.x$forecast, quantiles)))
    ) %>%
    dplyr::mutate(quantiles = purrr::map(.data$quantiles, ~ rlang::set_names(.x, ~ paste0("q", .)))) %>%
    dplyr::mutate(quantiles = purrr::map(.data$quantiles, dplyr::bind_cols)) %>%
    tidyr::unnest()


  # Generate the ggplot object
  gg <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$x))

  for (ribbon_number in 1:num_ribbons) {
    ymin <- rlang::sym(ribbon_quantiles[[ribbon_number]][1])
    ymax <- rlang::sym(ribbon_quantiles[[ribbon_number]][2])
    gg   <- gg + geom_ribbonspline(
      ggplot2::aes(ymin = !! ymin, ymax = !! ymax),
      fill = ribbon_colours[ribbon_number])
  }

  gg + ggalt::geom_xspline(ggplot2::aes(y = !! best_guess_quo), colour = line_colour)


}

# Function for eps distribution plots
eps_dist_plot <- function(plot_data, dist, ...) {
  func_args <- list(...)
  geom_func <- get(paste0("geom_", dist), envir = asNamespace("ggplot2"))
  geom_args <- intersect(names(func_args), c(std_aes(), names(formals(geom_func))))
  ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$x, y = .data$forecast, group = .data$x)) +
    do.call(geom_func, func_args[geom_args])
}

# Function for stacked probability plots
eps_stacked_prob_plot <- function(
  plot_data,
  quantiles,
  stack_type,
  bar_width,
  ...
) {

  # Sort the quantiles
  quantiles     <- sort(quantiles)
  num_quantiles <- length(quantiles)

  # Compute the quantiles - the quantile names are reversed to get probability > .
  plot_data <- plot_data %>%
    dplyr::group_by(.data$mname, .data$x) %>%
    tidyr::nest() %>%
    dplyr::transmute(
      .data$mname,
      .data$x,
       quantiles   = purrr::map(.data$data, ~ as.list(quantile(.x$forecast, quantiles)))
    ) %>%
    dplyr::mutate(quantiles = purrr::map(.data$quantiles, ~ rlang::set_names(.x, ~ rev(.)))) %>%
    dplyr::mutate(quantiles = purrr::map(.data$quantiles, dplyr::bind_cols)) %>%
    tidyr::unnest()

  # Gather quantiles and lag to create quantile start and end points
  plot_data <- plot_data %>%
    tidyr::gather(dplyr::ends_with("%"), key = "quantile", value = "forecast") %>%
    dplyr::group_by(.data$mname, .data$x) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      f1 = purrr::map(data, ~ dplyr::lag(.x$forecast)),
      quantile_end = purrr::map(data, ~ dplyr::lag(.x$quantile))
    ) %>%
    tidyr::unnest() %>%
    tidyr::gather(forecast, f1, key = "key", value = "forecast") %>%
    dplyr::mutate(quantile_label = paste(gsub("%", "", quantile), gsub("%", " %", quantile_end), sep = " - ")) %>%
    tidyr::drop_na()

  # Make the plot
  switch(stack_type,
    "column" = ggplot2::ggplot(
        plot_data, ggplot2::aes(factor(.data$x), .data$forecast, colour = quantile_label)
      ) +
      ggplot2::geom_line(size = bar_width),
    "area"   = ggplot2::ggplot(
        dplyr::filter(plot_data, key == "forecast"), ggplot2::aes(.data$x, .data$forecast, fill = quantile_label)
      ) +
      ggplot2::geom_area(position = "identity")
  )
}

# Function for ridge plots
eps_ridge_plot <- function(plot_data, ...) {
  ggplot2::ggplot(plot_data, ggplot2::aes(.data$forecast, factor(.data$x), fill = ..x..)) +
    ggridges::geom_density_ridges_gradient() +
    ggplot2::scale_fill_viridis_c(option = "C")
}

# Standard aesthetics for most ggplot2 geoms
std_aes <- function() {
  c("alpha", "colour", "fill", "linetype", "shape", "size", "stroke", "weight")
}

