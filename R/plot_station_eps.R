#' Plot eps data for a station
#'
#' `r lifecycle::badge("deprecated")`
#'
#' @export
#'
plot_station_eps <- function(
  .fcst,
  SID,
  fcst_dttm,
  type             = c("ribbon", "boxplot", "violin", "stacked_prob", "ridge", "spaghetti"),
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
  line_size        = 0.8,
  smooth_line      = FALSE,
  quantile_colours = NULL,
  stack_type       = c("column", "area"),
  bar_width        = 5,
  ncol             = NULL,
  ...
) {

  lifecycle::deprecate_stop(
    "0.1.0",
    "plot_station_eps()",
    "plot_station_ts()"
  )
  # Check inputs
  SID_quo     <- rlang::enquo(SID)
  fcst_dttm_quo  <- rlang::enquo(fcst_dttm)
  fcst_dttm_expr <- rlang::quo_get_expr(fcst_dttm_quo)
  fcst_dttm      <- harpCore::as_unixtime(fcst_dttm_expr)
  fcst_dttm_quo  <- rlang::quo(fcst_dttm)

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
    .data$fcst_dttm == rlang::eval_tidy(fcst_dttm_quo)
  )
  if (any(check_rows(plot_data) == 0)) {
    missing_fcst <- which(check_rows(plot_data) == 0)
    for (fcst_model in names(.fcst)[missing_fcst]) {
      warning("SID = ", SID, ", fcst_dttm = ", fcst_dttm_expr, " not found for forecast model: ", fcst_model, call. = FALSE, immediate. = TRUE)
    }
  }
  if (all(check_rows(plot_data) == 0)) {
    stop("No data to plot", call. = FALSE)
  }

  # Reshape data to tidy format plot
  plot_data <- plot_data %>%
    harpCore::as_harp_list() %>%
    harpCore::pivot_members() %>%
    unclass() %>%
    dplyr::bind_rows(.id = "fcst_model")

  # Set the correct valid time for the timezone and set the x axis
  plot_data <- plot_data %>%
    dplyr::mutate(validtime = as.POSIXct(.data$valid_dttm, origin = "1970-01-01 00:00:00", tz = tz)) %>%
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
    ),
    "spaghetti" = eps_spaghetti_plot(
      plot_data,
      line_colour,
      line_size,
      smooth_line,
      ...
    )
  )

  if (plot_obs) {
    func_args <- list(...)
    geom_args <- intersect(names(func_args), c(std_aes(), names(formals(geom_point))))
    eps_plot <- eps_plot +
      do.call(ggplot2::geom_point, c(list(data = plot_data, mapping = ggplot2::aes(y = !! obs_column_quo)), func_args[geom_args]))
  }

  if (is.null(ncol)) {
    if (type == "ridge") {
      eps_plot <- eps_plot + ggplot2::facet_wrap("fcst_model", nrow = 1)
    } else {
      eps_plot <- eps_plot + ggplot2::facet_wrap("fcst_model", ncol = 1)
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
    ribbon_colours <- ribbon_colours[2:n]
  } else {
    if (length(ribbon_colours) < num_ribbons) {
      stop("Only ", length(ribbon_colours), " colours supplied for ", num_ribbons, " ribbons.", call. = FALSE)
    }
  }

  # Compute quantiles
  plot_data <- tidyr::nest(plot_data, data = -tidyr::one_of(c("fcst_model", "x")))
  plot_data <- plot_data %>%
    dplyr::transmute(
      .data$fcst_model,
      .data$x,
      ens_mean    = purrr::map_dbl(.data$data, ~ mean(.x$forecast)),
      ens_median  = purrr::map_dbl(.data$data, ~ median(.x$forecast)),
      #ens_control = purrr::map_dbl(.data$data, ~ .x$forecast[.x$member == control_member]),
      quantiles   = purrr::map(.data$data, ~ as.list(quantile(.x$forecast, quantiles)))
    ) %>%
    dplyr::mutate(quantiles = purrr::map(.data$quantiles, ~ rlang::set_names(.x, ~ paste0("q", .)))) %>%
    dplyr::mutate(quantiles = purrr::map(.data$quantiles, dplyr::bind_cols))
    plot_data <- tidyr::unnest(plot_data, tidyr::one_of("quantiles"))

  # Generate the ggplot object
  gg <- ggplot2::ggplot(dplyr::arrange(plot_data, .data$x), ggplot2::aes(x = .data$x))

  for (ribbon_number in 1:num_ribbons) {
    ymin <- rlang::sym(ribbon_quantiles[[ribbon_number]][1])
    ymax <- rlang::sym(ribbon_quantiles[[ribbon_number]][2])
    gg   <- gg + geom_ribbonspline(
      ggplot2::aes(ymin = !! ymin, ymax = !! ymax),
      fill = ribbon_colours[ribbon_number])
  }

  gg + geom_linespline(ggplot2::aes(y = !! best_guess_quo), colour = line_colour)


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
  plot_data <- tidyr::nest(plot_data, data = -tidyr::one_of(c("fcst_model", "x")))
  plot_data <- plot_data %>%
    dplyr::transmute(
      .data$fcst_model,
      .data$x,
       quantiles   = purrr::map(.data$data, ~ as.list(quantile(.x$forecast, quantiles)))
    ) %>%
    dplyr::mutate(quantiles = purrr::map(.data$quantiles, ~ rlang::set_names(.x, ~ rev(.)))) %>%
    dplyr::mutate(quantiles = purrr::map(.data$quantiles, dplyr::bind_cols))
    plot_data <- tidyr::unnest(plot_data, tidyr::one_of("quantiles"))

  # Gather quantiles and lag to create quantile start and end points
  plot_data <- plot_data %>%
    tidyr::gather(dplyr::ends_with("%"), key = "quantile", value = "forecast")
  plot_data <- tidyr::nest(plot_data, data = -tidyr::one_of(c("fcst_model", "x")))
  plot_data <- plot_data %>%
    dplyr::mutate(
      f1 = purrr::map(data, ~ dplyr::lag(.x$forecast)),
      quantile_end = purrr::map(data, ~ dplyr::lag(.x$quantile))
    )
  plot_data <- tidyr::unnest(plot_data, tidyr::one_of(c("data", "f1", "quantile_end")))
  plot_data <- plot_data %>%
    tidyr::gather(forecast, .data$f1, key = "key", value = "forecast") %>%
    dplyr::mutate(quantile_label = paste(gsub("%", "", .data$quantile), gsub("%", " %", .data$quantile_end), sep = " - ")) %>%
    tidyr::drop_na()

  # Make the plot
  switch(stack_type,
    "column" = ggplot2::ggplot(
        plot_data, ggplot2::aes(.data$x, .data$forecast, colour = .data$quantile_label, group = .data$x)
      ) +
      ggplot2::geom_line(size = bar_width),
    "area"   = ggplot2::ggplot(
        dplyr::filter(plot_data, key == "forecast"), ggplot2::aes(.data$x, .data$forecast, fill = .data$quantile_label)
      ) +
      ggplot2::geom_area(position = "identity")
  )
}

# Function for ridge plots
eps_ridge_plot <- function(plot_data, ...) {

   if(!requireNamespace("ggridges", quietly = TRUE)) {
    stop("Please install the ggridges package from CRAN for ribbon plots", call. = FALSE)
  }

  ggplot2::ggplot(plot_data, ggplot2::aes(.data$forecast, factor(.data$x), fill = ..x..)) +
    ggridges::geom_density_ridges_gradient() +
    ggplot2::scale_fill_viridis_c(option = "C")
}

# Function for spaghetti plots
eps_spaghetti_plot <- function(plot_data, line_colour, line_size, smooth_line, ...) {
  gg = ggplot2::ggplot(dplyr::arrange(plot_data, .data$x), ggplot2::aes(.data$x, .data$forecast, group = .data$member))
  if (smooth_line) {
    gg + geom_linespline(colour = line_colour, size = line_size)
  } else {
    gg + ggplot2::geom_line(colour = line_colour, size = line_size)
  }
}

# Standard aesthetics for most ggplot2 geoms
std_aes <- function() {
  c("alpha", "colour", "fill", "linetype", "shape", "size", "stroke", "weight")
}

