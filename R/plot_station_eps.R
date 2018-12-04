#' Title
#'
#' @param .fcst
#' @param SID
#' @param fcdate
#' @param quantiles
#' @param tz
#' @param control_member
#'
#' @return
#' @export
#'
#' @examples
plot_station_eps <- function(
  .fcst,
  SID,
  fcdate,
  quantiles       = c(0.05, 0.25, 0.75, 0.95),
  tz              = "",
  control_member  = "mbr000",
  best_guess_line = ens_median,
  ribbon_colours  = NULL,
  line_colour     = "black"
) {

  # Check inputs
  SID_quo    <- rlang::enquo(SID)
  fcdate_quo <- rlang::enquo(fcdate)

  best_guess_quo  <- rlang::enquo(best_guess_line)
  best_guess_expr <- rlang::quo_get_expr(best_guess_quo)
  if (is.character(best_guess_expr)) {
    best_guess_quo <- rlang::sym(best_guess_expr)
  }
  best_guess_name <- rlang::quo_name(best_guess_quo)

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
      warning("SID = ", SID, ", fcdate = ", fcdate, " not found for forecast model: ", fcst_model, call. = FALSE, immediate. = TRUE)
    }
  }
  if (all(check_rows(plot_data) == 0)) {
    stop("No data to plot", call. = FALSE)
  }

  # Compute quantiles and prepare data for plot
  plot_data <- harpPoint::gather_members(harpPoint:::new_harp_fcst(plot_data))
  plot_data <- dplyr::bind_rows(unclass(plot_data), .id = "mname")
  plot_data <- plot_data %>%
    dplyr::group_by(.data$mname, .data$leadtime, .data$validdate) %>%
    tidyr::nest() %>%
    dplyr::transmute(
      .data$mname,
      .data$leadtime,
      validtime   = as.POSIXct(.data$validdate, origin = "1970-01-01 00:00:00", tz = tz),
      ens_mean    = purrr::map_dbl(.data$data, ~ mean(.x$forecast)),
      ens_median  = purrr::map_dbl(.data$data, ~ median(.x$forecast)),
      ens_control = purrr::map_dbl(.data$data, ~ .x$forecast[.x$member == control_member]),
      quantiles   = purrr::map(.data$data, ~ as.list(quantile(.x$forecast, quantiles)))
    ) %>%
    dplyr::mutate(quantiles = purrr::map(.data$quantiles, ~ rlang::set_names(.x, ~ paste0("q", .)))) %>%
    dplyr::mutate(quantiles = purrr::map(.data$quantiles, dplyr::bind_cols)) %>%
    tidyr::unnest()


  # Generate the ggplot object
  gg <- ggplot2::ggplot(plot_data, ggplot2::aes(x = validtime))

  for (ribbon_number in 1:num_ribbons) {
    ymin <- rlang::sym(ribbon_quantiles[[ribbon_number]][1])
    ymax <- rlang::sym(ribbon_quantiles[[ribbon_number]][2])
    gg   <- gg + geom_ribbonspline(
      ggplot2::aes(ymin = !! ymin, ymax = !! ymax),
      fill = ribbon_colours[ribbon_number])
  }

  gg <- gg + ggalt::geom_xspline(ggplot2::aes(y = !! best_guess_quo), colour = line_colour)
  gg + ggplot2::theme_bw() + ggplot2::facet_wrap("mname")

}

check_rows <- function(df) {
  purrr::map_int(df, nrow)
}
