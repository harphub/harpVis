#' Title
#'
#' @param .fcst
#' @param SID
#' @param fcst_dttm
#' @param lead_time
#' @param y_axis
#' @param skew_t
#' @param facet_by
#' @param colour_by
#' @param colours
#'
#' @return
#' @export
#'
#' @examples
plot_vertical_profile <- function(
  .fcst,
  SID,
  fcst_dttm,
  lead_time,
  y_axis         = p,
  skew_t         = FALSE,
  colour_by      = fcst_model,
  colours        = NULL,
  facet_by       = NULL,
  num_facet_cols = 2,
  ...
) {

  # Tidyeval

  if (!rlang::is_quosures(facet_by)) {
    if (is.null(facet_by)) {
      faceting <- FALSE
    } else {
      stop ("facet_by must be unquoted and wrapped in vars, e.g. facet_by = vars(parameter)", call. = FALSE)
    }
  } else {
    faceting <- TRUE
  }

  colour_by <- rlang::enquo(colour_by)

  y_axis <- rlang::enquo(y_axis)

  SID_in <- SID

  ##

  fcst_dttm_unix <- harpCore::as_unixtime(fcst_dttm)

  plot_data <- purrr::map(
    .fcst,
    dplyr::filter,
    .data$SID      %in% SID_in,
    .data$fcst_dttm   %in% fcst_dttm_unix,
    .data$leadtime %in% lead_time
  )

  if (all(purrr::map_int(plot_data, nrow)) < 1) {
    stop("No data found for SID = ", SID, ", fcst_dttm = ", fcst_dttm, ", lead_time = ", lead_time, call. = FALSE)
  }

  if (all(purrr::map_lgl(plot_data, ~any(grepl("_det+$", colnames(.x)))))) {
    plot_data <- purrr::map(
      plot_data,
      ~dplyr::rename_at(.x, dplyr::vars(dplyr::matches("_det+$")), ~ "forecast")
    )
  }

  plot_data <- dplyr::bind_rows(plot_data, .id = "fcst_model")

  if (any(grepl("_mbr[[:digit:]]+$", colnames(plot_data)))) {
    plot_data <- plot_data %>%
      harpCore::pivot_members()
  } else if (any(grepl("_det+$", colnames(plot_data)))) {
    plot_data <- plot_data %>%
      dplyr::rename_at(dplyr::vars(dplyr::matches("_det+$")), ~ "forecast")
  }

  if (skew_t) {

    gg <- ggsonde::ggsonde(plot_data, ggplot2::aes(x = .data$forecast, y = !! y_axis)) +
      ggsonde::geom_sonde(ggplot2::aes(colour = !! colour_by))

  } else {

    gg <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$forecast, y = !! y_axis, colour = !! colour_by)) +
      ggplot2::geom_path() +
      ggplot2::scale_y_reverse(breaks = unique(dplyr::pull(plot_data, !! y_axis))) +
      ggplot2::theme_bw()

    if (faceting) {
      gg <- gg + ggplot2::facet_wrap(facet_by, ncol = num_facet_cols)
    }

  }

  gg
}

reverselog_trans <- function(base = exp(1)) {
    trans <- function(x) -log(x, base)
    inv <- function(x) base^(-x)
    scales::trans_new(paste0("reverselog-", format(base)), trans, inv,
              scales::log_breaks(base = base),
              domain = c(1e-100, Inf))
}
