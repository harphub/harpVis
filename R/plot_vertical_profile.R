#' Title
#'
#' @param .fcst
#' @param SID
#' @param fcdate
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
  fcdate,
  lead_time,
  y_axis         = p,
  skew_t         = FALSE,
  colour_by      = parameter,
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

  fcdate_unix <- harpIO::str_datetime_to_unixtime(fcdate)

  plot_data <- purrr::map(
    .fcst,
    dplyr::filter,
    .data$SID      %in% SID_in,
    .data$fcdate   %in% fcdate_unix,
    .data$leadtime %in% lead_time
  )

  if (all(purrr::map_int(plot_data, nrow)) < 1) {
    stop("No data found for SID = ", SID, ", fcdate = ", fcdate, ", lead_time = ", lead_time, call. = FALSE)
  }

  plot_data <- dplyr::bind_rows(plot_data, .id = "mname")

  if (any(grepl("_mbr[[:digit:]]+$", colnames(plot_data)))) {
    plot_data <- plot_data %>%
      harpPoint::gather_members()
  } else if (any(grepl("_det+$"))) {
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
