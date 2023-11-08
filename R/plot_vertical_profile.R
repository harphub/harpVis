#' Plot a vertical profile
#'
#' Plots a vertical profile of data read in by a `harp_read_<>` function. There
#' are arguments for filtering the data to, for example station (SID), forecast
#' start time (fcst_dttm), lead time (lead_time), or valid time (valid_dttm) to
#' ensure that only the wanted data are plotted. There is also the option to
#' plot temperature (and / or) dewpoint temperature profiles on a skew-T / log -
#' P diagram.
#'
#' @param .data A data frame, a `harp_point_df` data frame or a `harp_list`.
#' @param SID The station ID (if available) in `.data` to plot
#' @param y_axis The column in `.data` containing the data to be used as the
#'   y-axis - i.e. the vertical position.
#' @param skew_t Logical. Whether to plot the profile on a skew-T /log-P
#'   diagram.
#' @param facet_by The column in `.data` to facet by.
#' @param colour_by The column in `.data` that will determine the line colour
#' @param colours A character vector of colour codes. If this is a named vector,
#'   then the colour values will be matched to levels based on the names of the
#'   vectors. Can also be a data frame with column names equal to the value for
#'   `colour_by` and `colour`.
#' @param reverse Logical - whether the y-axis should be reversed.
#' @param log_scale Logical - whether the y-axis should use a log10 scale.
#' @param ... Other arguments passed on to layer(). These are often aesthetics,
#'   used to set an aesthetic to a fixed value, like linwidth = 4, or linetype =
#'   3.
#'
#' @return A `ggplot` plot that can be saved with \code{\link[ggplot2]{ggsave}}.
#' @export
plot_vertical_profile <- function(
  .data,
  SID,
  y_axis         = "p",
  reverse        = TRUE,
  log_scale      = FALSE,
  skew_t         = FALSE,
  colour_by      = "fcst_model",
  colours        = NULL,
  facet_by       = NULL,
  num_facet_cols = NULL,
  ...
) {
  UseMethod("plot_vertical_profile")
}

#' @rdname plot_vertical_profile
#' @param fcst_dttm The forecast start time in `.data` to plot.
#' @param lead_time The lead_time in `data` to plot.
#' @export
plot_vertical_profile.harp_ens_point_df <- function(
  .data,
  SID,
  y_axis         = "p",
  reverse        = TRUE,
  log_scale      = FALSE,
  skew_t         = FALSE,
  colour_by      = "fcst_model",
  colours        = NULL,
  facet_by       = NULL,
  num_facet_cols = NULL,
  fcst_dttm,
  lead_time,
  ...
) {
  y_axis    <- rlang::ensym(y_axis)
  colour_by <- rlang::ensym(colour_by)
  plot_vertical_profile(
    harpCore::pivot_members(.data), SID, !!y_axis, reverse, log_scale, skew_t,
    !!colour_by, colours, facet_by, num_facet_cols, fcst_dttm, lead_time, ...
  )
}

#' @rdname plot_vertical_profile
#' @export
plot_vertical_profile.harp_list <- function(
  .data,
  SID,
  y_axis         = "p",
  reverse        = TRUE,
  log_scale      = FALSE,
  skew_t         = FALSE,
  colour_by      = "fcst_model",
  colours        = NULL,
  facet_by       = NULL,
  num_facet_cols = NULL,
  fcst_dttm,
  lead_time,
  ...
) {
  y_axis    <- rlang::ensym(y_axis)
  colour_by <- rlang::ensym(colour_by)
  plot_vertical_profile(
    harpCore::bind(.data), SID, !!y_axis, reverse, log_scale, skew_t,
    !!colour_by, colours, facet_by, num_facet_cols, fcst_dttm, lead_time, ...
  )
}

plt_vrt_prf_other_classes <- function(
  .data,
  SID,
  y_axis         = "p",
  reverse        = TRUE,
  log_scale      = FALSE,
  skew_t         = FALSE,
  colour_by      = "fcst_model",
  colours        = NULL,
  facet_by       = NULL,
  num_facet_cols = NULL,
  fcst_dttm,
  lead_time,
  ...
) {
  y_axis    <- rlang::ensym(y_axis)
  colour_by <- rlang::ensym(colour_by)
  prep_plot(
    .data, SID, fcst_dttm, lead_time, !!y_axis, reverse, log_scale,
    skew_t, !!colour_by, colours, facet_by, num_facet_cols, ...
  )
}

#' @export
plot_vertical_profile.harp_ens_point_df_long <- plt_vrt_prf_other_classes

#' @rdname plot_vertical_profile
#' @export
plot_vertical_profile.harp_det_point_df      <- plt_vrt_prf_other_classes

#' @rdname plot_vertical_profile
#' @param data_col The column in `.data` that contains the data to plot as a
#'   vertical profile.
#' @param valid_dttm If a `valid_dttm` column exists in `.data`, the valid time
#'   of the data to plot.
#' @export
plot_vertical_profile.data.frame <- function(
  .data,
  SID,
  y_axis         = "p",
  reverse        = TRUE,
  log_scale      = FALSE,
  skew_t         = FALSE,
  colour_by      = NULL,
  colours        = NULL,
  facet_by       = NULL,
  num_facet_cols = NULL,
  data_col,
  valid_dttm,
  ...
) {

  if ("SID" %in% colnames(.data)) {
    if (missing(SID)) {
      SID <- unique(.data[["SID"]])
    }
    .SID  <- SID
    .data <- dplyr::filter(.data, .data[["SID"]] %in% .SID)
    if (nrow(.data) < 1) {
      cli::cli_abort(c(
        "No data found to plot",
        "i" = "You've asked for...",
        "i" = "SID       = {(.SID)}."
      ))
    }
  }

  if ("valid_dttm" %in% colnames(.data)) {
    if (missing(valid_dttm)) {
      valid_dttm <- harpCore::as_YMDhms(unique(.data[["valid_dttm"]]))
    }
    .data_dttm <-  harpCore::as_dttm(valid_dttm)
    .data      <- dplyr::filter(.data, .data[["valid_dttm"]] %in% .data_dttm)
    if (nrow(.data) < 1) {
      err_message <- c(
        "No data found to plot",
        "i" = "You've asked for..."
      )
      if (!missing(SID)) {
        err_message <- c(err_message, c("i" = "SID       = {(.SID)}, "))
      }
      cli::cli_abort(c(
        err_message,
        c("i" = "valid_dttm = {(.data_dttm)}.")
      ))
    }
  }

  y_axis    <- rlang::ensym(y_axis)
  col_name  <- rlang::quo_name(rlang::ensym(data_col))
  if (is.null(colour_by)) {
    colour_by <- rlang::sym(".no_colours")
  } else {
    colour_by <- rlang::ensym(colour_by)
  }

  plot_func(
    .data, col_name, !!y_axis, reverse, log_scale, !!colour_by, colours, skew_t,
    facet_by, num_facet_cols, ...
  )

}

prep_plot <- function(
  .data,
  SID,
  fcst_dttm,
  lead_time,
  y_axis         = "p",
  reverse        = TRUE,
  log_scale      = FALSE,
  skew_t         = FALSE,
  colour_by      = "fcst_model",
  colours        = NULL,
  facet_by       = NULL,
  num_facet_cols = NULL,
  ...
) {

  colour_by <- rlang::ensym(colour_by)
  y_axis    <- rlang::ensym(y_axis)

  lt_col <- intersect(
    c("lead_time", "leadtime"),
    colnames(.data)
  )

  if (missing(SID)) {
    SID <- unique(.data[["SID"]])
  }
  .SID <- SID

  if (missing(fcst_dttm)) {
    fcst_dttm <- harpCore::as_YMDhms(unique(.data[["fcst_dttm"]]))
  }
  .data_dttm <-  harpCore::as_dttm(fcst_dttm)

  if (missing(lead_time)) {
    lead_time <- unique(.data[["lead_time"]])
  }
  .lt <- lead_time

  plot_data <- dplyr::filter(
    .data,
    .data[["SID"]]       %in% .SID,
    .data[["fcst_dttm"]] %in% .data_dttm,
    .data[[lt_col]]      %in% .lt
  )

  if (nrow(plot_data) < 1) {
    cli::cli_abort(c(
      "No data found to plot",
      "i" = "You've asked for...",
      "i" = "SID       = {.SID}, ",
      "i" = "fcst_dttm = {.data_dttm}",
      "i" = "lead_time = {.lt}"
    ))
  }

  plot_func(
    plot_data, "fcst", !!y_axis, reverse, log_scale, !!colour_by, colours,
    skew_t, facet_by, num_facet_cols, ...
  )

}

plot_func <- function(
  plot_data, data_col, y_axis, reverse, log_scale, colour_by, colours, skew_t,
  facet_by, num_facet_cols, ...
) {

  y_axis     <- rlang::ensym(y_axis)
  colour_by  <- rlang::ensym(colour_by)
  colour_aes <- rlang::quo_name(colour_by) != ".no_colours"

  if (!rlang::is_quosures(facet_by)) {
    if (is.null(facet_by)) {
      faceting <- FALSE
    } else {
      cli::cli_abort(c(
        "Invalid value for {.arg facet_by}",
        "i" = "{.arg facet_by} must be unquoted and wrapped in {.var vars}",
        "i" = "e.g. facet_by = vars(parameter)"
      ), call = rlang::caller_env())
    }
  } else {
    faceting <- TRUE
  }

  if (skew_t) {

    if ("units" %in% colnames(plot_data) && all(plot_data[["units"]] == "K")) {
      cli::cli_inform(c(
        "!" = cli::col_red("{.arg .data} has data in Kelvin."),
        "i" = "Scaling to degrees C."
      ))
      if (data_col == "fcst") {
        plot_data <- harpCore::scale_param(plot_data, -273.15, "degC")
      } else {
        plot_data <- harpCore::scale_param(
          plot_data, -273.15, "degC", col = {{data_col}}
        )
      }
    }

    plot_data <- dplyr::filter(plot_data, !!y_axis >= 100)

    gg <- ggsonde::ggsonde(
      plot_data, ggplot2::aes(x = .data[[data_col]], y = !!y_axis)
    )
    if (colour_aes) {
      gg <- gg + ggsonde::geom_sonde(ggplot2::aes(colour = !!colour_by))
    } else {
      gg <- gg + ggsonde::geom_sonde()
    }

    gg <- gg + ggplot2::theme(plot.margin = ggplot2::unit(rep(5.5, 4), "points"))

  } else {


    if (colour_aes) {
      gg <- ggplot2::ggplot(
        plot_data,
        ggplot2::aes(x = .data[[data_col]], y = !!y_axis, colour = !!colour_by)
      ) +
        ggplot2::geom_path(...)
    } else {
      gg <- ggplot2::ggplot(
        plot_data,
        ggplot2::aes(x = .data[[data_col]], y = !!y_axis)
      )
      if (!is.null(colours) && !is.data.frame(colours)) {
        gg <- gg + geom_path(colour = colours, ...)
      } else {
        gg <- gg + geom_path(...)
      }
    }

    y_scale <- c("log10", "reverse")[c(log_scale, reverse)]
    if (length(y_scale) > 0) {
      gg <- gg + ggplot2::scale_y_continuous(
        trans  = y_scale,
        breaks = unique(dplyr::pull(plot_data, !!y_axis))
      )
    }
    gg <- gg + theme_bw()
  }


  if (faceting) {
    gg <- gg + ggplot2::facet_wrap(facet_by, ncol = num_facet_cols)
  }

  if (!is.null(colours)) {
    if (is.data.frame(colours)) {
      colours <- dplyr::mutate(
        colours, dplyr::across(dplyr::where(is.factor), as.character)
      )
      named <- dplyr::pull(colours, !!colour_by)

      data_vals <- unique(dplyr::pull(plot_data, !!colour_by))
      if (!identical(sort(named), sort(data_vals))) {
        cli::cli_abort(c(
          "Names in data frame for {.arg colours} do not match data.",
          "x" = "You supplied {named}.",
          "i" = "Data have {data_vals}."
        ))
      }
      colours <- colours[["colour"]]
      names(colours) <- named
    }
    if (colour_aes) {
      gg <- gg + ggplot2::scale_colour_discrete(type = colours)
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
