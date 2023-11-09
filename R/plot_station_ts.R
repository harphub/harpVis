#' Plot a time series for point data
#'
#' This function plots a time series of data from a `harp_point_df` data frame,
#' or a `harp_list` containing `harp_point_df` data frames. The plotting is done
#' using \href{https://ggplot2.tidyverse.org/}{ggplot2} and thus uses some of
#' the same terminology in its arguments. Data are plotted using "geoms", and
#' the plots are divided into panels using facets. If the data contain a column
#' of observed values these may also be included in the plot, and for ensemble
#' data a best guess forecast may be derived from the data.
#'
#' @section Geoms:
#'
#'   The data are plotted using geoms from ggplot2. You can control which geom
#'   is used for each aspect of the plot using the respective arguments:
#'
#'  * `fcst_geom` for forecast;
#'  * `obs_geom` for observations;
#'  * `best_guess_geom` for the "best guess" forecast from an ensemble.
#'
#'   The geom should be specified as a character string based on the geom
#'   function from ggplot2 with the "geom_" prefix removed. For example, for a
#'   line plot for forecast data use `fcst_geom = "line"`. Other arguments to
#'   the geom function can be provided as named list to the appropriate
#'   `*_geom_args` argument to control, for example, the colour or size of the
#'   geom.
#'
#'   Note that aesthetic mappings to the geom cannot be controlled, except for
#'   \strong{x} via the `x_axis` argument and \strong{colour} and \strong{fill}
#'   via the `colour_by` argument.
#'
#' @section Ensembles:
#'
#'   For ensemble forecasts, some data manipulation is done prior to plotting
#'   depending on the geom as listed below. For geoms not included below, no
#'   manipulation is done and the plots may be difficult to interpret or not
#'   work at all.
#'
#'   * \strong{boxplot} - The data are grouped by the `x_axis` argument such
#'   that each box is representative of the ensemble distribution for each point
#'   on the x-axis. See \code{\link[ggplot2]{geom_boxplot}} for how the hinges,
#'   whiskers and outliers are defined.
#'   * \strong{violin} - The data are grouped by the `x_axis` argument such
#'   that each "violin" is representative of the ensemble distribution for each
#'   point on the x-axis.
#'   * \strong{line} - The data are grouped by each ensemble member and a time
#'   series is plotted for each member. This is the common ensemble "spaghetti"
#'   plot.
#'   * \strong{ribbon} - The data are divided into bands depending on the
#'   quantiles provided in the `quantiles` argument. The bands are centred such
#'   that the the outer band is between the lowest and highest quantiles with
#'   inner bands added until the innermost pair of quantiles is reached.
#'   Consequently an even number of quantiles must be provided.
#'   * \strong{col} - The data are divided into bands of increasing quantile
#'   pairs starting provided in the `quantiles` argument, with the minimum value
#'   (quantile = 0). This gives columns of stacked probabilities staring at 0
#'   that is particularly useful for accumulated variables such as
#'   precipitation, or variables that truncate at 0 such as wind speed or cloud
#'   or cloud cover.
#'
#'   A "best_guess" forecast can be added to the plot using the `best_guess`
#'   argument. This can either be the name of a function that reduces the
#'   ensemble to a single value (e.g. "mean", "median"), or the ensemble member
#'   to treat as the best guess (e.g. 0, or "mbr000"). The geom and its options
#'   can be provided via the `best_guess_geom` and `best_guess_geom_args`
#'   arguments, but they must be geoms that understand the \strong{x} and
#'   \strong{y} aesthetics.
#'
#' @section Filtering and faceting:
#'
#'   By default, all of the data that the function is given are plotted. In many
#'   cases this can result in overplotting. For data at more than one station
#'   (SID), or for more than one forecast start time (fcst_dttm), there are
#'   arguments to filter the data prior to plotting based on those values.
#'   Otherwise filtering should be done (e.g. using \code{\link[dplyr]{filter}})
#'   prior to passing the data to this function.
#'
#'   Alternatively multi-panel plots can be made using the `facet_by` argument.
#'   This groups the data based on the values in the columns provided to
#'   `facet_by` and plots each group of data in its own panel. The default
#'   behaviour is to plot 1 column of panels so that the x axis lines up for all
#'   panels, but this can be changed using the `num_facet_cols` argument.
#'   Additionally, the scale of the y axis for each panel is determined by the
#'   data for that panel, but can be common for all panels by setting
#'   `facet_scales = "fixed"`.
#'
#' @section Observations:
#'
#'   If the data include an observations column (e.g. from running
#'   \code{\link[harpCore]{join_to_fcst}}), these observations can be included
#'   in the plot be providing the name of the column that contains the
#'   observations via the `obs_col` argument. The geom and its options can be
#'   provided via the `obs_geom` and `obs_geom_args` arguments, but they must be
#'   geoms that understand the \strong{x} and \strong{y} aesthetics.
#'
#' @param .data A `harp_point_df` data frame, or a `harp_list` containing
#'   `harp_point_df` data frames.
#' @param SID The ID of the station(s) to plot. If more than one SID is asked
#'   for then `SID` should be included in `facet_by`.
#' @param fcst_dttm The start time(s) of the the forecast to plot. If more than
#'   one `fcst_dttm` is asked for, `fcst_dttm` should be included in `facet_by`.
#' @param x_axis The x axis of the plot.
#' @param fcst_geom The geom to use to plot the forecast data (see details).
#' @param fcst_geom_args Arguments to the `fcst_geom` geom as a named list.
#' @param fcst_colour_by Which column in `.data` to use to control the colours
#'   of the forecast data.
#' @param fcst_colours A vector of colours to use for the forecast data. It
#'   should be the same length as the number of colours to appear in the plot.
#'   Where the colours are a controlled by the data, this can be a named vector
#'   or a data frame with column names equal to the column in the data
#'   controlling the colour and "colour".
#' @param obs_col If observations are to be included in the plot, the column
#'   containing the observations data.
#' @param obs_geom The geom to use to plot the observations data.
#' @param obs_geom_args Arguments to the `obs_geom` geom as a named list.
#' @param facet_by The column(s) to use to facet the plot into panels.
#' @param num_facet_cols The number of columns in a faceted plot.
#' @param facet_scales Whether the scales should be fixed. Defaults to
#'   `"free_y"`. See \code{\link[ggplot2]{facet_wrap}} for more details.
#' @param smooth For line and ribbon plots, whether to smooth the lines by
#'   drawing an X-spline relative to control points. In the background
#'   \code{\link{geom_linespline}} and \code{\link{geom_ribbonspline}} are used.
#' @param ... Other arguments passed to methods.
#'
#' @return A ggplot object that can be saved with \code{\link[ggplot2]{ggasve}}.
#' @export
plot_station_ts <- function(
  .data,
  SID,
  fcst_dttm,
  x_axis         = "lead_time",
  fcst_geom      = "line",
  fcst_geom_args = list(),
  fcst_colour_by = NULL,
  fcst_colours   = NULL,
  obs_col        = NULL,
  obs_geom       = "point",
  obs_geom_args  = list(),
  facet_by       = NULL,
  num_facet_cols = 1,
  facet_scales   = "free_y",
  smooth         = FALSE,
  ...
) {
  UseMethod("plot_station_ts")
}

#' @export
plot_station_ts.harp_det_point_df <- function(
  .data,
  SID,
  fcst_dttm,
  x_axis         = "lead_time",
  fcst_geom      = "line",
  fcst_geom_args = list(),
  fcst_colour_by = NULL,
  fcst_colours   = NULL,
  obs_col        = NULL,
  obs_geom       = "point",
  obs_geom_args  = list(),
  facet_by       = NULL,
  num_facet_cols = 1,
  facet_scales   = "free_y",
  smooth         = FALSE,
  ...
) {

  det_plot(
    filter_ts(.data, SID, fcst_dttm, facet_by),
    {{x_axis}},
    {{fcst_colour_by}},
    fcst_colours,
    fcst_geom,
    fcst_geom_args,
    facet_by,
    num_facet_cols,
    facet_scales,
    {{obs_col}},
    obs_geom,
    obs_geom_args,
    smooth
  )

}

#' @param quantiles For `geom = "ribbon"`, or `geom = "col"`, the quantiles
#'   used to stratify the probabilities of an ensemble forecast.
#' @param best_guess What to plot as a "best guess" forecast. Can be any
#'   function as a character string that reduces a vector to a single value. Can
#'   also be an ensemble member as a numeric value or a string that is the same
#'   as the member in a `harp_ens_point_df` data frame that has had the
#'   members pivoted using \code{\link[harpCore]{pivot_members}}, e.g.
#'   `"mbr000"`.
#' @param best_guess_geom The geom to use to plot the best guess forecast.
#' @param best_guess_geom_args Arguments to `best_guess_geom` as a named list.
#'
#' @rdname plot_station_ts
#' @export
plot_station_ts.harp_ens_point_df <- function(
  .data,
  SID,
  fcst_dttm,
  x_axis               = "lead_time",
  fcst_geom            = "boxplot",
  fcst_geom_args       = list(),
  fcst_colour_by       = NULL,
  fcst_colours         = NULL,
  obs_col              = NULL,
  obs_geom             = "point",
  obs_geom_args        = list(),
  facet_by             = NULL,
  num_facet_cols       = 1,
  facet_scales         = "free_y",
  smooth               = FALSE,
  quantiles            = NULL,
  best_guess           = NULL,
  best_guess_geom      = "line",
  best_guess_geom_args = list(),
  ...
) {

  plot_station_ts(
    harpCore::pivot_members(.data),
    SID,
    fcst_dttm,
    {{x_axis}},
    fcst_geom,
    fcst_geom_args,
    {{fcst_colour_by}},
    fcst_colours,
    {{obs_col}},
    obs_geom,
    obs_geom_args,
    facet_by,
    num_facet_cols,
    facet_scales,
    smooth,
    quantiles,
    best_guess,
    best_guess_geom,
    best_guess_geom_args
  )

}

#' @export
plot_station_ts.harp_ens_point_df_long <- function(
  .data,
  SID,
  fcst_dttm,
  x_axis               = "lead_time",
  fcst_geom            = "boxplot",
  fcst_geom_args       = list(),
  fcst_colour_by       = NULL,
  fcst_colours         = NULL,
  obs_col              = NULL,
  obs_geom             = "point",
  obs_geom_args        = list(),
  facet_by             = NULL,
  num_facet_cols       = 1,
  facet_scales         = "free_y",
  smooth               = FALSE,
  quantiles            = NULL,
  best_guess           = NULL,
  best_guess_geom      = "line",
  best_guess_geom_args = list(),
  ...
) {

  .data <- filter_ts(.data, SID, fcst_dttm, facet_by)
  gg <- switch(
    fcst_geom,
    "boxplot" = ,
    "violin"  = ens_plot_dist(
      .data, {{x_axis}}, fcst_geom, fcst_geom_args,
      {{fcst_colour_by}}, fcst_colours
    ),
    "line" = ens_plot_spag(
      .data, {{x_axis}}, fcst_geom, fcst_geom_args,
      {{fcst_colour_by}}, fcst_colours, smooth
    ),
    "ribbon" = ens_plot_plume(
      .data, {{x_axis}}, fcst_geom, fcst_geom_args,
      fcst_colours, smooth, quantiles, facet_by
    ),
    "col" = ens_plot_stacked_prob(
      .data, {{x_axis}}, fcst_geom, fcst_geom_args,
      fcst_colours, smooth, quantiles, facet_by
    ),
    ens_plot_generic(
      .data, {{x_axis}}, fcst_geom, fcst_geom_args,
      {{fcst_colour_by}}, fcst_colours
    )
  )

  warn_level <- options()$warn
  options(warn = -1)

  if (is.null(facet_by)) {
    facet_vars <- NULL
  } else {
    facet_vars <- purrr::map_chr(rlang::eval_tidy(facet_by), rlang::quo_name)
  }

  x_axis     <- rlang::ensym(x_axis)
  group_vars <- c(rlang::quo_name(x_axis), facet_vars)


  gg <- add_best_guess(
    gg, .data, group_vars, !!x_axis, smooth,
    best_guess, best_guess_geom, best_guess_geom_args
  )

  gg <- add_obs(
    gg, .data, !!x_axis, {{obs_col}}, obs_geom, obs_geom_args, smooth
  )
  options(warn = warn_level)

  if (check_facets(facet_by)) {
    gg <- gg + facet_wrap(
      facet_by, ncol = num_facet_cols, scales = facet_scales
    )
  }

  if (is.element("units", colnames(.data))) {
    gg <- gg + ggplot2::labs(y = unique(.data[["units"]]))
  }

  gg + theme_harp_light()
}

#' @export
plot_station_ts.harp_list <- function(
  .data,
  SID,
  fcst_dttm,
  x_axis         = "lead_time",
  fcst_geom      = "line",
  fcst_geom_args = list(),
  fcst_colour_by = NULL,
  fcst_colours   = NULL,
  obs_col        = NULL,
  obs_geom       = "point",
  obs_geom_args  = list(),
  facet_by       = NULL,
  num_facet_cols = 1,
  facet_scales   = "free_y",
  smooth         = FALSE,
  ...
) {
  plot_station_ts(
    harpCore::bind(.data),
    SID,
    fcst_dttm,
    {{x_axis}},
    fcst_geom,
    fcst_geom_args,
    {{fcst_colour_by}},
    fcst_colours,
    {{obs_col}},
    obs_geom,
    obs_geom_args,
    facet_by,
    num_facet_cols,
    facet_scales,
    smooth,
    ...
  )
}

filter_ts <- function(
  in_data,
  SID,
  fcst_dttm,
  facet_by
) {

  if (missing(SID)) {
    SID <- unique(in_data[["SID"]])
  }
  .SID <- SID

  if (missing(fcst_dttm)) {
    fcst_dttm <- harpCore::as_YMDhms(unique(in_data[["fcst_dttm"]]))
  }
  .data_dttm <-  harpCore::as_dttm(fcst_dttm)

  num_sid        <- length(.SID)
  num_fcst_dttm  <- length(.data_dttm)
  num_fcst_model <- length(unique(in_data[["fcst_model"]]))

  if (is.null(facet_by)) {
    facet_vars <- NULL
  } else {
    facet_vars <- purrr::map_chr(rlang::eval_tidy(facet_by), rlang::quo_name)
  }

  missing_facets <- setdiff(
    c("fcst_model", "fcst_dttm", "SID")[
      c(num_fcst_model > 1, num_fcst_dttm > 1, num_sid > 1)
    ],
    facet_vars
  )

  if (length(missing_facets) > 0) {
    vars  <- glue::glue_collapse(missing_facets, sep = ", ", last = " & ")
    fc_by <- paste0(
      "facet_by = vars(", paste(missing_facets, collapse = ", "), ")"
    )
    cli::cli_warn(c(
      "More than one {vars} detected after filtering.",
      "i" = "Use {.var {fc_by}} to plot in seperate panels."
    ))
  }

  dplyr::filter(
    in_data,
    .data[["SID"]] %in% .SID,
    .data[["fcst_dttm"]] %in% .data_dttm
  )

}

det_plot <- function(
  plot_data,
  x_axis,
  fcst_colour_by,
  fcst_colours,
  fcst_geom,
  fcst_aes,
  facet_by,
  num_facet_cols,
  facet_scales,
  obs_col,
  obs_geom,
  obs_aes,
  smooth
) {

  x_axis <- rlang::ensym(x_axis)

  if (!arg_is_null({{fcst_colour_by}})) {
    colour_by <- rlang::ensym(fcst_colour_by)
    plot_data <- dplyr::mutate(
      plot_data,
      dplyr::across(!!colour_by, as.character)
    )
  }

  faceting <- check_facets(facet_by, rlang::caller_env())

  if (fcst_geom %in% c("line", "ribbon") && smooth) {
    fcst_geom <- paste0(fcst_geom, "spline")
  }

  geom <- check_geom(fcst_geom, "fcst_geom", rlang::caller_env())

  gg <- ggplot2::ggplot(plot_data, ggplot2::aes(x = !!x_axis))
  if (arg_is_null({{fcst_colour_by}})) {
    gg <- gg + do.call(
      geom, c(list(mapping = ggplot2::aes(y = .data[["fcst"]])), fcst_aes)
    )
  } else {
    gg <- gg + do.call(
      geom,
      c(
        list(
          mapping = ggplot2::aes(
            y = .data[["fcst"]], colour = !!colour_by, fill = !!colour_by
          )
        ),
        fcst_aes
      )
    )
  }

  warn_level <- options()$warn
  options(warn = -1)
  gg <- add_obs(
    gg, plot_data, !!x_axis, {{obs_col}}, obs_geom, obs_aes, smooth
  )
  options(warn = warn_level)

  if (faceting) {
    gg <- gg + ggplot2::facet_wrap(
      facet_by, ncol = num_facet_cols, scales = facet_scales
    )
  }

  if (is.element("units", colnames(plot_data))) {
    gg <- gg + ggplot2::labs(y = unique(plot_data[["units"]]))
  }

  gg + theme_harp_light()

}

ens_plot_dist <- function(
    plot_data, x_axis, geom, geom_args, colour_by, colours
) {

  x_axis <- rlang::ensym(x_axis)

  colouring = FALSE
  if (!arg_is_null({{colour_by}})) {
    colouring <- TRUE
    colour_by <- rlang::ensym(colour_by)
    plot_data <- dplyr::mutate(
      plot_data,
      dplyr::across(!!colour_by, as.character)
    )
  }

  geom <- check_geom(geom, "fcst_geom", rlang::caller_env())

  plot_data <- dplyr::mutate(plot_data, dplyr::across(!!x_axis, factor))
  if (colouring) {
    plot_data <- dplyr::mutate(plot_data, dplyr::across(!!colour_by, factor))
  }

  gg <- ggplot2::ggplot(plot_data, ggplot2::aes(!!x_axis, .data[["fcst"]]))

  if (colouring) {
    gg <- gg + do.call(
      geom, c(list(mapping = ggplot2::aes(fill = !!colour_by)), geom_args)
    )
  } else {
    gg <- gg + do.call(geom, geom_args)
  }

  gg

}

ens_plot_spag <- function(
  plot_data, x_axis, geom, geom_args, colour_by, colours, smooth
) {

  x_axis <- rlang::ensym(x_axis)

  colouring = FALSE
  if (!arg_is_null({{colour_by}})) {
    colouring <- TRUE
    colour_by <- rlang::ensym(colour_by)
    plot_data <- dplyr::mutate(
      plot_data,
      dplyr::across(!!colour_by, as.character)
    )
  }

  if (smooth) {
    geom <- paste0(geom, "spline")
  }
  geom <- check_geom(geom, "fcst_geom", rlang::caller_env())

  if (colouring) {
    plot_data <- dplyr::mutate(plot_data, dplyr::across(!!colour_by, factor))
  }

  gg <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(!!x_axis, .data[["fcst"]])
  )

  if (colouring) {
    gg <- gg + do.call(
      geom, c(
        list(mapping = ggplot2::aes(
          colour = !!colour_by, group = paste(.data[["member"]], !!colour_by)
        )),
        geom_args
      )
    )
  } else {
    gg <- gg + do.call(
      geom,
      c(list(mapping = ggplot2::aes(group = .data[["member"]])), geom_args)
    )
  }

  gg
}

ens_plot_plume <- function(
  data_in, x_axis, geom, geom_args, colours, smooth, quantiles, facet_by
) {

  if (is.null(facet_by)) {
    facet_vars <- NULL
  } else {
    facet_vars <- purrr::map_chr(rlang::eval_tidy(facet_by), rlang::quo_name)
  }

  x_axis     <- rlang::ensym(x_axis)
  group_vars <- c(rlang::quo_name(x_axis), facet_vars)

  quantiles <- check_quantiles(
    quantiles, c(0, 0.1, 0.25, 0.75, 0.9, 1), TRUE, rlang::caller_env()
  )

  plot_data <- make_quantile_df(data_in, group_vars, quantiles)

  if (smooth) {
    geom <- paste0(geom, "spline")
  }
  geom <- check_geom(geom, "fcst_geom", rlang::caller_env())

  gg <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      !!x_axis, ymin = .data[["ymin"]], ymax = .data[["ymax"]], fill = range
    )
  ) +
    do.call(geom, geom_args)

  if (is.null(colours)) {
    colours <- RColorBrewer::brewer.pal(length(quantiles) / 2, "Blues")
  }

  if (length(colours) != length(quantiles) / 2) {
    num_col <- length(colours)
    num_qnt <- length(quantiles)
    cli::cli_abort(c(
      "The length of {.arg colours} is not half the length of {.arg quantiles}",
      "x" = "You provided {num_col} colour{?s} for {num_qnt} quantile{?s}.",
      "i" = "You should have {num_qnt/2} colour{?s} for {num_qnt} quantile{?s}."
    ))
  }

  scale_colours(
    gg, plot_data, colours, "range", "colour_by", rlang::caller_env()
  )

}

ens_plot_stacked_prob <- function(
  data_in, x_axis, geom, geom_args, colours, smooth, quantiles, facet_by
) {

  if (is.null(facet_by)) {
    facet_vars <- NULL
  } else {
    facet_vars <- purrr::map_chr(rlang::eval_tidy(facet_by), rlang::quo_name)
  }

  x_axis     <- rlang::ensym(x_axis)
  group_vars <- c(rlang::quo_name(x_axis), facet_vars)

  quantiles <- sort(unique(quantiles))
  if (quantiles[1] != 0) {
    quantiles <- c(0, quantiles)
  }
  quantiles <- c(
    quantiles[1:(length(quantiles) - 1)],
    quantiles[length(quantiles):2]
  )

  plot_data <- make_quantile_df(data_in, group_vars, quantiles)
  plot_data <- dplyr::rename(
    dplyr::mutate(
      plot_data,
      y = dplyr::case_when(
        grepl("^00", .data[["range"]]) ~ .data[["ymax"]],
        .default = .data[["ymax"]] - .data[["ymin"]]
      )
    ),
    range_old = .data[["range"]]
  )

  plot_data <- dplyr::inner_join(
    plot_data,
    tibble::tibble(
      range_old = unique(plot_data[["range_old"]]),
      range     = rev(unique(plot_data[["range_old"]]))
    )
  )

  geom <- check_geom(geom, "fcst_geom", rlang::caller_env())

  gg <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      !!x_axis, .data[["y"]], fill = .data[["range"]]
    )
  ) +
    do.call(geom, geom_args)

  if (is.null(colours)) {
    colours <- RColorBrewer::brewer.pal(
      length(quantiles) - 1, "Blues"
    )
  }

  if (length(colours) != length(quantiles) - 1) {
    num_col <- length(colours)
    num_qnt <- length(quantiles)
    cli::cli_abort(c(
      "The length of {.arg colours} is not the length of {.arg quantiles} - 1.",
      "x" = "You provided {num_col} colour{?s} for {num_qnt} quantile{?s}.",
      "i" = "You should have {num_qnt-1} colour{?s} for {num_qnt} quantile{?s}."
    ))
  }

  scale_colours(
    gg, plot_data, colours, "range", "colour_by", rlang::caller_env()
  )

}

ens_plot_generic <- function(
    plot_data, x_axis, geom, geom_args, colour_by, colours
) {

  x_axis <- rlang::ensym(x_axis)

  colouring = FALSE
  if (!arg_is_null({{colour_by}})) {
    colouring <- TRUE
    colour_by <- rlang::ensym(colour_by)
    plot_data <- dplyr::mutate(
      plot_data,
      dplyr::across(!!colour_by, as.character)
    )
  }

  geom_in <- geom
  geom    <- check_geom(geom, "fcst_geom", rlang::caller_env())

  poss_geoms <- glue::glue_collapse(
    c("\"boxplot\"", "\"violin\"", "\"line\"", "\"ribbon\"", "\"col\""),
    sep = ", ", last = " or "
  )
  cli::cli_warn(c(
    "No specific data manipulation is done for {.var fcst_geom = \"{geom_in}\"}",
    "Results may be unpredicatable.",
    "i" = "Use {poss_geoms} for {.arg fcst_geom} for predicatble results."
  ))

  if (colouring) {
    plot_data <- dplyr::mutate(plot_data, dplyr::across(!!colour_by, factor))
  }

  gg <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(!!x_axis, .data[["fcst"]])
  )

  if (colouring) {
    gg <- gg + do.call(
      geom, c(list(mapping = ggplot2::aes(colour = !!colour_by)), geom_args)
    )
  } else {
    gg <- gg + do.call(geom, geom_args)
  }

  gg
}



add_best_guess <- function(
  gg, data_in, group_vars, x_axis, smooth,
  best_guess, best_guess_geom, best_guess_geom_args
) {

  if (is.null(best_guess)) {
    return(gg)
  }

  x_axis <- rlang::ensym(x_axis)

  if (is_fun(best_guess)) {
    func <- match.fun(best_guess)
    plot_data <- dplyr::summarise(
      data_in, best_guess = best_guess, y = func(.data[["fcst"]]),
      .by = group_vars
    )
  } else {
    if (is.numeric(best_guess)) {
      best_guess <- paste0("mbr", formatC(best_guess, width = 3, flag = "0"))
    }
    plot_data <- dplyr::mutate(
      dplyr::filter(data_in, .data[["member"]] == best_guess),
      best_guess = .data[["member"]], y = .data[["fcst"]]
    )
  }

  if (smooth && best_guess_geom %in% c("line", "ribbon")) {
    best_guess_geom <- paste0(best_guess_geom, "spline")
  }

  best_guess_geom <- check_geom(
    best_guess_geom, "best_guess_geom", rlang::caller_env()
  )

  if (is.element("colour", names(best_guess_geom_args))) {
    gg <- gg + ggnewscale::new_scale_colour()
  }

  gg <- gg +
    do.call(
      best_guess_geom,
      c(
        list(
          mapping = ggplot2::aes(
            x      = !!x_axis,
            y      = .data[["y"]],
            colour = .data[["best_guess"]]
          ),
          data = plot_data,
          inherit.aes = FALSE
        ),
        best_guess_geom_args[names(best_guess_geom_args) != "colour"]
      )
    )

  if (is.element("colour", names(best_guess_geom_args))) {
    gg <- gg +
      ggplot2::scale_colour_manual(values = best_guess_geom_args[["colour"]])
  }

  gg
}

add_obs <- function(gg, plot_data, x_axis, obs_col, obs_geom, obs_aes, smooth) {

  if (arg_is_null({{obs_col}})) {
    return(gg)
  }

  if (obs_geom %in% c("line", "ribbon") && smooth) {
    obs_geom <- paste0(obs_geom, "spline")
  }
  geom <- check_geom(obs_geom, "obs_geom", rlang::caller_env())
  x_axis  <- rlang::ensym(x_axis)
  obs_col <- rlang::ensym(obs_col)

  gg <- gg +
    ggnewscale::new_scale_colour() +
    ggnewscale::new_scale_fill()

  plot_data[["observed"]] <- rlang::quo_name(obs_col)

  gg <- gg +
    do.call(
      geom,
      c(
        list(
          mapping = ggplot2::aes(
            !!x_axis, !!obs_col, colour = .data[["observed"]],
            fill = .data[["observed"]]
          ),
          data = plot_data, inherit.aes = FALSE
        ),
        obs_aes[!names(obs_aes) %in% c("colour", "fill")]
      )
    )

  if (is.element("colour", names(obs_aes))) {
    gg <- gg + ggplot2::scale_colour_manual(values = obs_aes[["colour"]])
  }

  if (is.element("fill", names(obs_aes))) {
    gg <- gg + ggplot2::scale_fill_manual(values = obs_aes[["fill"]])
  }

  gg

}

check_facets <- function(facet_expr, caller) {

  if (!rlang::is_quosures(facet_expr)) {
    if (is.null(facet_expr)) {
      return(FALSE)
    } else {
      cli::cli_abort(c(
        "Invalid value for {.arg facet_by}",
        "i" = "{.arg facet_by} must be unquoted and wrapped in {.var vars()}",
        "i" = "e.g. facet_by = vars(fcst_model)"
      ), call = caller)
    }
  }

  TRUE

}

check_geom <- function(geom, arg, caller) {
  geom_in <- geom
  geom    <- strsplit(sub("geom_", "", geom), "::")[[1]]
  if (length(geom) == 1 && geom %in% c("linespline", "ribbonspline")) {
    return(get(paste0("geom_", geom), mode = "function"))
  }
  if (length(geom) == 1) {
    pkg  <- "ggplot2"
    geom <- paste0("geom_", geom)
  } else {
    pkg  <- geom[1]
    geom <- paste0("geom_", geom[2])
  }
  geom_out <- try(
    get(geom, mode = "function", envir = asNamespace(pkg)), silent = TRUE
  )

  if (inherits(geom_out, "try-error")) {
    cli::cli_abort(c(
      "{.arg {arg}} not found.",
      "x" = "{.var {geom}} does not exist for package {.var {pkg}}."
    ))
  }

  geom_out
}


scale_colours <- function(gg, plot_data, colours, colour_by, arg, caller) {

  if (is.null(colours)) {
    return(gg)
  }

  colour_by <- rlang::ensym(colour_by)

  if (!is.null(colours)) {
    if (is.data.frame(colours)) {
      if (is.null(colour_by)) {
        cli::cli_abort(c(
          "{.arg colour_by} is missing with no default.",
          "i" = paste(
            "When passing {.arg colours} as a data frame {.arg {arg}}",
            " must be provided."
          )
        ), call = caller)
      }
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
        ), call = caller)
      }
      colours <- colours[["colour"]]
      names(colours) <- named
    }

    gg +
      ggplot2::scale_colour_discrete(type = colours) +
      ggplot2::scale_fill_discrete(type = colours)

  }
}

arg_is_null <- function(arg) {
  rlang::quo_is_null(rlang::enquo(arg))
}

check_quantiles <- function(x, default, check_even, caller) {
  if (is.null(x)) {
    cli::cli_inform(
      cli::col_cyan(
        "Using default quantiles: {default}"
      )
    )
    x <- default
  }
  if (check_even && length(x) %% 2 != 0) {
    num_quantiles <- length(x)
    cli::cli_abort(c(
      "Wrong number of quantiles.",
      "x" = "You provided {num_quantiles} quantiles.",
      "i" = "There must be an even number of quantiles."
    ), call = caller)
  }
  sort(x)
}

make_quantile_df <- function(.data, grps, quantiles) {

  qlist <- lapply(
    (1:(length(quantiles) / 2)),
    function(i) quantiles[c(i, length(quantiles) - i + 1)]
  )

  lapply(
    qlist,
    function(q) {
      dplyr::summarise(
        .data,
        q = dplyr::mutate(
          stats::setNames(
            as.data.frame(t(quantile(.data[["fcst"]], q))), c("ymin", "ymax")
          ),
          range = paste0(
            paste(formatC(q * 100, width = 2, flag = "0"), collapse = "-"), "%"
          )
        ),
        .by = dplyr::any_of(grps)
      ) %>%
      tidyr::unnest("q")
    }
  ) %>%
    dplyr::bind_rows()

}

is_fun <- function(x) {
  res <- try(match.fun(x), silent = TRUE)
  if (inherits(res, "try-error")) {
    return(FALSE)
  }
  TRUE
}
