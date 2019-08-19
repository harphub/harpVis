plot_skew_t <- function(
  data                   = NULL,
  sfc_temperature_range  = c(-40, 50),
  pressure_grid_lines    = c(1050, 1000, 850, 700, 500, 400, 300, 250, 200, 150, 100),
  temperature_grid_lines = seq(-100, 50, 10),
  mixing_ratio_lines     = c(20, 12, 8, 5, 3, 2, 1, 0.4),
  mixing_ratio_top_p     = 500,
  dry_adiabat_lines      = seq(-30, 170, 10),
  moist_adiabat_lines    = c(32, 28, 24, 20, 16, 12, 8, 4, 1),
  moist_adiabat_top_p    = 200,
  moist_adiabat_label_p  = 400,
  text_size              = 3.5,
  label_offset           = 0.01,
  kink_temperature       = 5,
  kink_pressure          = c(400, 625),
  expand_x               = 0.04,
  expand_y               = 0.04
) {

  min_t <- min(sfc_temperature_range)
  max_t <- max(sfc_temperature_range)

  kink_pressure_min <- min(kink_pressure)
  kink_pressure_max <- max(kink_pressure)

  ### The border of the skew_t plot ###

  ymax  <- RadioSonde::skewty(max(pressure_grid_lines))
  ymin  <- RadioSonde::skewty(min(pressure_grid_lines))
  xmin  <- RadioSonde::skewtx(min_t, RadioSonde::skewty(max(pressure_grid_lines)))
  xmax  <- RadioSonde::skewtx(max_t, RadioSonde::skewty(max(pressure_grid_lines))) #1000))
  kinkx <- RadioSonde::skewtx(kink_temperature, RadioSonde::skewty(kink_pressure_min))
  kinky <- c(RadioSonde::skewty(kink_pressure_max), RadioSonde::skewty(kink_pressure_min))
  xc <- c(xmin, xmin, xmax, xmax, kinkx, kinkx, xmin)
  yc <- c(ymin, ymax, ymax, kinky, ymin, ymin)

  kink_lm <- stats::lm(kinky ~ c(xmax, kinkx))
  kink_gradient  <- -1 * kink_lm$coefficients[2]
  kink_intercept <- kink_lm$coefficients[1]

  skew_t_border <- data.frame(x = xc, y = yc)

  ### The grid lines for pressure ###

  num_pressure_levels <- length(pressure_grid_lines)

  kink_indices <- which(pressure_grid_lines > kink_pressure_min & pressure_grid_lines < kink_pressure_max)

  y_all <- RadioSonde::skewty(pressure_grid_lines)
  x_left  <- rep(xmin, num_pressure_levels)
  x_right <- rep(xmax, num_pressure_levels)
  x_right[pressure_grid_lines <= kink_pressure_min] <- kinkx
  x_right[kink_indices] <- xmax - (y_all[kink_indices] - min(kinky)) / kink_gradient

  skew_t_pressure <- data.frame(
    pressure = pressure_grid_lines,
    x        = x_left,
    y        = y_all,
    xend     = x_right,
    yend     = y_all,
    x_label  = x_left - label_offset * (xmax - xmin)
  )

  ### The grid lines for temperature ###

  num_temperature_lines <- length(temperature_grid_lines)

  # For bottom edge
  pressure_left  <- rep(max(pressure_grid_lines), num_temperature_lines)

  # For left edge temperature: < min_t
  indices <- which(temperature_grid_lines <= min_t)
  pressure_left[indices] <- pressure_from_skewtx_and_t(xmin, temperature_grid_lines[indices])

  # For top edge
  pressure_right <- rep(min(pressure_grid_lines), num_temperature_lines)

  # For right edge: temperature > temperature at (kinkx, ymax)
  kink_temperature <- mapply(t_from_skewtx_and_skewty, c(kinkx, kinkx, xmax), c(ymin, max(kinky), min(kinky)))

  # Above kink
  indices <- which(temperature_grid_lines > kink_temperature[1] & temperature_grid_lines <= kink_temperature[2])
  pressure_right[indices] <- pressure_from_skewtx_and_t(kinkx, temperature_grid_lines[indices])

  # Below kink
  indices <- which(temperature_grid_lines >= kink_temperature[3])
  pressure_right[indices] <- pressure_from_skewtx_and_t(xmax, temperature_grid_lines[indices])

  # In kink
  indices      <- which(temperature_grid_lines > kink_temperature[2] & temperature_grid_lines < kink_temperature[3])
  y_in_kink    <- RadioSonde::skewty(seq(kink_pressure_max, kink_pressure_min, -1))
  x_in_kink    <- xmax - (y_in_kink - min(kinky)) / kink_gradient
  t_in_kink    <- t_from_skewtx_and_skewty(x_in_kink, y_in_kink)
  kink_indices <- sapply(temperature_grid_lines[indices], function(x) which.min(abs(x - t_in_kink)))

  pressure_right[indices] <- mapply(
    pressure_from_skewtx_and_t,
    x_in_kink[kink_indices],
    temperature_grid_lines[indices]
  )

  y_right <- RadioSonde::skewty(pressure_right)
  x_right <- RadioSonde::skewtx(temperature_grid_lines, y_right)
  y_left  <- RadioSonde::skewty(pressure_left)
  x_left  <- RadioSonde::skewtx(temperature_grid_lines, y_left)

  inside_plot <- Reduce(
    intersect,
    list(
      which(y_right <= ceiling(ymin)),
      which(x_right <= ceiling(xmax)),
      which(y_left >= floor(ymax)),
      which(x_left >= floor(xmin))
    )
  )

  skew_t_temperature <- data.frame(
    temp = temperature_grid_lines,
    x    = x_left,
    y    = y_left,
    xend = x_right,
    yend = y_right,
    x_label = x_right + label_offset * (xmax - xmin),
    y_label = y_right + label_offset * (ymin - ymax)
  )

  skew_t_temperature <- skew_t_temperature[inside_plot, ]

  ### The saturation mixing ratios ###

  num_mixing_ratios <- length(mixing_ratio_lines)

  y_right <- RadioSonde::skewty(rep(mixing_ratio_top_p, num_mixing_ratios))
  t_mix   <- tmr(mixing_ratio_lines, mixing_ratio_top_p)
  x_right <- RadioSonde::skewtx(t_mix, y_right)
  y_left  <- RadioSonde::skewty(rep(max(pressure_grid_lines), num_mixing_ratios))
  t_mix   <- tmr(mixing_ratio_lines, max(pressure_grid_lines))
  x_left  <- RadioSonde::skewtx(t_mix, y_left)

  skew_t_mixing_ratio <- data.frame(
    mixing_ratio = mixing_ratio_lines,
    x            = x_left,
    y            = y_left,
    xend         = x_right,
    yend         = y_right,
    y_label      = y_left - label_offset * (ymin - ymax)
  )

  ### The dry adiabats ###

  num_dry_adiabat_lines <- length(dry_adiabat_lines)

  skew_t_dry_adiabat <- list()
  dry_adiabat_labels <- list()
  for (dry_adiabat in seq_along(dry_adiabat_lines)) {

    pressure <- seq(max(pressure_grid_lines), min(pressure_grid_lines), -1)
    y_values <- RadioSonde::skewty(pressure)
    x_values <- RadioSonde::skewtx(tda(dry_adiabat_lines[dry_adiabat], pressure), y_values)
    first_x  <- which.min(abs(x_values - xmin))
    last_x   <- which.min(abs(x_values - xmax))

    x_values <- x_values[first_x:last_x]
    y_values <- y_values[first_x:last_x]

    in_kink <- intersect(which(x_values > kinkx), which(y_values > min(kinky)))

    outside_plot <- which(y_values[in_kink] - (-1 * kink_gradient * x_values[in_kink] + kink_intercept) > 0)
    x_values[in_kink][outside_plot] <- NA
    y_values[in_kink][outside_plot] <- NA

    skew_t_dry_adiabat[[dry_adiabat]] <- data.frame(
      theta = dry_adiabat_lines[dry_adiabat],
      x     = x_values,
      y     = y_values
    )

    dry_adiabat_labels[[dry_adiabat]] <- data.frame(
      theta = dry_adiabat_lines[dry_adiabat],
      x = x_values[1] + label_offset * (xmax - xmin),
      y = y_values[1] - label_offset * (ymin - ymax)
    )

  }
  skew_t_dry_adiabat <- Reduce(rbind, skew_t_dry_adiabat)
  dry_adiabat_labels <- Reduce(rbind, dry_adiabat_labels)


  ### The moist adiabats ###

  num_moist_adiabat_lines <- length(moist_adiabat_lines)
  pressure                <- seq(max(pressure_grid_lines), moist_adiabat_top_p, -10)
  y_values                <- RadioSonde::skewty(pressure)

  skew_t_moist_adiabat <- list()
  for (moist_adiabat in seq_along(moist_adiabat_lines)) {
    x_values <- mapply(
      function(x, y) RadioSonde::skewtx(RadioSonde::satlft(moist_adiabat_lines[moist_adiabat], x), y),
      pressure,
      y_values
    )
    x_values[x_values < xmin] <- NA
    y_values[is.na(x_values)] <- NA

    pressure_level <- pressure
    pressure_level[is.na(x_values)] <- NA

    skew_t_moist_adiabat[[moist_adiabat]] <- data.frame(
      pseudo_temp = moist_adiabat_lines[moist_adiabat],
      pressure    = na.omit(pressure_level),
      x           = na.omit(x_values),
      y           = na.omit(y_values)
    )
  }
  skew_t_moist_adiabat <- Reduce(rbind, skew_t_moist_adiabat)

  moist_adiabat_labels <- skew_t_moist_adiabat[skew_t_moist_adiabat$pressure == moist_adiabat_label_p,]

  ### Plot limits ###

  x_range <- xmax - xmin
  y_range <- ymin - ymax

  min_x <- xmin - expand_x * x_range
  max_x <- xmax + expand_x * x_range
  min_y <- ymax - expand_y * y_range
  max_y <- ymin + expand_y * y_range

  ### Construct the plot ###

  ggplot2::ggplot(data, ggplot2::aes(.data$x, .data$y)) +
    ggplot2::geom_segment(
      data   = skew_t_pressure,
      ggplot2::aes(xend = .data$xend, yend = .data$yend),
      size   = 0.25,
      colour = "cornflowerblue"
    ) +

    ggplot2::geom_text(
      data   = skew_t_pressure,
      ggplot2::aes(x = .data$x_label, label = .data$pressure),
      size   = text_size,
      hjust  = 1,
      colour = "cornflowerblue"
    ) +

    ggplot2::geom_segment(
      data   = skew_t_temperature,
      ggplot2::aes(xend = .data$xend, yend = .data$yend),
      size   = 0.25,
      colour = "coral"
    ) +

    ggplot2::geom_text(
      data   = skew_t_temperature,
      ggplot2::aes(x = .data$x_label, y = .data$y_label, label = .data$temp),
      size   = text_size,
      hjust  = 0,
      angle  = 45,
      colour = "coral"
    ) +

    ggplot2::geom_segment(
      data     = skew_t_mixing_ratio,
      ggplot2::aes(xend = .data$xend, yend = .data$yend),
      size     = 0.25,
      colour   = "mediumorchid",
      linetype = 2
    ) +

    ggplot2::geom_text(
      data     = skew_t_mixing_ratio,
      ggplot2::aes(y = .data$y_label, label = .data$mixing_ratio),
      size     = text_size,
      vjust    = 1,
      colour   = "mediumorchid"
    ) +

    ggplot2::geom_path(
      data   = skew_t_dry_adiabat,
      ggplot2::aes(group = .data$theta),
      size   = 0.25,
      colour = "springgreen4"
    ) +

    ggplot2::geom_text(
      data    = dry_adiabat_labels,
      ggplot2::aes(label = .data$theta),
      size    = text_size,
      hjust   = 0,
      vjust   = 1,
      colour  = "springgreen4"
    ) +

    ggplot2::geom_path(
      data     = skew_t_moist_adiabat,
      ggplot2::aes(group = .data$pseudo_temp),
      size     = 0.25,
      colour   = "springgreen4",
      linetype = 2
    ) +

    ggplot2::geom_text(
      data    = moist_adiabat_labels,
      ggplot2::aes(label = .data$pseudo_temp),
      size    = text_size,
      hjust   = 0.5,
      vjust   = 0,
      nudge_y = 0.25,
      colour  = "springgreen4"
    ) +

    ggplot2::geom_path(data = skew_t_border, colour = "grey70", size = 0.5) +
    ggplot2::coord_equal(xlim = c(min_x, max_x), ylim = c(min_y, max_y)) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.margin = unit(c(7, 7, 7, 7), "points"))

}

pressure_from_skewtx_and_t <- function(x, t) {
  exponent <- (132.18199999999999 - (x - 0.54000000000000004 * t)/0.90691999999999995)/44.061
  10 ^ exponent
}

t_from_skewtx_and_skewty <- function(x, y) {
  (x - 0.90691999999999995 * y) / 0.54000000000000004
}

tmr <- function(w, p) {
  #
  # Determine x-coordinate on skew-T, log p diagram given
  # temperature (C)
  # and y-coordinate from FUNCTION SKEWTY.  X-origin at T=0c.
  #
  #            "algorithms for generating a skew-t, log p
  #            diagram and computing selected meteorological
  #            quantities."
  #            atmospheric sciences laboratory
  #            u.s. army electronics command
  #            white sands missile range, new mexico 88002
  #            33 pages
  #       baker, schlatter  17-may-1982
  #   this function returns the temperature (celsius) on a mixing
  #   ratio line w (g/kg) at pressure p (mb). the formula is
  #   given in
  #   table 1 on page 7 of stipanuk (1973).
  #
  #   initialize constants
  c1 <- 0.049864645499999999
  c2 <- 2.4082965000000001
  c3 <- 7.0747499999999999
  c4 <- 38.9114
  c5 <- 0.091499999999999998
  c6 <- 1.2035
  x <- log10((w * p)/(622. + w))
  tmrk <- 10^(c1 * x + c2) - c3 + c4 * ((10.^(c5 * x) - c6)^
      2.)
  tmrk - 273.14999999999998
}

tda <- function(o, p)	{
  #       reference stipanuk paper entitled:
  #            "algorithms for generating a skew-t, log p
  #            diagram and computing selected meteorological
  #            quantities."
  #            atmospheric sciences laboratory
  #            u.s. army electronics command
  #            white sands missile range, new mexico 88002
  #            33 pages
  #       baker, schlatter  17-may-1982
  #   this function returns the temperature tda (celsius)
  #   on a dry adiabat
  #   at pressure p (millibars). the dry adiabat is given by
  #   potential temperature o (celsius). the computation is
  #   based on
  #   poisson's equation.
  ok <- o + 273.14999999999998
  tdak <- ok * ((p * 0.001)^0.28599999999999998)
  tdak - 273.14999999999998
}

