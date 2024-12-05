#' Plot spatial verification scores
#'
#' \code{plot_spatial_verif} is used to plot verification scores computed by
#' functions from the harpSpatial package. This function uses it's own plotting
#' options while allowing specific plot options (\code{plot_opts}) to be passed
#' into a separate plotting function (such as \code{plot_basic}, \code{plot_fss},
#' \code{plot_sal} and \code{plot_nact}).
#'
#' @param verif_data Output from \link[harpSPatial]{spatial_verify}. Expected to
#'   either be a dataframe or an SQLite file (needs path to file).
#' @param score_name The score to plot. This will call the appropriate spatial plotting
#'   function through \code{spatial_plot_func}.
#' @param filter_by Filter the data before plotting. Must be wrapped inside the
#'   \link[dplyr]{vars} function. This can be useful for making a single plot
#'   where there are many groups. For example, if the data contains various models
#'   the data can be filtered with
#'   e.g. \code{filter_by = vars(det_model == "be13", fctime == 0)}.
#' @param show_info Prints the contents of the score tables used as input before
#'   visualisation as well as plotting options (\code{plot_opts}) if any exist.
#' @param plot_opts A list of plotting options that may be passed from outside into
#'   plotting functions that may use something specific while others might not be used.
#' @param colour_theme The colour theme for the plot - can be any ggplot2 theme
#' @param base_size base font size.
#' @param base_family base font family.
#' @param base_line_size base size for line elements.
#' @param base_rect_size base size for rect elements.
#' @param legend_position The position of legends ("none", "left", "right",
#'   "bottom", "top", or two-element numeric vector).
#' @param plot_caption Caption for the plot. Set to "auto" to automatically
#'   generate the caption. Anything else inside quotes will be used as the plot caption.
#' @param leadtimes_by Leadtimes in scores are by default expected to be in seconds
#'   but there is an option to view leadtimes as hours, minutes or seconds.
#' @param save_image Saves the plot as a .png file without a preset path.
#' @return A plot (interactive or saved image)
#' @import ggplot2
#' @export
#'
#' @examples
#' plot_spatial_verif(verif_data, 'SAL')
#' plot_spatial_verif(verif_data, 'FSS')
#' plot_spatial_verif(verif_data, 'NACT')
#' plot_spatial_verif(verif_data, 'NACT', plot_opts = list(nact_scores = list("pod", "far"), colour_by = "scale"))
#' plot_spatial_verif(verif_data, 'mse')
#' plot_spatial_verif(verif_data, 'mae')
#' plot_spatial_verif(verif_data, 'bias')

plot_spatial_verif <- function(
  verif_data,
  score_name,
  filter_by         = NULL,
  show_info         = FALSE,
  plot_opts         = list(),
  colour_theme      = "bw",
  base_size         = 11,
  base_family       = "",
  base_line_size    = base_size / 22,
  base_rect_size    = base_size / 22,
  legend_position   = "right",
  plot_caption      = "auto",
  leadtimes_by      = "hours",
  save_image        = FALSE,

  ...) {

  score_quo  <- rlang::enquo(score_name)
  score_name <- rlang::quo_name(score_quo)
  my_plot_func <- spatial_plot_func(score_name)

  ################

  if (!is.object(verif_data) && is.character(verif_data)) {
    if (grepl(".sqlite", verif_data)) {
      library(RSQLite)
      message(verif_data)
      sql_object <- harpIO:::dbopen(verif_data)
      verif_data <- as.data.frame(harpIO:::dbquery(sql_object, paste("SELECT * FROM ", score_name)))
      harpIO:::dbclose(sql_object)
    }
  }
  ################

  # the column(s) to filter data for before plotting
  filter_by_err  <- paste("filter_by must be wrapped in vars and unquoted,\n",
    "e.g. filter_by = vars(leadtime == 12, threshold == 280).")
  filter_by_null <- try(is.null(filter_by), silent = TRUE)
  filter_vars    <- ""
  if (inherits(filter_by_null, "try-error")) {
    stop(filter_by_err, call. = FALSE)
  } else {
    if (filter_by_null) {
      filtering <- FALSE
    } else {
      if (inherits(filter_by, "quosures")) {
        filtering <- TRUE
        filter_vars <- names(purrr::map_chr(rlang::eval_tidy(filter_by), rlang::quo_name))
      } else {
        stop(filter_by_err, call. = FALSE)
      }
    }
  }

  # select the right data set if verif_data is a list of tables

  if (!is.data.frame(verif_data) && is.list(verif_data)) {
    # If list of tables, select the table of selected score,
    # each score has it's own table
    plot_data <- tibble::as_tibble(verif_data[[score_name]])
  } else {
    plot_data <- tibble::as_tibble(verif_data)
  }

  if (any(!is.element(c("fcdate", "leadtime"), names(plot_data)))) {
    fcbdate <- NULL
    fcedate <- NULL
    stop("columns named fcdate and leadtime are missing!")
  } else {
    plot_data <- plot_data %>% dplyr::mutate(fcdates = plot_data$fcdate + plot_data$leadtime) # valid datetimes

    plot_data$fcdate <- lubridate::as_datetime(plot_data$fcdate,
                                               origin = lubridate::origin,
                                               tz = "UTC")

    plot_data$fcdates <- lubridate::as_datetime(plot_data$fcdates,
                                               origin = lubridate::origin,
                                               tz = "UTC")
    ## forecast dates
    fcbdate <- strftime(min(plot_data$fcdate, na.rm = TRUE), format = "%d-%m-%Y %H:%M")
    fcedate <- strftime(max(plot_data$fcdate, na.rm = TRUE), format = "%d-%m-%Y %H:%M")
    ## filename dates
    savebdate <- strftime(min(plot_data$fcdate, na.rm = TRUE), format = "%Y%m%d%H%M")
    saveedate <- strftime(max(plot_data$fcdate, na.rm = TRUE), format = "%Y%m%d%H%M")

    valid_hours <- unique(lubridate::hour(plot_data$fcdate))
  }

  # leadtimes in the dataframe are usually in seconds,
  # so for better viewing one can convert to hours/minutes
  if (is.element("leadtime", names(plot_data))) {
    if (leadtimes_by == "hours") {
       ldtconv <- 3600
    } else if (leadtimes_by == "minutes") {
       ldtconv <- 60
    } else {
       ldtconv <- 1
    }
    plot_data <- plot_data %>% dplyr::mutate(leadtime = leadtime / ldtconv)
  }

  if (filtering) {
    plot_data <- dplyr::filter(plot_data, !!! filter_by)
    ## forecast dates, find limits again in case it was specified in filtering
    fcbdate <- strftime(min(plot_data$fcdate, na.rm = TRUE), format = "%d-%m-%Y %H:%M")
    fcedate <- strftime(max(plot_data$fcdate, na.rm = TRUE), format = "%d-%m-%Y %H:%M")
    ## filename dates
    savebdate <- strftime(min(plot_data$fcdate, na.rm = TRUE), format = "%Y%m%d%H%M")
    saveedate <- strftime(max(plot_data$fcdate, na.rm = TRUE), format = "%Y%m%d%H%M")
  }

  # This should be done after filtering
  used_models <- paste(unique(plot_data$model), sep = "-")
  used_params <- paste(unique(plot_data$prm), sep = "-")

  if (is.element("leadtime", names(plot_data)) && length(unique(plot_data$leadtime)) > 1) {
    message("Multiple leadtimes found: ", paste(unique(plot_data$leadtime), collapse = " "))
  }
  if (is.element("model", names(plot_data)) && length(unique(plot_data$model)) > 1) {
    message("Multiple models found: ", paste(unique(plot_data$model), collapse = " "))
  }
  if (is.element("prm", names(plot_data)) && length(unique(plot_data$prm)) > 1) {
    message("Multiple parameters found: ", paste(unique(plot_data$prm), collapse = " "))
  }
  
  # Add in filtered valid hours
  if ("fcst_cycle" %in% names(plot_data)){
    filtered_valid_hours <- as.integer(unique(plot_data$fcst_cycle))
  } else {
    filtered_valid_hours <- valid_hours
  }

  if (show_info) {
      message("================================")
      print(verif_data)
      message("================================")
      message("Score name: ", score_name)
      print(plot_data, n = Inf)
      message("Start date: ", fcbdate)
      message("End date: ", fcedate)
      message("Model(s): ", used_models)
      message("Parameter(s): ", used_params)
      message("Plotting options: ")
      print(plot_opts)
      message("Filtering options: ")
      print(filter_by)
      message("================================")
  }

  ########## PLOTTING FUNCTION SELECTION
  # In cases where plot_spatial_line is used,
  # score_name will be used to look for both the
  # table name AS WELL AS the column name,
  # so this might change in the future
  gg <- do.call(my_plot_func, c(list(plot_data, score_name), plot_opts))

  ### Plot background, stolen from plot_point_verif

  if (is.function(colour_theme)) {
    theme_func <- colour_theme
  } else {
    if (!grepl("^theme_[[:alpha:]]", colour_theme)) colour_theme <- paste0("theme_", colour_theme)
    if (grepl("harp", colour_theme)) {
      function_env <- "harpVis"
    } else {
      function_env <- "ggplot2"
    }
    theme_func <- get(colour_theme, envir = asNamespace(function_env))
  }

  gg <- gg + theme_func(
    base_size      = base_size,
    base_family    = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  )

  ####
  if (my_plot_func == "plot_spatial_line") {
    plot_subtitle <- paste("Period:", fcbdate, "-", fcedate,
                           ":: Hours {", paste(c(sprintf("%02d", filtered_valid_hours)),
                           collapse = ", "), "}")
  } else {
    ldts <- unique(plot_data$leadtime)
    # truncate leadtimes if there are too many
    if (length(ldts) > 5) {
      plot_subtitle <- paste("Period:", fcbdate, "-", fcedate,
                             ":: Hours {", paste(c(sprintf("%02d", filtered_valid_hours)), collapse = ", "), "}",
                             ":: Leadtimes {", paste(ldts[1:3], collapse = ", "), ", ...,", ldts[length(ldts)], "}")
    } else {
      plot_subtitle <- paste("Period:", fcbdate, "-", fcedate,
                             ":: Hours {", paste(c(sprintf("%02d", filtered_valid_hours)), collapse = ", "), "}",
                             ":: Leadtimes {", paste(ldts, collapse = ", "), "}")
    }
  }

  if (plot_caption == "auto") {
    plot_caption <- paste("Verification for", used_params)
  }

  gg <- gg + ggplot2::labs(subtitle = plot_subtitle, caption = plot_caption)

  # finished
  if (save_image) {
    fname <- paste(score_name, "_",
                  savebdate, "_",
                  saveedate, "_",
                  used_models, "_",
                  used_params, ".png",
                  sep = "")
    message("Saving as: ", fname)
    ggplot2::ggsave(filename = fname,
                    dpi     = 96,
                    width   = 800,
                    height  = 600,
                    units   = "px")
  } else {
    gg
  }
}
