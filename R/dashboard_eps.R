# Shiny modeule file for eps dashboard - calls nested modules.

#' Title
#'
#' @param id
#'
#' @return
#' @export
#'
#' @examples
dashboard_epsUI <- function(id) {

  ns <- shiny::NS(id)

  shiny::div(
    shiny::fluidRow(
      shiny::column(7,
        shiny::fluidRow(
          shiny::column(6,
            shiny::tags$div(class = "dashboard-panel",
              shiny::plotOutput(ns("dashboard_summary_1"), height = "100%", width = "100%")
            )
          ),
          shiny::column(6,
            shiny::tags$div(class = "dashboard-panel",
              shiny::plotOutput(ns("dashboard_summary_2"), height = "100%", width = "100%")
            )
          )
        )
      ),
      shiny::column(5,
        shiny::tags$div(class = "dashboard-panel",
          shiny::plotOutput(ns("dashboard_summary_3"), height = "100%", width = "100%")
        )
      )
    ),
    shiny::div(class = "row", id = ns("dashboard_thresh")),
    shiny::fluidRow(
      shiny::column(3,
        shiny::tags$div(class = "dashboard-panel",
          shiny::plotOutput(ns("dashboard_thresh_1"), height = "100%", width = "100%")
        )
      ),
      shiny::column(3,
        shiny::tags$div(class = "dashboard-panel",
          shiny::plotOutput(ns("dashboard_thresh_2"), height = "100%", width = "100%")
        )
      ),
      shiny::column(6,
        shiny::tags$div(class = "dashboard-panel",
          shiny::plotOutput(ns("dashboard_thresh_3"), height = "100%", width = "100%")
        )
      )
    )
  )
}

#' Title
#'
#' @param input
#' @param output
#' @param session
#'
#' @return
#' @export
#'
#' @examples
dashboard_eps <- function(
  input,
  output,
  session,
  verif_data,
  colour_table,
  time_axis
) {

  # bg_colour = "#D5D5D5"

  ns <- session$ns

  dashboard_plots <- shiny::reactiveValues(
    spread_skill   = NULL,
    rank_histogram = NULL,
    crps           = NULL,
    reliability    = NULL,
    roc            = NULL,
    brier          = NULL
  )

  thresh_data_to_plot <- shiny::reactiveVal(NULL)
  new_data            <- shiny::reactiveVal(1)
  summary_table       <- shiny::reactiveVal(NULL)
  summary_scores      <- shiny::reactiveVal(NULL)
  thresh_table        <- shiny::reactiveVal(NULL)
  thresh_scores       <- shiny::reactiveVal(NULL)
  verif_type          <- shiny::reactiveVal(NULL)

  set_height <- function() {
    if (is.null(verif_data())) {
      return("auto")
    }
    if (is.null(attr(verif_data(), "is_profile"))) {
      return("auto")
    }
    if (attr(verif_data(), "is_profile")) {
      return(600)
    }
    "auto"
  }

  get_bg_colour = function() "#0A0A2C"


  shiny::observeEvent(verif_data(), {
    shiny::req(verif_data())
    is_ens <- any(
      sapply(
        names(verif_data()),
        function(x) isTRUE(grepl("ens", x) && nrow(verif_data()[[x]]) > 0)
      )
    )
    if (is_ens) {
      verif_type("ens")
      summary_table("ens_summary_scores")
      summary_scores(c(
        spread_skill              = "Spread :: Skill",
        crps                      = "CRPS",
        normalized_rank_histogram = "Rank Histogram"
      ))
      thresh_table("ens_threshold_scores")
      thresh_scores(c(
        reliability = "Reliability",
        roc         = "ROC",
        brier_score = "Brier Score"
      ))
    } else {
      verif_type("det")
      summary_table("det_summary_scores")
      summary_scores(c(
        bias = "Mean Bias",
        mae  = "MAE",
        stde = "Std Dev of Error"
      ))
      thresh_table("det_threshold_scores")
      thresh_scores(c(
        hit_rate               = "Hit Rate",
        false_alarm_rate       = "False Alarm Rate",
        equitable_threat_score = "ETS"
      ))
    }
    new_data(new_data() * -1)
  })

  shiny::observeEvent(list(new_data(), colour_table(), time_axis()), {
    shiny::req(verif_data())
    shiny::req(colour_table())
    shiny::req(time_axis())
    lead_time_var <- intersect(
      c("leadtime", "lead_time"),
      colnames(verif_data()[[summary_table()]])
    )

    leadtimes <- unique(verif_data()[[summary_table()]][[lead_time_var]])
    if (length(leadtimes) < 1) {
      return()
    }
    if (is.character(leadtimes)) {
      leadtimes <- as.numeric(leadtimes[!leadtimes %in% "All"])
    }
    closest_to_twelve <- which(abs(leadtimes - 12) == min(abs(leadtimes - 12)))
    selected_leadtime <- leadtimes[closest_to_twelve]

    legend_summary    <- "none"
    show_thresh_data  <- TRUE

    if (is.null(verif_data()[[thresh_table()]]) || nrow(verif_data()[[thresh_table()]]) < 1) {
      dashboard_plots$reliability <- NULL
      dashboard_plots$roc         <- NULL
      dashboard_plots$brier       <- NULL
      legend_summary              <- "right"
      show_thresh_data            <- FALSE
    }

    for (i in seq_along(summary_scores())) {
      plot_x_axis <- rlang::sym(time_axis())
      plot_flip_axes <- FALSE
      is_profile <- attr(verif_data(), "is_profile")
      if (!is.null(is_profile) && is_profile) {
        if (summary_scores()[i] != "Rank Histogram") {
          plot_x_axis <- rlang::sym("p")
          plot_flip_axes <- TRUE
        }
      }
      aspect_ratio <- NULL
      if (plot_flip_axes) {
        aspect_ratio <- 1.25
      }
      dashboard_plots[[paste0("summary_", i)]] <- harpVis::plot_point_verif(
        verif_data(),
        !!rlang::sym(names(summary_scores())[i]),
        verif_type      = verif_type(),
        x_axis          = !!plot_x_axis,
        plot_num_cases  = FALSE,
        rank_is_relative = TRUE,
        rank_hist_type   = "lollipop",
        legend_position = ifelse(i < length(summary_scores()), "none", legend_summary),
        num_legend_rows = nrow(colour_table()),
        plot_caption    = "none",
        colour_theme    = "harp_midnight",
        plot_title      = summary_scores()[i],
        colour_table    = colour_table(),
        flip_axes       = plot_flip_axes
      ) +
        ggplot2::theme(aspect.ratio = aspect_ratio)
    }

    if (show_thresh_data) {
      thresh_data_to_plot(verif_data())
    } else {
      thresh_data_to_plot(NULL)
    }

  })

  shiny::observeEvent(list(thresh_data_to_plot(), time_axis()), {


    shiny::removeUI(selector = paste0("#", ns("thresh_selectors")))
    if (!is.null(thresh_data_to_plot())) {
      thresh_col <- "threshold"
      if (is.element("percentile", colnames(thresh_data_to_plot()))) {
        thresh_col <- "percentile"
      }
      shiny::insertUI(
        selector = paste0("#", ns("dashboard_thresh")),
        where    = "beforeEnd",
        ui       = shiny::fluidRow(
          shiny::div(
            id    = ns("thresh_selectors"),
            shiny::column(3,
              shiny::column(6,
                shiny::selectInput(
                  ns("threshold"),
                  "Threshold",
                  sort(unique(
                    thresh_data_to_plot()[[thresh_table()]][[thresh_col]]
                  )),
                  sort(unique(
                    thresh_data_to_plot()[[thresh_table()]][[thresh_col]]
                  ))[1]
                )
              )
            )
          )
        )
      )
      if (any(names(thresh_scores()) %in% c("reliability", "roc", "economic_value"))) {
        time_var <- intersect(
          time_axis(),
          colnames(thresh_data_to_plot()[[thresh_table()]])
        )

        time_label <- gsub(
          "Dttm", "Date-Time", totitle(gsub("_", " ", time_var))
        )

        times <- unique(thresh_data_to_plot()[[thresh_table()]][[time_var]])
        times <- times[times != "All"]
        if (grepl("dttm", time_var)) {
          times_dttm <- do.call(c, lapply(times, as.POSIXct, tz = "UTC"))
          times <- times[order(match(times, times_dttm))]
          names(times) <- format(times_dttm, "%H:%M %d %b %Y")
        } else {
          times <- sort(as.numeric(times))
        }
        shiny::insertUI(
          selector = paste0("#", ns("thresh_selectors")),
          where    = "beforeEnd",
          ui       =  shiny::column(6,
            shiny::selectInput(
              ns("leadtime"),
              time_label,
              times,
              times[1]
            )
          )
        )
      }

    }

  }, ignoreNULL = FALSE)


  shiny::observeEvent(
    list(
      input$threshold, input$leadtime, thresh_data_to_plot(),
      colour_table(), time_axis()
    ), {

    if (is.null(thresh_data_to_plot())) {
      for (i in 1:3) {
        dashboard_plots[[paste0("thresh_", i)]] <- NULL
      }
      return()
    }

    shiny::req(colour_table())
    shiny::req(input$threshold)

    thresh_data_attributes <- attributes(thresh_data_to_plot())

    if (any(names(thresh_scores()) %in% c("reliability", "roc", "economic_value"))) {

      time_var <- intersect(
        time_axis(),
        colnames(thresh_data_to_plot()[[thresh_table()]])
      )

      shiny::req(input$leadtime)

      plot_data_thresh_lead <- purrr::map_at(
        thresh_data_to_plot(),
        thresh_table(),
        dplyr::filter,
        .data[[time_var]] == input$leadtime,
        .data[["threshold"]]   == input$threshold
      )
      attributes(plot_data_thresh_lead) <- thresh_data_attributes
    }

    plot_data_thresh <- purrr::map_at(
      thresh_data_to_plot(),
      thresh_table(),
      dplyr::filter,
      threshold == input$threshold
    )
    attributes(plot_data_thresh) <- thresh_data_attributes

    for (i in seq_along(thresh_scores())) {

      if (names(thresh_scores())[i] %in% c("reliability", "roc", "economic_value")) {
        plot_data <- plot_data_thresh_lead
      } else {
        plot_data <- plot_data_thresh
      }

      if (nrow(plot_data[[thresh_table()]]) < 1) {
        dashboard_plots[[paste0("thresh_", i)]] <- NULL
      } else {
        dashboard_plots[[paste0("thresh_", i)]] <- harpVis::plot_point_verif(
          plot_data,
          !!rlang::sym(names(thresh_scores())[i]),
          verif_type       = verif_type(),
          x_axis           = !!rlang::sym(time_axis()),
          plot_num_cases   = FALSE,
          rank_is_relative = TRUE,
          rank_hist_type   = "lollipop",
          legend_position  = ifelse(i < length(thresh_scores()), "none", "right"),
          num_legend_rows  = nrow(colour_table()),
          plot_caption     = "none",
          colour_theme     = "harp_midnight",
          plot_title       = thresh_scores()[i],
          colour_table     = colour_table()
        )
      }

    }

  }, ignoreNULL = FALSE)

  output$dashboard_summary_1 <- shiny::renderPlot({
    shiny::req(dashboard_plots$summary_1)
  }, height = "auto", res = 96, bg = get_bg_colour())

  output$dashboard_summary_2 <- shiny::renderPlot({
    shiny::req(dashboard_plots$summary_2)
  }, height = "auto", res = 96, bg = get_bg_colour())

  output$dashboard_summary_3 <- shiny::renderPlot({
    shiny::req(dashboard_plots$summary_3)
  }, height = "auto", res = 96, bg = get_bg_colour())

  output$dashboard_thresh_1 <- shiny::renderPlot({
    shiny::req(dashboard_plots$thresh_1)
  }, height = "auto", res = 96, bg = get_bg_colour())

  output$dashboard_thresh_2 <- shiny::renderPlot({
    shiny::req(dashboard_plots$thresh_2)
  }, height = "auto", res = 96, bg = get_bg_colour())

  output$dashboard_thresh_3 <- shiny::renderPlot({
    shiny::req(dashboard_plots$thresh_3)
  }, height = "auto", res = 96, bg = get_bg_colour())


}
