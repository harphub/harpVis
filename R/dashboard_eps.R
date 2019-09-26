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

  shiny::tabPanel("Dashboard",
    shiny::fluidRow(
      shiny::column(7,
        shiny::fluidRow(
          shiny::column(6,
            shiny::tags$div(class = "dashboard-panel",
              shiny::plotOutput(ns("dashboard_spread_skill"), height = "100%", width = "100%")
            )
          ),
          shiny::column(6,
            shiny::tags$div(class = "dashboard-panel",
              shiny::plotOutput(ns("dashboard_crps"), height = "100%", width = "100%")
            )
          )
        )
      ),
      shiny::column(5,
        shiny::tags$div(class = "dashboard-panel",
          shiny::plotOutput(ns("dashboard_rank_hist"), height = "100%", width = "100%")
        )
      )
    ),
    shiny::div(class = "row", id = ns("dashboard_thresh")),
    shiny::fluidRow(
      shiny::column(3,
        shiny::tags$div(class = "dashboard-panel",
          shiny::plotOutput(ns("dashboard_reliability"), height = "100%", width = "100%")
        )
      ),
      shiny::column(3,
        shiny::tags$div(class = "dashboard-panel",
          shiny::plotOutput(ns("dashboard_roc"), height = "100%", width = "100%")
        )
      ),
      shiny::column(6,
        shiny::tags$div(class = "dashboard-panel",
          shiny::plotOutput(ns("dashboard_brier"), height = "100%", width = "100%")
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
dashboard_eps <- function(input, output, session, verif_data, colour_table) {

  # bg_colour = "#D5D5D5"
  bg_colour = "#0A0A2C"

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

  shiny::observeEvent(list(verif_data(), colour_table()), {
    shiny::req(verif_data())
    shiny::req(colour_table())

    leadtimes <- unique(verif_data()[[1]]$leadtime)
    closest_to_twelve <- which(abs(leadtimes -12) == min(abs(leadtimes - 12)))
    selected_leadtime <- leadtimes[closest_to_twelve]

    legend_summary    <- "none"
    show_thresh_data  <- TRUE
    threshold_element <- grep("ens_thresh", names(verif_data()))

    if (is.null(verif_data()[[threshold_element]])) {
      dashboard_plots$reliability <- NULL
      dashboard_plots$roc         <- NULL
      dashboard_plots$brier       <- NULL
      legend_summary              <- "right"
      show_thresh_data            <- FALSE
    }
    if (nrow(verif_data()[[threshold_element]]) < 1) {
      dashboard_plots$reliability <- NULL
      dashboard_plots$roc         <- NULL
      dashboard_plots$brier       <- NULL
      legend_summary              <- "right"
      show_thresh_data            <- FALSE
    }

    dashboard_plots$rank_histogram <- harpVis::plot_point_verif(
      verif_data(),
      normalized_rank_histogram,
      plot_num_cases = FALSE,
      filter_by = ggplot2::vars(leadtime == selected_leadtime),
      legend_position = legend_summary,
      num_legend_rows = nrow(colour_table()),
      plot_caption = "none",
      colour_theme = "harp_midnight",
      plot_title   = "Rank Histogram",
      colour_table = colour_table()
    )

    dashboard_plots$spread_skill <-  harpVis::plot_point_verif(
      verif_data(),
      spread_skill,
      plot_num_cases = FALSE,
      legend_position = "none",
      plot_caption = "none",
      colour_theme = "harp_midnight",
      plot_title   = "Spread :: Skill",
      colour_table = colour_table()
    )

    dashboard_plots$crps <-  harpVis::plot_point_verif(
      verif_data(),
      crps,
      plot_num_cases = FALSE,
      legend_position = "none",
      plot_caption = "none",
      colour_theme = "harp_midnight",
      plot_title   = "CRPS",
      colour_table = colour_table()
    )

    if (show_thresh_data) {
      thresh_data_to_plot(verif_data())
    } else {
      thresh_data_to_plot(NULL)
    }

  })

  observeEvent(thresh_data_to_plot(), {

    shiny::removeUI(selector = paste0("#", ns("thresh_selectors")))
    if (!is.null(thresh_data_to_plot())) {
      threshold_element <- grep("ens_thresh", names(thresh_data_to_plot()))
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
                  sort(unique(thresh_data_to_plot()[[threshold_element]]$threshold)),
                  sort(unique(thresh_data_to_plot()[[threshold_element]]$threshold))[1]
                )
              ),
              shiny::column(6,
                shiny::selectInput(
                  ns("leadtime"),
                  "Lead Time",
                  sort(unique(thresh_data_to_plot()[[threshold_element]]$leadtime)),
                  sort(unique(thresh_data_to_plot()[[threshold_element]]$leadtime))[1]
                )
              )
            )
          )
        )
      )
    }

  }, ignoreNULL = FALSE)


  observeEvent(list(input$threshold, input$leadtime, thresh_data_to_plot(), colour_table()), {

    req(thresh_data_to_plot())
    req(colour_table())
    req(input$threshold)
    req(input$leadtime)

    thresh_data_attributes <- attributes(thresh_data_to_plot())

    plot_data_roc_rel <- purrr::map_at(
      thresh_data_to_plot(),
      "ens_threshold_scores",
      dplyr::filter,
      leadtime == input$leadtime,
      threshold == input$threshold
    )
    attributes(plot_data_roc_rel) <- thresh_data_attributes

    plot_data_brier <- purrr::map_at(
      thresh_data_to_plot(),
      "ens_threshold_scores",
      dplyr::filter,
      threshold == input$threshold
    )
    attributes(plot_data_brier) <- thresh_data_attributes

    if (nrow(plot_data_roc_rel[["ens_threshold_scores"]]) < 1) return()

    dashboard_plots$reliability <- harpVis::plot_point_verif(
      plot_data_roc_rel,
      reliability,
      plot_num_cases = FALSE,
      #filter_by = ggplot2::vars(leadtime == input$leadtime, threshold == input$threshold),
      legend_position = "none",
      plot_caption = "none",
      colour_theme = "harp_midnight",
      plot_title   = "Reliability",
      colour_table = colour_table()
    )

    dashboard_plots$roc <- harpVis::plot_point_verif(
      plot_data_roc_rel,
      roc,
      plot_num_cases = FALSE,
      #filter_by = ggplot2::vars(leadtime == input$leadtime, threshold == input$threshold),
      legend_position = "none",
      plot_caption = "none",
      colour_theme = "harp_midnight",
      plot_title   = "ROC",
      colour_table = colour_table()
    )

    dashboard_plots$brier <- harpVis::plot_point_verif(
      plot_data_brier,
      brier_score,
      plot_num_cases = FALSE,
      #f ilter_by = ggplot2::vars(threshold == input$threshold),
      legend_position = "right",
      num_legend_rows = nrow(colour_table()),
      plot_caption = "none",
      colour_theme = "harp_midnight",
      plot_title   = "Brier Score",
      colour_table = colour_table()
    )
  })

  output$dashboard_rank_hist <- shiny::renderPlot({
    shiny::req(dashboard_plots$rank_histogram)
  }, height = "auto", bg = bg_colour)
  output$dashboard_spread_skill <- shiny::renderPlot({
    shiny::req(dashboard_plots$spread_skill)
  }, height = "auto", bg = bg_colour)
  output$dashboard_crps <- shiny::renderPlot({
    shiny::req(dashboard_plots$crps)
  }, height = "auto", bg = bg_colour)
  output$dashboard_reliability <- shiny::renderPlot({
    shiny::req(dashboard_plots$reliability)
  }, height = "auto", bg = bg_colour)
  output$dashboard_roc <- shiny::renderPlot({
    shiny::req(dashboard_plots$roc)
  }, height = "auto", bg = bg_colour)
  output$dashboard_brier <- shiny::renderPlot({
    shiny::req(dashboard_plots$brier)
  }, height = "auto", bg = bg_colour)


}
