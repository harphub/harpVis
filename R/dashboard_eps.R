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
dashboard_eps <- function(input, output, session, verif_data) {

  # bg_colour = "#D5D5D5"
  bg_colour = "#0A0A2C"

  dashboard_plots <- shiny::reactiveValues(
    spread_skill   = NULL,
    rank_histogram = NULL,
    crps           = NULL,
    reliability    = NULL,
    roc            = NULL,
    brier          = NULL
  )

  shiny::observeEvent(verif_data(), {
    shiny::req(verif_data())

    leadtimes <- unique(verif_data()[[1]]$leadtime)
    closest_to_twelve <- which(abs(leadtimes -12) == min(abs(leadtimes - 12)))
    selected_leadtime <- leadtimes[closest_to_twelve]

    dashboard_plots$rank_histogram <- harpVis::plot_point_verif(
      verif_data(),
      rank_histogram,
      plot_num_cases = FALSE,
      filter_by = ggplot2::vars(leadtime == selected_leadtime),
      legend_position = "none",
      plot_caption = "none",
      colour_theme = "harp_midnight",
      plot_title   = "Rank Histogram"
    )

    dashboard_plots$spread_skill <-  harpVis::plot_point_verif(
      verif_data(),
      spread_skill,
      plot_num_cases = FALSE,
      legend_position = "none",
      plot_caption = "none",
      colour_theme = "harp_midnight",
      plot_title   = "Spread :: Skill"
    )

    dashboard_plots$crps <-  harpVis::plot_point_verif(
      verif_data(),
      crps,
      plot_num_cases = FALSE,
      legend_position = "none",
      plot_caption = "none",
      colour_theme = "harp_midnight",
      plot_title   = "CRPS"
    )

    threshold_element <- grep("thresh", names(verif_data()))
    if (is.null(verif_data()[[threshold_element]])) {
      dashboard_plots$reliability <- NULL
      dashboard_plots$roc         <- NULL
      dashboard_plots$brier       <- NULL
      return()
    }
    if (nrow(verif_data()[[threshold_element]]) < 1) {
      dashboard_plots$reliability <- NULL
      dashboard_plots$roc         <- NULL
      dashboard_plots$brier       <- NULL
      return()
    }

    thresholds <- unique(verif_data()[[threshold_element]]$threshold)
    if (length(thresholds) == 1) {
      selected_threshold <- thresholds
    } else {
      selected_threshold <- thresholds[(length(thresholds) - 1)]
    }

    dashboard_plots$reliability <- harpVis::plot_point_verif(
      verif_data(),
      reliability,
      plot_num_cases = FALSE,
      filter_by = ggplot2::vars(leadtime == selected_leadtime, threshold == selected_threshold),
      legend_position = "none",
      plot_caption = "none",
      colour_theme = "harp_midnight",
      plot_title   = "Reliability"
    )

    dashboard_plots$roc <- harpVis::plot_point_verif(
      verif_data(),
      roc,
      plot_num_cases = FALSE,
      filter_by = ggplot2::vars(leadtime == selected_leadtime, threshold == selected_threshold),
      legend_position = "none",
      plot_caption = "none",
      colour_theme = "harp_midnight",
      plot_title   = "ROC"
    )

    dashboard_plots$brier <- harpVis::plot_point_verif(
      verif_data(),
      brier_score,
      plot_num_cases = FALSE,
      filter_by = ggplot2::vars(threshold == selected_threshold),
      legend_position = "right",
      num_legend_rows = 8,
      plot_caption = "none",
      colour_theme = "harp_midnight",
      plot_title   = "Brier Score"
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
