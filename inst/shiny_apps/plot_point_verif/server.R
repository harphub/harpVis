# server for shiny_plot_point_verif

server <- function(input, output, session) {

  # bg_colour = "#D5D5D5"
  bg_colour = "#0A0A2C"

  verif_data <- shiny::callModule(harpVis::options_bar, "options_bar")


  ############################################################
  # DASHBOARD PLOTS                                          #
  ############################################################

  shiny::callModule(harpVis::dashboard_eps, "dashboard", verif_data)

  ############################################################
  # INTERACTIVE PLOT                                         #
  ############################################################

  score_names <- shiny::reactiveVal("Waiting for valid data")

  shiny::observeEvent(verif_data(), {
    shiny::req(verif_data())
    if (any(grepl("ens_", names(verif_data())))) {
      scores <- c(
        unlist(lapply(harpVis::ens_verif_data, names), use.names = FALSE),
        "spread_skill", "spread_skill_ratio", "brier_score_decomposition", "sharpness"
      )
    } else if (any(grepl("det_", names(verif_data())))) {
      scores <- unlist(lapply(harpVis::det_verif_data, names), use.names = FALSE)
    } else {
      return(score_names("Waiting for valid data"))
    }
    scores <- sort(setdiff(scores, c("mname", "leadtime", "threshold")))
    scores <- gsub("num_", "number_of_", scores)
    score_names(harpVis:::totitle(gsub("_", " ", scores)))
  })

  shiny::observeEvent(score_names(), {
    shiny::updateSelectInput(session, "score", choices = score_names())
  })

  ###

  score_to_plot <- reactiveVal()

  shiny::observeEvent(input$score, {
    shiny::req(input$score)
    if (input$score == "Waiting for valid data") {
      score_to_plot(NULL)
    } else {
      score_to_plot(input$score)
    }
  })

  score_options <- reactiveValues(
    score   = NULL,
    x_axis  = "leadtime",
    facets  = NULL,
    filters = NULL
  )

  shiny::observeEvent(score_to_plot(), {
    shiny::req(score_to_plot())

    if (grepl("Brier", score_to_plot()) | score_to_plot() == "ROC Area") {

      threshold_element <- grep("thresh", names(verif_data()))
      if (is.null(verif_data()[[threshold_element]])) {
        score_options$x_axis  <- "leadtime"
        score_options$facets  <- NULL
        score_options$filters <- NULL
        return()
      }

      thresholds <- unique(verif_data()[[threshold_element]]$threshold)
      leadtimes  <- unique(verif_data()[[threshold_element]]$leadtime)

      shiny::removeUI("#scoreOptions")
      shiny::insertUI(selector = "#scoreOptionsPlaceholder", ui = tags$div(id = "scoreOptions"))
      shiny::insertUI(
        selector = "#scoreOptions",
        ui = shiny::radioButtons(
          "x_axis",
          "x axis",
          choiceNames  = c("Lead Time", "Threshold"),
          choiceValues = c("leadtime", "threshold"),
          selected     = "leadtime"
        )
      )

      shiny::observeEvent(input$x_axis, {
        shiny::req(input$x_axis)
        if (input$x_axis == "leadtime") {
          selected_thresholds = thresholds[1]
          selected_leadtimes  = leadtimes
        } else {
          selected_thresholds = thresholds
          selected_leadtimes  = leadtimes[1]
        }

        shiny::removeUI("#thresholds_selector")
        shiny::removeUI("#leadtimes_selector")

        shiny::insertUI(
          selector = "#scoreOptions",
          ui = shiny::tags$div(
            id = "thresholds_selector",
            shiny::selectInput(
              "thresholds",
              "Threshold",
              choices  = thresholds,
              selected = selected_thresholds,
              multiple = TRUE
            )
          )
        )

        shiny::insertUI(
          selector = "#scoreOptions",
          ui = shiny::tags$div(
            id = "leadtimes_selector",
            shiny::selectInput(
              "leadtimes",
              "Lead Time",
              choices  = leadtimes,
              selected = selected_leadtimes,
              multiple = TRUE
            )
          )
        )

      })

      shiny::observeEvent(list(input$thresholds, input$leadtimes, input$x_axis), {
        shiny::req(input$thresholds)
        shiny::req(input$leadtimes)
        shiny::req(input$x_axis)
        score_options$x_axis  <- input$x_axis
        score_options$filters <- ggplot2::vars(leadtime %in% input$leadtimes, threshold %in% input$thresholds)
        if (input$x_axis == "leadtime" & length(input$thresholds) > 1) {
          score_options$facets <- ggplot2::vars(threshold)
        } else if (input$x_axis == "threshold" & length(input$leadtimes) > 1) {
          score_options$facets <- ggplot2::vars(leadtime)
        } else if (length(input$thresholds) > 1 & length(input$leadtimes) > 1) {
          score_options$facets <- ggplot2::vars(leadtime, threshold)
        } else {
          score_options$facets <- NULL
        }
        score_options$score <- score_to_plot()
      })


    } else if (grepl("Value", score_to_plot()) | score_to_plot() %in% c("ROC", "Reliability", "Sharpness")) {

      threshold_element <- grep("thresh", names(verif_data()))
      if (is.null(verif_data()[[threshold_element]])) {
        score_options$x_axis  <- "leadtime"
        score_options$facets  <- NULL
        score_options$filters <- NULL
        return()
      }

      thresholds <- unique(verif_data()[[threshold_element]]$threshold)
      leadtimes  <- unique(verif_data()[[threshold_element]]$leadtime)

      shiny::removeUI("#scoreOptions")
      shiny::insertUI(selector = "#scoreOptionsPlaceholder", ui = tags$div(id = "scoreOptions"))

      shiny::insertUI(
        selector = "#scoreOptions",
        ui = shiny::tags$div(
          id = "thresholds_selector",
          shiny::selectInput(
            "thresholds",
            "Threshold",
            choices  = thresholds,
            selected = thresholds[1],
            multiple = TRUE
          )
        )
      )

      shiny::insertUI(
        selector = "#scoreOptions",
        ui = shiny::tags$div(
          id = "leadtimes_selector",
          shiny::selectInput(
            "leadtimes",
            "Lead Time",
            choices  = leadtimes,
            selected = leadtimes[1],
            multiple = TRUE
          )
        )
      )

      shiny::observeEvent(list(input$thresholds, input$leadtimes), {
        req(input$thresholds)
        req(input$leadtimes)
        score_options$x_axis  <- input$x_axis
        score_options$filters <- ggplot2::vars(leadtime %in% input$leadtimes, threshold %in% input$thresholds)
        if (length(input$leadtimes) == 1 & length(input$thresholds) > 1) {
          score_options$facets <- ggplot2::vars(threshold)
        } else if (length(input$thresholds) == 1 & length(input$leadtimes) > 1) {
          score_options$facets <- ggplot2::vars(leadtime)
        } else if (length(input$thresholds) > 1 & length(input$leadtimes) > 1) {
          score_options$facets <- ggplot2::vars(leadtime, threshold)
        } else {
          score_options$facets <- NULL
        }
        score_options$score <- score_to_plot()
      })



    } else {

      shiny::removeUI("#scoreOptions")
      score_options$x_axis  <- "leadtime"
      score_options$facets  <- NULL
      score_options$filters <- NULL
      score_options$score   <- score_to_plot()



    }
  })

  ggplot_data <- shiny::reactiveVal()

  score_options_debounce <- shiny::debounce(shiny::reactive(shiny::reactiveValuesToList(score_options)), 500)

  observeEvent(score_options_debounce(), {
    shiny::req(score_options_debounce()$score)
    print("Getting plot")
    plot_score     <- gsub("number_of_", "num_", tolower(gsub(" ", "_", score_options_debounce()$score)))
    plot_score_sym <- rlang::sym(plot_score)
    x_axis_sym     <- rlang::sym(score_options_debounce()$x_axis)
    ggplot_data(
      harpVis::plot_point_verif(
        verif_data(),
        !! plot_score_sym,
        x_axis       = !! x_axis_sym,
        facet_by     = score_options_debounce()$facets,
        filter_by    = score_options_debounce()$filters,
        colour_theme = "harp_midnight"
      )
    )
  })


  output$verif_plot <- shiny::renderPlot({
    shiny::req(ggplot_data())
  }, height = 550, bg = bg_colour)

} # end of shiny server
