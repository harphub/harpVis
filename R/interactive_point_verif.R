#' @rdname interactive_point_verif
#' @inheritParams dashboard_point_verifUI
#' @export
interactive_point_verifUI <- function(id) {

  ns <- shiny::NS(id)

  shiny::fluidRow(
    shiny::column(2,
      shiny::selectInput(ns("score"), "Score", "Waiting for valid data"),
      shiny::tags$div(id = ns("placeholder"))
    ),
    shiny::column(8,
      shiny::plotOutput(ns("plot"), height = "100%")
    )
  )


}


#' Shiny module for interactively showing point verification scores
#'
#' @inheritParams dashboard_point_verif
#' @return An interactive list of options chosen for a plot that can be passed
#'   to \code{\link{download_verif_plot}}
#' @export
#'
#' @examples
#' library(shiny)
#'
#' shinyOptions(theme = "white")
#'
#' ui <- fluidPage(
#'   fluidRow(
#'     column(12, interactive_point_verifUI("int"))
#'   ),
#'   fluidRow(
#'     column(12, verbatimTextOutput("opts"))
#'   )
#' )
#'
#' server <- function(input, output, session) {
#'
#'   col_tbl <- data.frame(
#'     fcst_model = unique(verif_data_ens$ens_summary_scores$fcst_model),
#'     colour     = c("red", "blue")
#'   )
#'
#'   opts <- callModule(
#'     interactive_point_verif, "int", reactive(verif_data_ens),
#'     reactive(col_tbl),  reactive("lead_time")
#'   )
#'
#'   output$opts <- renderPrint({
#'     req(opts())
#'   })
#'
#' }
#'
#' if (interactive()) {
#'   shinyApp(ui, server)
#' }

interactive_point_verif <- function(
  input, output, session, verif_data, colour_table, time_axis
) {

  ns <- session$ns

  theme_opt <- shiny::getShinyOption("theme", default = "white")

  plot_theme <- switch(
    theme_opt,
    "dark"  = "harp_midnight",
    "light" = "harp_light",
    "white" = "bw"
  )

  bg_colour = switch(
    theme_opt,
    "dark"  = "#0A0A2C",
    "light" = "#F0F1F2",
    "white" = "white"
  )

  new_data <- shiny::reactiveVal(NULL)


  # Get the ui type we need from the score

  ui_type        <- shiny::reactiveVal("ens_summary")
  more_selectors <- shiny::reactiveVal(FALSE)
  det_ens        <- shiny::reactiveVal(FALSE)
  all_members    <- shiny::reactiveVal("")
  all_models     <- shiny::reactiveVal("")
  thresh_types   <- shiny::reactiveVal(FALSE)

  # Reset the app when new data arrives

  shiny::observeEvent(verif_data(), {
    shiny::removeUI(paste0("#", ns("ens-summary")))
    shiny::removeUI(paste0("#", ns("ens-rank-hist")))
    shiny::removeUI(paste0("#", ns("ens-cat")))
    shiny::removeUI(paste0("#", ns("ens-cat-choose-x")))
    shiny::removeUI(paste0("#", ns("det-summary")))
    shiny::removeUI(paste0("#", ns("det-cat")))
    shiny::removeUI(paste0("#", ns("ens-cat-choose-x-leadtime")))
    shiny::removeUI(paste0("#", ns("ens-cat-choose-x-threshold")))

    shiny::req(verif_data())

    if (is.null(new_data())) {
      new_data(1)
    } else {
      new_data(new_data() * -1)
    }

    shiny::updateSelectInput(session, "score", choices = "Waiting for valid data")
    ui_type(NULL)
  })

  # Update the score selector

  shiny::observeEvent(new_data(), {
    shiny::req(new_data())
    score_choices <- make_score_list(verif_data())
    selected_score <- input$score
    if (!is.null(selected_score)) {
      if (!is.element(input$score, unlist(score_choices))) {
        selected_score <- NULL
      }
    }
    shiny::updateSelectInput(session, "score", choices = score_choices, selected = selected_score)
    det_df <- verif_data()[["det_summary_scores"]]
    if (!is.null(det_df) && is.data.frame(det_df) && is.element("member", colnames(det_df))) {
      det_ens(TRUE)
    } else {
      det_ens(FALSE)
    }
    thresh_dfs <- grep("threshold", names(verif_data()))
    if (length(thresh_dfs) > 0) {
      thresh_colnames <- Reduce(
        union, lapply(verif_data()[thresh_dfs], colnames)
      )
      if (is.element("Type", thresh_colnames)) {
        thresh_types(TRUE)
      }
    }
  })

  shiny::observeEvent(input$score, {
    ui_type(get_ui_type(input$score))
  })

  shiny::observeEvent(list(det_ens(), new_data()), {
    if (det_ens()) {
      fcst_model_col <- intersect(
        c("fcst_model", "mname"),
        colnames(verif_data()[["det_summary_scores"]])
      )
      all_members(sort(unique(verif_data()[["det_summary_scores"]][["member"]])))
      all_models(sort(unique(verif_data()[["det_summary_scores"]][[fcst_model_col]])))
    } else {
      all_members("")
      all_models("")
    }
  })

  # Update the UI based on the score

  shiny::observeEvent(list(ui_type(), all_members(), time_axis()), {

    shiny::req(ui_type())

    shiny::removeUI(paste0("#", ns("ens-summary")))
    shiny::removeUI(paste0("#", ns("ens-rank-hist")))
    shiny::removeUI(paste0("#", ns("ens-hexbin")))
    shiny::removeUI(paste0("#", ns("ens-cat")))
    shiny::removeUI(paste0("#", ns("ens-cat-choose-x")))
    shiny::removeUI(paste0("#", ns("det-summary")))
    shiny::removeUI(paste0("#", ns("det-hexbin")))
    shiny::removeUI(paste0("#", ns("det-cat")))

    profile_verif <- attr(verif_data(), "is_profile")
    if (is.null(profile_verif)) {
      profile_verif <- FALSE
    }

    if (ui_type() == "ens_summary") {
      selected_y_to_zero <- input[["ens-extend-to-zero"]]
      if (is.null(selected_y_to_zero)) {
        selected_y_to_zero <- TRUE
      }
      selected_num_cases <- input[["ens-summary-num-cases"]]
      if (is.null(selected_num_cases)) {
        selected_num_cases <- FALSE
      }
      shiny::insertUI(
        selector = paste0("#", ns("placeholder")),
        ui       = shiny::tags$div(
          id = ns("ens-summary"),
          shiny::checkboxInput(
            ns("ens-extend-to-zero"),
            "y-axis to zero",
            selected_y_to_zero
          ),
          shiny::checkboxInput(
            ns("ens-summary-num-cases"),
            "Show number of cases",
            selected_num_cases
          )
        )
      )
      more_selectors(FALSE)
    }

    if (ui_type() == "ens_rank_hist") {

      if (!profile_verif) {
        times <- get_times(verif_data()[["ens_summary_scores"]], time_axis())

        times[["times"]] <- c("All", times[["times"]])

        selected_time <- input[["ens-rank-hist-leadtime"]]
        if (
          any(is.null(selected_time)) ||
            !all(is.element(selected_time, times[["times"]]))
        ) {
          selected_time <- times[["times"]][1]
        }

        shiny::insertUI(
          selector = paste0("#", ns("placeholder")),
          ui = shiny::tags$div(
            id = ns("ens-rank-hist"),
            shiny::selectInput(
              ns("ens-rank-hist-leadtime"),
              times[["time_label"]],
              times[["times"]],
              selected_time,
              multiple = TRUE
            )
          )
        )
      }
      more_selectors(FALSE)
    }

    if (ui_type() == "ens_hexbin") {

      if (!profile_verif) {
        times <- get_times(verif_data()[["ens_summary_scores"]], time_axis())

        selected_time <- input[["ens-hexbin-time"]]
        if (
          any(is.null(selected_time)) ||
            !all(is.element(selected_time, times[["times"]]))
        ) {
          selected_time <- times[["times"]][1]
        }
      }

      fcst_model <- unique(verif_data()[["ens_summary_scores"]][["fcst_model"]])
      selected_fcst_model <- input[["ens-hexbin-fcst-model"]]
      if (
        any(is.null(selected_fcst_model)) ||
          !all(is.element(selected_fcst_model, fcst_model))
      ) {
        selected_fcst_model <- fcst_model[1]
      }

      shiny::insertUI(
        selector = paste0("#", ns("placeholder")),
        ui = shiny::tags$div(
          id = ns("ens-hexbin")
        )
      )

      shiny::insertUI(
        selector = paste0("#", ns("ens-hexbin")),
        ui = shiny::checkboxGroupInput(
          ns("ens-hexbin-fcst-model"),
          "Forecast model",
          fcst_model,
          selected_fcst_model
        )
      )

      if (!profile_verif) {
        shiny::insertUI(
          selector = paste0("#", ns("ens-hexbin")),
          ui = shiny::selectInput(
            ns("ens-hexbin-time"),
            times[["time_label"]],
            times[["times"]],
            selected_time,
            multiple = TRUE
          )
        )
      }

      more_selectors(FALSE)
    }


    if (ui_type() == "ens_cat_choose_x") {
      shiny::insertUI(
        selector = paste0("#", ns("placeholder")),
        ui       = shiny::tags$div(
          id = ns("ens-cat-choose-x"),
          shiny::checkboxInput(
            ns("ens-extend-to-zero"),
            "y-axis to zero",
            TRUE
          ),
          shiny::radioButtons(
            ns("ens-cat-choose-x-x_axis"),
            "x axis",
            c("Lead Time" = "leadtime", "Threshold" = "threshold"),
            selected = "leadtime"
          )
        )
      )
      more_selectors(TRUE)
    }

    if (ui_type() == "ens_cat") {
      lead_time_var <- intersect(
        c("leadtime", "lead_time"),
        colnames(verif_data()[["ens_threshold_scores"]])
      )

      lt <- unique(verif_data()$ens_threshold_scores[[lead_time_var]])
      chr_leads  <- lt[lt != "All"]
      num_leads  <- harpCore::extract_numeric(chr_leads)
      lead_times <- c(
        lt[lt == "All"],
        chr_leads[order(match(num_leads, sort(num_leads)))]
      )
      thresholds <- parse_thresholds(
        unique(verif_data()$ens_threshold_scores$threshold)
      )
      shiny::insertUI(
        selector = paste0("#", ns("placeholder")),
        ui       = shiny::tags$div(
          id = ns("ens-cat"),
          shiny::selectInput(
            ns("ens-cat-leadtime"),
            "Lead Time",
            lead_times,
            lead_times[1],
            multiple = TRUE
          ),
          shiny::selectInput(
            ns("ens-cat-threshold"),
            "Threshold",
            thresholds,
            thresholds[1],
            multiple = TRUE
          )
        )
      )
      more_selectors(FALSE)
    }

    if (ui_type() == "det_summary") {

      shiny::insertUI(
        selector = paste0("#", ns("placeholder")),
        ui       = shiny::tags$div(
          id = ns("det-summary"),
          shiny::checkboxInput(
            ns("det-extend-to-zero"),
            "y-axis to zero",
            TRUE
          )
        )
      )

      if (any(nchar(all_members()) > 0)) {
        shiny::insertUI(
          selector = paste0("#", ns("det-summary")),
          where    = "beforeEnd",
          ui       = shiny::tags$div(
            id = ns("det-member-ens"),
            shiny::selectInput(
              ns("det-member"),
              "Highlight member",
              all_members(),
              all_members()[1],
              multiple = TRUE
            ),
            shiny::checkboxGroupInput(
              ns("det-models"),
              "Show models",
              all_models(),
              all_models()
            )
          )
        )
      } else {
        shiny::insertUI(
          selector = paste0("#", ns("det-summary")),
          where    = "beforeEnd",
          ui       = shiny::checkboxInput(
            ns("det-summary-num-cases"),
            "Show number of cases",
            FALSE
          )
        )
      }

      more_selectors(FALSE)

    }

    if (ui_type() == "det_hexbin") {

      if (!profile_verif) {
        times <- get_times(verif_data()[["det_summary_scores"]], time_axis())

        selected_time <- input[["det-hexbin-time"]]
        if (
          any(is.null(selected_time)) ||
            !all(is.element(selected_time, times[["times"]]))
        ) {
          selected_time <- times[["times"]][1]
        }
      }

      fcst_model <- unique(verif_data()[["det_summary_scores"]][["fcst_model"]])
      selected_fcst_model <- input[["det-hexbin-fcst-model"]]
      if (
        any(is.null(selected_fcst_model)) ||
          !all(is.element(selected_fcst_model, fcst_model))
      ) {
        selected_fcst_model <- fcst_model[1]
      }

      shiny::insertUI(
        selector = paste0("#", ns("placeholder")),
        ui = shiny::tags$div(
          id = ns("det-hexbin")
        )
      )

      shiny::insertUI(
        selector = paste0("#", ns("det-hexbin")),
        ui = shiny::checkboxGroupInput(
          ns("det-hexbin-fcst-model"),
          "Forecast model",
          fcst_model,
          selected_fcst_model
        )
      )

      if (!profile_verif) {
        shiny::insertUI(
          selector = paste0("#", ns("det-hexbin")),
          ui = shiny::selectInput(
            ns("det-hexbin-time"),
            times[["time_label"]],
            times[["times"]],
            selected_time,
            multiple = TRUE
          )
        )
      }

      if (any(nchar(all_members()) > 0)) {

        selected_member <- input[["det-hexbin-member"]]
        if (
          any(is.null(selected_member)) ||
            !all(is.element(selected_member, all_members()))
        ) {
          selected_member <- all_members()[1]
        }

        shiny::insertUI(
          selector = paste0("#", ns("det-hexbin")),
          ui = shiny::selectInput(
            ns("det-hexbin-member"),
            "Member",
            all_members(),
            selected_member,
            multiple = TRUE
          )
        )
      }

      more_selectors(FALSE)
    }

    if (ui_type() == "det_cat") {
      radio_choices <- c(time_axis(), "threshold")
      names(radio_choices) <- gsub(
        "Dttm",
        "Date-Time",
        totitle(gsub("_", " ", radio_choices))
      )
      shiny::insertUI(
        selector = paste0("#", ns("placeholder")),
        ui       = shiny::tags$div(
          id = ns("det-cat"),
          shiny::checkboxInput(
            ns("det-extend-to-zero"),
            "y-axis to zero",
            TRUE
          ),
          shiny::radioButtons(
            ns("det-cat-x_axis"),
            "x axis",
            radio_choices,
            selected = radio_choices[1]
          )
        )
      )
      more_selectors(TRUE)
    }

  })

  # Add extra UI selector if there is an x-axis selector

  shiny::observeEvent(list(input[["ens-cat-choose-x-x_axis"]], input[["det-cat-x_axis"]], more_selectors()), {

    if (more_selectors()) {

      if (thresh_types()) {
        thresh_elements <- grep("threshold", names(verif_data()))
        type_names <- unique(unlist(lapply(
          verif_data()[thresh_elements],
          function(x) unique(x[["Type"]])

        )))
      }

      if (ui_type() == "ens_cat_choose_x") {

        if (shiny::req(input[["ens-cat-choose-x-x_axis"]]) == "leadtime") {
          thresholds <- parse_thresholds(
            unique(verif_data()$ens_threshold_scores$threshold)
          )
          shiny::removeUI(paste0("#", ns("ens-cat-choose-x-lead")))
          shiny::insertUI(
            selector = paste0("#", ns("ens-cat-choose-x")),
            where    = "beforeEnd",
            ui       = shiny::tags$div(
              id = ns("ens-cat-choose-x-thresh"),
              shiny::selectInput(
                ns("ens-cat-choose-x-threshold"),
                "Threshold",
                thresholds,
                thresholds[1],
                multiple = TRUE
              )
            )
          )

        } else {

          lead_time_var <- intersect(
            c("leadtime", "lead_time"),
            colnames(verif_data()[["ens_threshold_scores"]])
          )
          lt <- unique(verif_data()$ens_threshold_scores[[lead_time_var]])
          chr_leads  <- lt[lt != "All"]
          num_leads  <- harpCore::extract_numeric(chr_leads)
          lead_times <- c(
            lt[lt == "All"],
            chr_leads[order(match(num_leads, sort(num_leads)))]
          )
          shiny::removeUI(paste0("#", ns("ens-cat-choose-x-thresh")))
          shiny::insertUI(
            selector = paste0("#", ns("ens-cat-choose-x")),
            where    = "beforeEnd",
            ui       = shiny::tags$div(
              id = ns("ens-cat-choose-x-lead"),
              shiny::selectInput(
                ns("ens-cat-choose-x-leadtime"),
                "Lead Time",
                lead_times,
                lead_times[1],
                multiple = TRUE
              )
            )
          )
        }

      }

      if (ui_type() == "det_cat") {

        if (shiny::req(input[["det-cat-x_axis"]]) != "threshold") {
          thresholds <- parse_thresholds(
            unique(verif_data()$det_threshold_scores$threshold)
          )
          shiny::removeUI(paste0("#", ns("det-cat-x-lead")))
          shiny::insertUI(
            selector = paste0("#", ns("det-cat")),
            where    = "beforeEnd",
            ui       = shiny::tags$div(
              id = ns("det-cat-x-thresh"),
              shiny::selectInput(
                ns("det-cat-x-threshold"),
                "Threshold",
                thresholds,
                thresholds[1],
                multiple = TRUE
              )
            )
          )

        } else {

          time_var <- intersect(
            time_axis(),
            colnames(verif_data()[["det_threshold_scores"]])
          )
          time_label <- gsub(
            "Dttm",
            "Date-Time",
            totitle(gsub("_", " ", time_var))
          )
          times <- unique(verif_data()$det_threshold_scores[[time_var]])
          add_All <- all_cols_all(verif_data(), "det_threshold_scores")
          select_times <- parse_times(times, time_var)
          if (!add_All) {
            select_times <- select_times[select_times != "All"]
          }
          shiny::removeUI(paste0("#", ns("det-cat-x-thresh")))
          shiny::insertUI(
            selector = paste0("#", ns("det-cat")),
            where    = "beforeEnd",
            ui       = shiny::tags$div(
              id = ns("det-cat-x-lead"),
              shiny::selectInput(
                ns("det-cat-x-time"),
                time_label,
                select_times,
                select_times[1],
                multiple = TRUE
              )
            )
          )

          if (thresh_types()) {
            shiny::insertUI(
              selector = paste0("#", ns("det-cat-x-lead")),
              where    = "afterBegin",
              ui       = shiny::tags$div(
                id = ns("det-cat-x-type"),
                shiny::selectInput(
                  ns("det-cat-x-type-select"),
                  "Type",
                  type_names,
                  type_names[1]
                )
              )
            )
          }
        }

      }

    } else {

      shiny::removeUI(paste0("#", ns("ens-cat-choose-x-leadtime")))
      shiny::removeUI(paste0("#", ns("ens-cat-choose-x-threshold")))
      shiny::removeUI(paste0("#", ns("det-cat-x-time")))
      shiny::removeUI(paste0("#", ns("det-cat-x-threshold")))

    }

  })


  # Collect the score options into a reactive object

  score_options <- shiny::eventReactive(
    list(
      ui_type(),
      time_axis(),
      input[["score"]],
      input[["ens-summary-num-cases"]],
      input[["ens-extend-to-zero"]],
      input[["ens-rank-hist-leadtime"]],
      input[["ens-hexbin-time"]],
      input[["ens-hexbin-fcst-model"]],
      input[["ens-cat-choose-x-x_axis"]],
      input[["ens-cat-choose-x-leadtime"]],
      input[["ens-cat-choose-x-threshold"]],
      input[["ens-cat-leadtime"]],
      input[["ens-cat-threshold"]],
      input[["det-extend-to-zero"]],
      input[["det-summary-num-cases"]],
      input[["det-member"]],
      input[["det-models"]],
      input[["det-hexbin-time"]],
      input[["det-hexbin-fcst-model"]],
      input[["det-hexbin-member"]],
      input[["det-cat-x_axis"]],
      input[["det-cat-x-time"]],
      input[["det-cat-x-threshold"]],
      input[["det-cat-x-type-select"]]
    ), {

      shiny::req(verif_data())
      shiny::req(input[["score"]])
      shiny::req(ui_type())
      highlight <- NULL

      fcst_model_col <- intersect(
        c("mname", "fcst_model"),
        Reduce(union, lapply(verif_data(), colnames))
      )

      is_profile <- attr(verif_data(), "is_profile")
      if (is.null(is_profile)) {
        is_profile <- FALSE
      }

      flip_axes <- FALSE
      num_cases_position <- "below"

      if (ui_type() == "ens_summary") {

        n_cases   <- input[["ens-summary-num-cases"]]
        to_zero   <- input[["ens-extend-to-zero"]]
        facets    <- NULL
        filters   <- NULL
        x_axis    <- ifelse(is_profile, "p", time_axis())
        line_cols <- "mname"
        if (is_profile) {
          flip_axes <- TRUE
          num_cases_position <- "right"
        }

      } else if (ui_type() == "ens_rank_hist") {

        n_cases   <- FALSE
        to_zero   <- TRUE
        if (!is_profile) {
          times     <- shiny::req(input[["ens-rank-hist-leadtime"]])
        }
        x_axis    <- time_axis()
        x_sym     <- rlang::sym(x_axis)
        line_cols <- "mname"

        if (!is_profile) {
          if (length(times) == 1) {
            if (times == "All") {
              facets  <- NULL
              filters <- ggplot2::vars(as.character(!!x_sym) != "All")
            } else {
              facets  <- NULL
              filters <- ggplot2::vars(as.character(!!x_sym) == times)
            }
          } else {
            if (is.element("All", times)) {
              facets  <- NULL
              filters <- ggplot2::vars(as.character(!!x_sym) %in% times[times != "All"])
            } else {
              facets  <- ggplot2::vars(!!x_sym)
              filters <- ggplot2::vars(as.character(!!x_sym) %in% times)
            }
          }
        } else {
          facets  <- NULL
          filters <- NULL
        }

      } else if (ui_type() == "ens_hexbin") {

        n_cases   <- FALSE
        to_zero   <- FALSE
        if (!is_profile) {
          times   <- shiny::req(input[["ens-hexbin-time"]])
        }
        f_model   <- shiny::req(input[["ens-hexbin-fcst-model"]])
        x_axis    <- time_axis()
        x_sym     <- rlang::sym(x_axis)
        fmc_sym   <- rlang::sym(fcst_model_col)
        line_cols <- "mname"

        if (is_profile) {
          if (length(f_model) == 1) {
            facets  <- NULL
            filters <- ggplot2::vars(!!fmc_sym == f_model)
          }
          if (length(f_model) > 1) {
            facets  <- ggplot2::vars(!!fmc_sym)
            filters <- ggplot2::vars(!!fmc_sym %in% f_model)
          }
        } else {
          if (length(times) == 1 && length(f_model) == 1) {
            facets  <- NULL
            filters <- ggplot2::vars(!!fmc_sym == f_model, !!x_sym == times)
          }
          if (length(times) > 1 && length(f_model) == 1) {
            facets  <- ggplot2::vars(!!x_sym)
            filters <- ggplot2::vars(!!fmc_sym == f_model, !!x_sym %in% times)
          }
          if (length(times) == 1 && length(f_model) > 1) {
            facets  <- ggplot2::vars(!!fmc_sym)
            filters <- ggplot2::vars(!!fmc_sym %in% f_model, !!x_sym == times)
          }
          if (length(times) > 1 && length(f_model) > 1) {
            facets  <- ggplot2::vars(!!x_sym, !!fmc_sym)
            filters <- ggplot2::vars(!!fmc_sym %in% f_model, !!x_sym %in% times)
          }
        }

      } else if (ui_type() == "ens_cat_choose_x") {

        to_zero   <- input[["ens-extend-to-zero"]]
        n_cases   <- FALSE
        x_axis    <- shiny::req(input[["ens-cat-choose-x-x_axis"]])
        line_cols <- "mname"

        if (x_axis == "leadtime") {
          thresholds <- shiny::req(input[["ens-cat-choose-x-threshold"]])
          if (length(thresholds) == 1) {
            facets  <- NULL
            filters <- ggplot2::vars(as.character(threshold) == thresholds)
          } else {
            facets  <- ggplot2::vars(threshold)
            filters <- ggplot2::vars(as.character(threshold) %in% thresholds)
          }
        } else {
          leadtimes <- shiny::req(input[["ens-cat-choose-x-leadtime"]])
          if (length(leadtimes) == 1) {
            facets  <- NULL
            filters <- ggplot2::vars(as.character(leadtime) == leadtimes)
          } else {
            facets  <- ggplot2::vars(leadtime)
            filters <- ggplot2::vars(as.character(leadtime) %in% leadtimes)
          }
        }

      } else if (ui_type() == "ens_cat") {

        to_zero    <- FALSE
        n_cases    <- FALSE
        x_axis     <- time_axis()
        line_cols  <- "mname"
        leadtimes  <- shiny::req(input[["ens-cat-leadtime"]])
        thresholds <- shiny::req(input[["ens-cat-threshold"]])
        if (length(thresholds) == 1 & length(leadtimes) == 1) {
          facets  <- NULL
          filters <- ggplot2::vars(as.character(leadtime) == leadtimes, as.character(threshold) == thresholds)
        } else {
          facets  <- ggplot2::vars(leadtime, threshold)
          filters <- ggplot2::vars(as.character(leadtime) %in% leadtimes, as.character(threshold) %in% thresholds)
        }

      } else if (ui_type() == "det_summary") {

        if (is.null(input[["det-extend-to-zero"]])) return()

        n_cases   <- TRUE
        to_zero   <- input[["det-extend-to-zero"]]
        facets    <- NULL
        filters   <- NULL
        x_axis    <- ifelse(is_profile, "p", time_axis())
        line_cols <- "mname"
        if (is_profile) {
          flip_axes <- TRUE
          num_cases_position <- "right"
        }
        if (det_ens()) {
          n_cases   <- FALSE
          facets    <- ggplot2::vars({{fcst_model_col}})
          if (fcst_model_col == "mname") {
            filters <- ggplot2::vars(mname %in% shiny::req(input[["det-models"]]))
          } else {
            filters <- ggplot2::vars(fcst_model %in% shiny::req(input[["det-models"]]))
          }
          line_cols <- "member"
          highlight <- input[["det-member"]]
        } else {
          n_cases   <- input[["det-summary-num-cases"]]
        }

      } else if (ui_type() == "det_hexbin") {

        n_cases   <- FALSE
        to_zero   <- FALSE
        if (!is_profile) {
          times   <- shiny::req(input[["det-hexbin-time"]])
          n_tim   <- length(times)
        }
        f_model   <- shiny::req(input[["det-hexbin-fcst-model"]])
        x_axis    <- time_axis()
        x_sym     <- rlang::sym(x_axis)
        fmc_sym   <- rlang::sym(fcst_model_col)
        line_cols <- "mname"

        n_mod <- length(f_model)

        if (det_ens()) {

          members <- shiny::req(input[["det-hexbin-member"]])

          n_mem <- length(members)
          mem_sym <- rlang::sym("member")

          if (is_profile) {

            if (n_mod == 1 && n_mem == 1) {
              facets  <- NULL
              filters <- ggplot2::vars(
                !!fmc_sym == f_model, !!mem_sym == members
              )
            }
            if (n_mod > 1 && n_mem == 1) {
              facets  <- ggplot2::vars(!!fmc_sym)
              filters <- ggplot2::vars(!!mem_sym == members)
            }
            if (n_mod == 1 && n_mem > 1) {
              facets  <- ggplot2::vars(!!mem_sym)
              filters <- ggplot2::vars(
                !!fmc_sym == f_model, !!mem_sym %in% members
              )
            }
            if (n_mod > 1 && n_mem > 1) {
              facets  <- ggplot2::vars(!!fmc_sym, !!mem_sym)
              filters <- ggplot2::vars(
                !!fmc_sym %in% f_model, !!mem_sym %in% members
              )
            }

          } else {

            if (n_tim == 1 && n_mod == 1 && n_mem == 1) {
              facets  <- NULL
              filters <- ggplot2::vars(
                !!fmc_sym == f_model, !!x_sym == times, !!mem_sym == members
              )
            }
            if (n_tim > 1 && n_mod == 1 && n_mem == 1) {
              facets  <- ggplot2::vars(!!x_sym)
              filters <- ggplot2::vars(
                !!fmc_sym == f_model, !!x_sym %in% times, !!mem_sym == members
              )

            }
            if (n_tim == 1 && n_mod > 1 && n_mem == 1) {
              facets  <- ggplot2::vars(!!fmc_sym)
              filters <- ggplot2::vars(
                !!fmc_sym %in% f_model, !!x_sym == times, !!mem_sym == members
              )
            }
            if (n_tim > 1 && n_mod > 1 && n_mem == 1) {
              facets  <- ggplot2::vars(!!x_sym, !!fmc_sym)
              filters <- ggplot2::vars(
                !!fmc_sym %in% f_model, !!x_sym %in% times, !!mem_sym == members
              )
            }
            if (n_tim == 1 && n_mod == 1 && n_mem > 1) {
              facets  <- ggplot2::vars(!!mem_sym)
              filters <- ggplot2::vars(
                !!fmc_sym == f_model, !!x_sym == times, !!mem_sym %in% members
              )
            }
            if (n_tim > 1 && n_mod == 1 && n_mem > 1) {
              facets  <- ggplot2::vars(!!x_sym, !!mem_sym)
              filters <- ggplot2::vars(
                !!fmc_sym == f_model, !!x_sym %in% times, !!mem_sym %in% members
              )

            }
            if (n_tim == 1 && n_mod > 1 && n_mem > 1) {
              facets  <- ggplot2::vars(!!fmc_sym, !!mem_sym)
              filters <- ggplot2::vars(
                !!fmc_sym %in% f_model, !!x_sym == times, !!mem_sym %in% members
              )
            }
            if (n_tim > 1 && n_mod > 1 && n_mem > 1) {
              facets  <- ggplot2::vars(!!x_sym, !!fmc_sym, !!mem_sym)
              filters <- ggplot2::vars(
                !!fmc_sym %in% f_model, !!x_sym %in% times,
                !!mem_sym %in% members
              )
            }

          }

        } else {

          if (is_profile) {
            if (n_mod == 1) {
              facets  <- NULL
              filters <- ggplot2::vars(!!fmc_sym == f_model)
            }
            if (n_mod > 1) {
              facets  <- ggplot2::vars(!!fmc_sym)
              filters <- ggplot2::vars(!!fmc_sym %in% f_model)
            }
          } else {
            if (n_tim == 1 && n_mod == 1) {
              facets  <- NULL
              filters <- ggplot2::vars(!!fmc_sym == f_model, !!x_sym == times)
            }
            if (n_tim > 1 && n_mod == 1) {
              facets  <- ggplot2::vars(!!x_sym)
              filters <- ggplot2::vars(!!fmc_sym == f_model, !!x_sym %in% times)
            }
            if (n_tim == 1 && n_mod > 1) {
              facets  <- ggplot2::vars(!!fmc_sym)
              filters <- ggplot2::vars(!!fmc_sym %in% f_model, !!x_sym == times)
            }
            if (n_tim > 1 && n_mod > 1) {
              facets  <- ggplot2::vars(!!x_sym, !!fmc_sym)
              filters <- ggplot2::vars(!!fmc_sym %in% f_model, !!x_sym %in% times)
            }
          }

        }

      } else if (ui_type() == "det_cat") {

        to_zero   <- input[["det-extend-to-zero"]]
        n_cases   <- FALSE
        x_axis    <- shiny::req(input[["det-cat-x_axis"]])
        line_cols <- "mname"

        if (x_axis != "threshold") {
          thresholds <- shiny::req(input[["det-cat-x-threshold"]])
          if (length(thresholds) == 1) {
            facets  <- NULL
            filters <- ggplot2::vars(as.character(threshold) == thresholds)
          } else {
            facets  <- ggplot2::vars(threshold)
            filters <- ggplot2::vars(as.character(threshold) %in% thresholds)
          }
        } else {
          times <- shiny::req(input[["det-cat-x-time"]])
          if (length(times) == 1) {
            facets  <- NULL
            filters <- ggplot2::vars(
              as.character(.data[[time_axis()]]) == times
            )
          } else {
            facets  <- ggplot2::vars(.data[[time_axis()]])
            filters <- ggplot2::vars(
              as.character(.data[[time_axis()]]) %in% times
            )
          }
          if (thresh_types()) {
            thresh_type <- shiny::req(input[["det-cat-x-type-select"]])
            filters <- c(filters, vars(Type == thresh_type))
          }
        }

      }

      list(
        score       = input[["score"]],
        num_cases   = n_cases,
        to_y_zero   = to_zero,
        x_axis      = x_axis,
        facets      = facets,
        filters     = filters,
        line_cols   = line_cols,
        highlight   = highlight,
        flip_axes   = flip_axes,
        n_cases_pos = num_cases_position
      )

    }

  )

  # make the plot

  output$plot <- shiny::renderPlot({
    shiny::req(score_options())
    shiny::req(verif_data())

    score_type <- strsplit(score_options()$score, "_")[[1]][1]
    plot_score <- gsub("^[[:alpha:]]+_[[:alpha:]]+_scores_", "", score_options()$score)

    score_name  <- plot_score
    plot_score  <- rlang::sym(plot_score)
    plot_x_axis <- rlang::sym(score_options()$x_axis)

    aspect_ratio <- NULL
    if (score_options()$flip_axes) {
      aspect_ratio <- 1.25
    }

    if (score_options()$line_cols == "mname") {

      line_cols  <- rlang::sym(score_options()$line_cols)
      score_plot <- harpVis::plot_point_verif(
        verif_data(),
        !!plot_score,
        verif_type         = score_type,
        x_axis             = !!plot_x_axis,
        colour_by          = !!line_cols,
        plot_num_cases     = score_options()$num_cases,
        extend_y_to_zero   = score_options()$to_y_zero,
        facet_by           = score_options()$facets,
        filter_by          = score_options()$filters,
        rank_is_relative   = TRUE,
        rank_hist_type     = "lollipop",
        colour_theme       = plot_theme,
        colour_table       = colour_table(),
        num_cases_position = score_options()$n_cases_pos,
        flip_axes          = score_options()$flip_axes
      ) +
        ggplot2::theme(aspect.ratio = aspect_ratio)

    } else {

      line_cols <- rlang::sym("member_highlight")
      plot_data <- shiny::req(verif_data())
      if (is.null(score_options()$highlight)) return()

      plot_data[["det_summary_scores"]][["member_highlight"]] <- forcats::fct_other(
        plot_data[["det_summary_scores"]][["member"]],
        keep        = score_options()$highlight,
        other_level = "Other members"
      )

      highlight_cols <- c(RColorBrewer::brewer.pal(8, "Set1"), RColorBrewer::brewer.pal(8, "Dark2"))
      highlight_mems <- c(
        paste0("mbr", formatC(seq(0, 1500), width = 3, flag = "0")),
        paste0("mbr", formatC(seq(0, 1500), width = 3, flag = "0"), "_lag"),
        "Other members"
      )
      num_vec        <- floor(length(highlight_mems) / length(highlight_cols))
      num_xtra       <- length(highlight_mems) %% length(highlight_cols)
      all_cols       <- rep(highlight_cols, num_vec)

      if (num_xtra > 0) {
        all_cols <- c(all_cols, highlight_cols[1:num_xtra])
      }

      member_cols <- data.frame(
        member_highlight = highlight_mems,
        colour           = all_cols,
        stringsAsFactors = FALSE
      )

      member_cols[["colour"]][grep("Other", member_cols[["member_highlight"]])] <- "grey70"
      member_cols <- dplyr::filter(
        member_cols,
        .data[["member_highlight"]] %in% c(score_options()$highlight, "Other members")
      )

      score_plot <- harpVis::plot_point_verif(
        plot_data,
        !! plot_score,
        verif_type         = score_type,
        x_axis             = !!plot_x_axis,
        colour_by          = !!line_cols,
        plot_num_cases     = score_options()$num_cases,
        extend_y_to_zero   = score_options()$to_y_zero,
        facet_by           = score_options()$facets,
        filter_by          = score_options()$filters,
        colour_theme       = plot_theme,
        colour_table       = member_cols,
        group              = member,
        num_cases_position = score_options()$n_cases_pos,
        flip_axes          = score_options()$flip_axes
      ) +
        ggplot2::theme(aspect.ratio = aspect_ratio)

      all_highlights <- grep(
        "mbr[[:digit:]]+$",
        levels(plot_data[["det_summary_scores"]][["member_highlight"]]),
        value = TRUE
      )

      for (highlight_member in all_highlights) {
        group_colour <- member_cols[["colour"]][member_cols[["member_highlight"]] == highlight_member]
        group_data   <- dplyr::filter(
          plot_data[["det_summary_scores"]],
          .data[["member"]] == highlight_member,
          !!!score_options()$filters
        ) %>%
          dplyr::rename_with(
            ~suppressWarnings(harpCore::psub(
              .x, c("^leadtime$", "^mname$"), c("lead_time", "fcst_model")
            ))
          )
        group_data <- filter_for_x(
          group_data, score_options()$x_axis,
          flip_axes = score_options()$flip_axes,
          facet_vars = score_options()$facets
        )
        score_plot <- score_plot +
          ggplot2::geom_line(data  = group_data, colour = group_colour, size = 1.1) +
          ggplot2::geom_point(data = group_data, colour = group_colour, size = 2, show.legend = FALSE)
      }

    }

    score_plot

  }, height = 550, bg = bg_colour, res = 96)

  return(score_options)

}


### Helper functions.

# Populate the score selector with formatted names

make_score_list <- function(verif_list) {

  group_cols <- attr(verif_list, "group_cols")
  verif_list <- verif_list[which(purrr::map_int(verif_list, nrow) > 0)]

  verif_names <- purrr::map(
    verif_list,
    ~setdiff(
      colnames(.x),
      union(get("meta_colnames", envir = harpVis_data), group_cols)
    )
  )


  # Add derived scores if the required data are available in verif_list

  if (!is.null(verif_list$det_summary_scores)) {

    if (
      "bias" %in% verif_names$det_summary_scores &&
      "rmse" %in% verif_names$det_summary_scores
    ) {
      verif_names$det_summary_scores <- c(
        verif_names$det_summary_scores, "bias_rmse"
      )
    }

    if (
      "bias" %in% verif_names$det_summary_scores &&
      "stde" %in% verif_names$det_summary_scores
    ) {
      verif_names$det_summary_scores <- c(
        verif_names$det_summary_scores, "bias_stde"
      )
    }

    if (
      "mean_fcst" %in% verif_names$det_summary_scores &&
      "mean_obs" %in% verif_names$det_summary_scores
    ) {
      verif_names$det_summary_scores <- c(
        verif_names$det_summary_scores, "fcst_obs"
      )
    }

  }

  if (!is.null(verif_list$ens_summary_scores)) {
    if ("spread" %in% verif_names$ens_summary_scores && "rmse" %in% verif_names$ens_summary_scores) {
      verif_names$ens_summary_scores <- c(
        verif_names$ens_summary_scores,
        "spread_skill",
        "spread_skill_ratio"
      )
    }
    if ("spread" %in% verif_names$ens_summary_scores && "stde" %in% verif_names$ens_summary_scores) {
      verif_names$ens_summary_scores <- c(
        verif_names$ens_summary_scores,
        "spread_stde",
        "spread_stde_ratio"
      )
    }
    if ("dropped_members_spread" %in% verif_names$ens_summary_scores && "rmse" %in% verif_names$ens_summary_scores) {
      verif_names$ens_summary_scores <- c(
        verif_names$ens_summary_scores,
        "spread_skill_with_dropped",
        "spread_skill_ratio_with_dropped"
      )
    }
    if ("rank_histogram" %in% verif_names$ens_summary_scores) {
      verif_names$ens_summary_scores <- c(verif_names$ens_summary_scores, "normalized_rank_histogram")
    }
  }

  if (!is.null(verif_list$ens_threshold_scores)) {
    brier_decomp <-  paste(
      sort(c("brier_score_resolution", "brier_score_reliability", "brier_score_uncertainty")),
      collapse = ".&."
    )
    if (grepl(brier_decomp, paste(sort(verif_names$ens_threshold_scores), collapse = ".&."))) {
      verif_names$ens_threshold_scores <- c(verif_names$ens_threshold_scores, "brier_score_decomposition")
    }
    if ("reliability" %in% verif_names$ens_threshold_scores) {
      verif_names$ens_threshold_scores <- c(verif_names$ens_threshold_scores, "sharpness")
    }
  }

  # Remove standard errors in anticipation of a better way of dealing with them
  if (!is.null(verif_names$det_summary_scores)) {
    verif_names$det_summary_scores <- grep(
      "_std_error", verif_names$det_summary_scores, value = TRUE, invert = TRUE
    )
  }
  if (!is.null(verif_names$det_threshold_scores)) {
    verif_names$det_threshold_scores <- grep(
      "_std_error", verif_names$det_threshold_scores, value = TRUE, invert = TRUE
    )
  }

  # Format for use in the selectInput for Score

  verif_types <- harpVis:::totitle(gsub("_", " ", names(verif_names)))

  verif_names <- purrr::map2(
    names(verif_names),
    verif_names,
    function(x, y) {
      y                  <- sort(y)
      score_names        <- paste(x, y, sep = "_")
      names(score_names) <- harpVis:::totitle(gsub("_", " ", gsub("num_", "number_of_", y)))
      score_names
    }
  )

  names(verif_names) <- verif_types

  verif_names

}

# Get the name of the type of UI needed for a given score

get_ui_type <- function(verif_name) {

  ens_cat_choose_x <- paste(
    c("brier", "roc_area", "threshold", "climatology", "tw_crps"), collapse = "|"
  )

  if (grepl("ens_summary", verif_name)) {
    if (grepl("rank_histogram", verif_name)) {
      "ens_rank_hist"
    } else if (grepl("hexbin", verif_name)) {
      "ens_hexbin"
    } else {
      "ens_summary"
    }
  } else if (grepl("ens_threshold", verif_name)) {
    if (grepl(ens_cat_choose_x, gsub("ens_threshold", "", verif_name))) {
      "ens_cat_choose_x"
    } else {
      "ens_cat"
    }
  } else if (grepl("det_summary", verif_name)) {
    if (grepl("hexbin", verif_name)) {
      "det_hexbin"
    } else {
      "det_summary"
    }
  } else if (grepl("det_threshold", verif_name)) {
    "det_cat"
  } else {
    NULL
  }

}

get_times <- function(df, time_var) {

  time_var <- intersect(
    time_var,
    colnames(df)
  )

  time_label <- gsub(
    "Dttm", "Date-Time", totitle(gsub("_", " ", time_var))
  )

  times_in <- as.character(unique(df[[time_var]]))

  times <- times_in[grep("All|;", times_in, invert = TRUE)]
  if (grepl("dttm", time_var)) {
    times_dttm <- do.call(c, lapply(times, as.POSIXct, tz = "UTC"))
    times <- times[order(match(times, times_dttm))]
    names(times) <- format(times_dttm, "%H:%M %d %b %Y")
  } else {
    times <- sort(as.numeric(times))
  }
  times <- c(times_in[grep("All|;", times_in)], times)
  list(times = times, time_label = time_label)
}

# Function to see if "All" should be included in lead time selection
all_cols_all <- function(.verif_data, el) {
  group_cols <- grep(
    "threshold",
    Reduce(union, attr(.verif_data, "group_vars")),
    inv = TRUE,
    value = TRUE
  )
  nrow(
    dplyr::filter(
      .verif_data[[el]],
      dplyr::if_all(dplyr::all_of(group_cols), ~grepl("All|; ", .x))
    )
  ) > 0
}
