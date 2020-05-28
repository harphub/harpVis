# Shiny module for interactive score plotting

#' Title
#'
#' @param id
#'
#' @return
#' @export
#'
#' @examples
interactive_epsUI <- function(id) {

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


#' Title
#'
#' @param input
#' @param output
#' @param session
#' @param verif_data
#' @param bg_colour
#'
#' @return
#' @export
#'
#' @examples
interactive_eps <- function(input, output, session, verif_data, colour_table, bg_colour) {

  ns <- session$ns

  new_data <- shiny::reactiveVal(NULL)

  # Get the ui type we need from the score

  ui_type        <- shiny::reactiveVal("ens_summary")
  more_selectors <- shiny::reactiveVal(FALSE)
  det_ens        <- shiny::reactiveVal(FALSE)
  all_members    <- shiny::reactiveVal("")
  all_models     <- shiny::reactiveVal("")

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
      new_data(0)
    } else {
      new_data(new_data() + 1)
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
    if (!is.null(det_df) && is.element("member", colnames(det_df))) {
      det_ens(TRUE)
    } else {
      det_ens(FALSE)
    }
  })

  shiny::observeEvent(input$score, {
    ui_type(get_ui_type(input$score))
  })

  shiny::observeEvent(list(det_ens(), new_data()), {
    if (det_ens()) {
      all_members(sort(unique(verif_data()[["det_summary_scores"]][["member"]])))
      all_models(sort(unique(verif_data()[["det_summary_scores"]][["mname"]])))
    } else {
      all_members("")
      all_models("")
    }
  })

  # Update the UI based on the score

  shiny::observeEvent(list(ui_type(), all_members()), {

    shiny::req(ui_type())

    if (ui_type() == "ens_summary") {
      shiny::removeUI(paste0("#", ns("ens-rank-hist")))
      shiny::removeUI(paste0("#", ns("ens-cat")))
      shiny::removeUI(paste0("#", ns("ens-cat-choose-x")))
      shiny::removeUI(paste0("#", ns("det-summary")))
      shiny::removeUI(paste0("#", ns("det-cat")))
      shiny::insertUI(
        selector = paste0("#", ns("placeholder")),
        ui       = shiny::tags$div(
          id = ns("ens-summary"),
          shiny::checkboxInput(
            ns("ens-extend-to-zero"),
            "y-axis to zero",
            TRUE
          ),
          shiny::checkboxInput(
            ns("ens-summary-num-cases"),
            "Show number of cases",
            FALSE
          )
        )
      )
      more_selectors(FALSE)
    }

    if (ui_type() == "ens_rank_hist") {
      lead_times <- c("All", sort(unique(verif_data()$ens_summary_scores$leadtime)))
      shiny::removeUI(paste0("#", ns("ens-summary")))
      shiny::removeUI(paste0("#", ns("ens-cat")))
      shiny::removeUI(paste0("#", ns("ens-cat-choose-x")))
      shiny::removeUI(paste0("#", ns("det-summary")))
      shiny::removeUI(paste0("#", ns("det-cat")))
      shiny::insertUI(
        selector = paste0("#", ns("placeholder")),
        ui       = shiny::tags$div(
          id = ns("ens-rank-hist"),
          shiny::selectInput(
            ns("ens-rank-hist-leadtime"),
            "Lead Time",
            lead_times,
            lead_times[1],
            multiple = TRUE
          )
        )
      )
      more_selectors(FALSE)
    }

    if (ui_type() == "ens_cat_choose_x") {
      shiny::removeUI(paste0("#", ns("ens-summary")))
      shiny::removeUI(paste0("#", ns("ens-cat")))
      shiny::removeUI(paste0("#", ns("ens-rank-hist")))
      shiny::removeUI(paste0("#", ns("det-summary")))
      shiny::removeUI(paste0("#", ns("det-cat")))
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
      lead_times <- sort(unique(verif_data()$ens_threshold_scores$leadtime))
      thresholds <- sort(unique(verif_data()$ens_threshold_scores$threshold))
      shiny::removeUI(paste0("#", ns("ens-summary")))
      shiny::removeUI(paste0("#", ns("ens-cat-choose-x")))
      shiny::removeUI(paste0("#", ns("ens-rank-hist")))
      shiny::removeUI(paste0("#", ns("det-summary")))
      shiny::removeUI(paste0("#", ns("det-cat")))
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
      shiny::removeUI(paste0("#", ns("ens-rank-hist")))
      shiny::removeUI(paste0("#", ns("ens-cat")))
      shiny::removeUI(paste0("#", ns("ens-cat-choose-x")))
      shiny::removeUI(paste0("#", ns("ens-summary")))
      shiny::removeUI(paste0("#", ns("det-cat")))
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
      if (length(all_members() > 0)) {
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
      }
      more_selectors(FALSE)
    }

  })

  # Add extra UI selector if there is an x-axis selector

  shiny::observeEvent(list(input[["ens-cat-choose-x-x_axis"]], more_selectors()), {

    if (more_selectors()) {

      if (shiny::req(input[["ens-cat-choose-x-x_axis"]]) == "leadtime") {
        thresholds <- sort(unique(verif_data()$ens_threshold_scores$threshold))
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

        lead_times <- sort(unique(verif_data()$ens_threshold_scores$leadtime))
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

    } else {

      shiny::removeUI(paste0("#", ns("ens-cat-choose-x-leadtime")))
      shiny::removeUI(paste0("#", ns("ens-cat-choose-x-threshold")))

    }

  })

  # Collect the score options into a reactive object

  score_options <- shiny::eventReactive(
    list(
      ui_type(),
      input[["score"]],
      input[["ens-summary-num-cases"]],
      input[["ens-extend-to-zero"]],
      input[["ens-rank-hist-leadtime"]],
      input[["ens-cat-choose-x-x_axis"]],
      input[["ens-cat-choose-x-leadtime"]],
      input[["ens-cat-choose-x-threshold"]],
      input[["ens-cat-leadtime"]],
      input[["ens-cat-threshold"]],
      input[["det-extend-to-zero"]],
      input[["det-member"]],
      input[["det-models"]]
    ), {

      shiny::req(verif_data())
      shiny::req(input[["score"]])
      shiny::req(ui_type())
      highlight <- NULL

      if (ui_type() == "ens_summary") {

        n_cases   <- input[["ens-summary-num-cases"]]
        to_zero   <- input[["ens-extend-to-zero"]]
        facets    <- NULL
        filters   <- NULL
        x_axis    <- "leadtime"
        line_cols <- "mname"

      } else if (ui_type() == "ens_rank_hist") {

        n_cases   <- FALSE
        to_zero   <- TRUE
        leadtimes <- shiny::req(input[["ens-rank-hist-leadtime"]])
        x_axis    <- "leadtime"
        line_cols <- "mname"

        if (length(leadtimes) == 1) {
          if (leadtimes == "All") {
            facets  <- NULL
            filters <- NULL
          } else {
            facets  <- NULL
            filters <- ggplot2::vars(leadtime == as.numeric(leadtimes))
          }
        } else {
          if (is.element("All", leadtimes)) {
            facets  <- NULL
            filters <- ggplot2::vars(leadtime %in% as.numeric(leadtimes[leadtimes != "All"]))
          } else {
            facets  <- ggplot2::vars(leadtime)
            filters <- ggplot2::vars(leadtime %in% as.numeric(leadtimes))
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
            filters <- ggplot2::vars(threshold == as.numeric(thresholds))
          } else {
            facets  <- ggplot2::vars(threshold)
            filters <- ggplot2::vars(threshold %in% as.numeric(thresholds))
          }
        } else {
          leadtimes <- shiny::req(input[["ens-cat-choose-x-leadtime"]])
          if (length(leadtimes) == 1) {
            facets  <- NULL
            filters <- ggplot2::vars(leadtime == as.numeric(leadtimes))
          } else {
            facets  <- ggplot2::vars(leadtime)
            filters <- ggplot2::vars(leadtime %in% as.numeric(leadtimes))
          }
        }

      } else if (ui_type() == "ens_cat") {

        to_zero    <- FALSE
        n_cases    <- FALSE
        x_axis     <- "leadtime"
        line_cols  <- "mname"
        leadtimes  <- shiny::req(input[["ens-cat-leadtime"]])
        thresholds <- shiny::req(input[["ens-cat-threshold"]])
        if (length(thresholds) == 1 & length(leadtimes) == 1) {
          facets  <- NULL
          filters <- ggplot2::vars(leadtime == as.numeric(leadtimes), threshold == as.numeric(thresholds))
        } else {
          facets  <- ggplot2::vars(leadtime, threshold)
          filters <- ggplot2::vars(leadtime %in% as.numeric(leadtimes), threshold %in% as.numeric(thresholds))
        }

      } else if (ui_type() == "det_summary") {

        if (is.null(input[["det-extend-to-zero"]])) return()

        n_cases   <- TRUE
        to_zero   <- input[["det-extend-to-zero"]]
        facets    <- NULL
        filters   <- NULL
        x_axis    <- "leadtime"
        line_cols <- "mname"
        if (det_ens()) {
          n_cases   <- FALSE
          facets    <- ggplot2::vars(mname)
          filters   <- ggplot2::vars(mname %in% shiny::req(input[["det-models"]]))
          line_cols <- "member"
          highlight <- input[["det-member"]]
        }
      }

      list(
        score     = input[["score"]],
        num_cases = n_cases,
        to_y_zero = to_zero,
        x_axis    = x_axis,
        facets    = facets,
        filters   = filters,
        line_cols = line_cols,
        highlight = highlight
      )

    }

  )

  # make the plot

  output$plot <- shiny::renderPlot({
    shiny::req(score_options())

    score_type <- strsplit(score_options()$score, "_")[[1]][1]
    plot_score <- gsub("^[[:alpha:]]+_[[:alpha:]]+_scores_", "", score_options()$score)

    plot_score  <- rlang::sym(plot_score)
    plot_x_axis <- rlang::sym(score_options()$x_axis)

    if (score_options()$line_cols == "mname") {

      line_cols  <- rlang::sym(score_options()$line_cols)
      score_plot <- harpVis::plot_point_verif(
        shiny::req(verif_data()),
        !!plot_score,
        verif_type       = score_type,
        x_axis           = !!plot_x_axis,
        colour_by        = !!line_cols,
        plot_num_cases   = score_options()$num_cases,
        extend_y_to_zero = score_options()$to_y_zero,
        facet_by         = score_options()$facets,
        filter_by        = score_options()$filters,
        colour_theme     = "harp_midnight",
        colour_table     = colour_table()
      )

    } else {

      line_cols <- rlang::sym("member_highlight")
      plot_data <- shiny::req(verif_data())

      plot_data[["det_summary_scores"]][["member_highlight"]] <- forcats::fct_other(
        plot_data[["det_summary_scores"]][["member"]],
        keep        = score_options()$highlight,
        other_level = "Other members"
      )

      highlight_cols <- c(RColorBrewer::brewer.pal(8, "Set1"), RColorBrewer::brewer.pal(8, "Dark2"))
      highlight_mems <- c(
        paste0("mbr", formatC(seq(0, 1500), width = 3, flag = "0")),
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
        verif_type       = score_type,
        x_axis           = !!plot_x_axis,
        colour_by        = !!line_cols,
        plot_num_cases   = score_options()$num_cases,
        extend_y_to_zero = score_options()$to_y_zero,
        facet_by         = score_options()$facets,
        filter_by        = score_options()$filters,
        colour_theme     = "harp_midnight",
        colour_table     = member_cols,
        group            = member
      )

      all_highlights <- grep(
        "mbr[[:digit:]]+$",
        levels(plot_data[["det_summary_scores"]][["member_highlight"]]),
        value = TRUE
      )

      for (highlight_member in all_highlights) {
        group_colour <- member_cols[["colour"]][member_cols[["member_highlight"]] == highlight_member]
        group_data   <- dplyr::filter(
          plot_data[["det_summary_scores"]],
          member == highlight_member,
          !!!score_options()$filters
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

  verif_list <- verif_list[which(purrr::map_int(verif_list, nrow) > 0)]

  verif_names <- purrr::map(
    verif_list,
    ~setdiff(names(.x), c("mname", "leadtime", "threshold", "member"))
  )

  # Add derived scores if the required data are available in verif_list

  if (!is.null(verif_list$ens_summary_scores)) {
    if ("spread" %in% verif_names$ens_summary_scores && "rmse" %in% verif_names$ens_summary_scores) {
      verif_names$ens_summary_scores <- c(
        verif_names$ens_summary_scores,
        "spread_skill",
        "spread_skill_ratio"
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

  ens_cat_choose_x <- paste(c("brier", "roc_area", "threshold", "climatology"), collapse = "|")

  if (grepl("ens_summary", verif_name)) {
    if (grepl("rank_histogram", verif_name)) {
      "ens_rank_hist"
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
    "det_summary"
  } else if (grepl("det_threshold", verif_name)) {
    "det_cat"
  } else {
    NULL
  }

}
