#' @param id An ID string for the module's UI
#' @rdname colour_choices
#'
#' @export
colour_choicesUI <- function(id) {

  ns <- shiny::NS(id)

  shiny::div(class = "col-sm-2 col-lg-1", id = ns("plot_button_div"),
    shiny::actionButton(ns("plot_options"), "", icon = shiny::icon("palette"), width = "50%")
  )

}

#' Shiny module for picking colours
#'
#' The UI includes a button with a palette icon for opening a modal to pick
#' colours for a plot. The output is a data frame that can be used to control
#' colours in plotting point verification scores using the
#' \code{\link{plot_point_verif}} function.
#'
#' Note that the module uses the "old" semantics for shiny modules so should be
#' called with \code{\link[shiny]{callModule}} rather than
#' \code{\link[shiny]{moduleServer}}.
#'
#' @param input input reactive
#' @param output output reactive
#' @inheritParams shiny::moduleServer
#' @param verif_data verification data as a reactive value
#'
#' @return A reactive data frame to be used as input to the `colour_table`
#'   argument in \code{\link{plot_point_verif}}.
#'
#' @export
#' @examples
#' library(shiny)
#'
#' ui <- fluidPage(
#'   colour_choicesUI("cols"),
#'   plotOutput("verifPlot")
#' )
#'
#' server <- function(input, output, session) {
#'   col_table <- callModule(colour_choices, "cols", reactive(verif_data_ens))
#'   output$verifPlot <- renderPlot({
#'     plot_point_verif(
#'       verif_data_ens, spread, colour_table = col_table(),
#'       plot_num_cases = FALSE
#'     )
#'   })
#' }
#'
#' if (interactive()) {
#'   shinyApp(ui, server)
#' }
#'
#'
colour_choices <- function(input, output, session, verif_data) {

  ns <- session$ns

  # Initialise colour palette
  selected_palette <- shiny::reactiveVal("Paired")
  palette_colours  <- shiny::reactiveVal(RColorBrewer::brewer.pal(12, "Paired")[c(4, 8, 10, 1:3, 5:7, 9, 11:12)])

  # Update the palette colours when a new palette is chosen. For custom palette use the
  # "default" colours from the Paired palette.
  shiny::observeEvent(input$colour_palette, {
    shiny::req(input$colour_palette)
    if (input$colour_palette == "Paired") {
      available_colours <- RColorBrewer::brewer.pal(12, "Paired")
      available_colours <- available_colours[c(4, 8, 10, 1:3, 5:7, 9, 11:12)]
    } else if (input$colour_palette == "harp2") {
      available_colours <- c(
        "#CCCCCC",
        "#E69F00",
        "#56B4E9",
        "#009E73",
        "#F0E442",
        "#0072B2",
        "#D55E00",
        "#CC79A7",
        "#FF0000",
        "#A50026",
        "#111111"
      )
    } else if (input$colour_palette == "Custom") {
      fcst_model_col <- intersect(
        c("mname", "fcst_model"),
        Reduce(union, lapply(verif_data(), colnames))
      )
      fcst_models <- unique(unlist(
        lapply(verif_data(), function(x) unique(x[[fcst_model_col]]))
      ))
      if (is.null(colour_table()) | nrow(colour_table()) < length(fcst_models)) {
        available_colours <- palette_colours()
      } else {
        available_colours <- colour_table()$colour
      }
    } else {
      available_colours <- RColorBrewer::brewer.pal(
        RColorBrewer:::qualnum[input$colour_palette],
        RColorBrewer:::quallist[input$colour_palette]
      )
    }
    palette_colours(available_colours)
  })

  shiny::observeEvent(palette_colours(), {
    shiny::req(input$colour_palette)
    if (input$colour_palette == "Custom") {
      palette      <- "square"
      allowed_cols <- NULL
    } else {
      palette      <- "limited"
      allowed_cols <- palette_colours()
    }

    fcst_model_col <- intersect(
      c("mname", "fcst_model"),
      Reduce(union, lapply(verif_data(), colnames))
    )
    fcst_models <- unique(unlist(
      lapply(verif_data(), function(x) unique(x[[fcst_model_col]]))
    ))
    lapply(
      seq_along(fcst_models),
      function(i) {
        colourpicker::updateColourInput(
          session,
          paste0("colour_", i),
          value = palette_colours()[i],
          palette = palette,
          allowedCols = allowed_cols
        )
      }
    )
  })

  # Initialise colour tables and spawn observers
  colour_table_modal <- shiny::reactiveVal(NULL)
  colour_table <- shiny::reactiveVal(NULL)
  shiny::observeEvent(verif_data(), {
    shiny::req(verif_data())
    fcst_model_col <- intersect(
      c("mname", "fcst_model"),
      Reduce(union, lapply(verif_data(), colnames))
    )
    fcst_models <- unique(unlist(
      lapply(verif_data(), function(x) unique(x[[fcst_model_col]]))
    ))
    colour_table_modal(
      dplyr::rename_with(
        data.frame(
          mname = fcst_models,
          colour = palette_colours()[1:length(fcst_models)],
          stringsAsFactors = FALSE
        ),
        ~gsub("mname", fcst_model_col, .x),
        "mname"
      )
    )
    colour_table(colour_table_modal())
    lapply(
      seq_along(fcst_models),
      function(i) shiny::observeEvent(input[[paste0("colour_", i)]], {
        modified_colours <- colour_table_modal()
        new_colour       <- input[[paste0("colour_", i)]]
        modified_colours$colour[modified_colours[[fcst_model_col]] == fcst_models[i]] <- new_colour
        colour_table_modal(modified_colours)
      })
    )
  })

  # function for color selector modal - this sets the initial colours and spawns the selectors
  colour_modal <- function() {

    fcst_model_col <- intersect(
      c("mname", "fcst_model"),
      Reduce(union, lapply(verif_data(), colnames))
    )
    fcst_models <- unique(unlist(
      lapply(verif_data(), function(x) unique(x[[fcst_model_col]]))
    ))
    if (selected_palette() == "Custom") {
      palette      <- "square"
      allowed_cols <- NULL
    } else {
      palette      <- "limited"
      allowed_cols <- palette_colours()
    }

    shiny::modalDialog(
      shiny::selectInput(
        ns("colour_palette"),
        "Colour Palette",
        c(RColorBrewer:::quallist, "harp2", "Custom"),
        selected_palette()
      ),
      lapply(seq_along(fcst_models), function(i)
        colourpicker::colourInput(
          ns(paste0("colour_", i)),
          fcst_models[i],
          value = colour_table()$colour[i],
          showColour = "background",
          palette = palette,
          allowedCols = allowed_cols
        )
      ),
      footer = shiny::tagList(
        shiny::actionButton(ns("colours_modal_cancel"), "Cancel"),
        customActionButton(ns("colours_modal_ok"), "OK", btn_class = "primary")
      ),
      size = "s"
    )

  }

  # Open modal on button click
  shiny::observeEvent(input$plot_options, {
    shiny::showModal(colour_modal())
  })

  # Update colour table on modal ok
  shiny::observeEvent(input$colours_modal_ok, {
    colour_table(colour_table_modal())
    selected_palette(input$colour_palette)
    shiny::removeModal()
  })

  # Reset colour table for modal on cancel
  shiny::observeEvent(input$colours_modal_cancel, {
    colour_table_modal(colour_table())
    shiny::removeModal()
  })

  return(colour_table)

}
