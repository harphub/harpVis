# Module for options bar in shiny app for plotting point verification
# The user provides the data directory and the files are scanned -
# the user can then select what model comparisons to do, for what dates, a
# and for the parameter.

#' Title
#'
#' @param id
#'
#' @return
#' @export
#'
#' @examples
options_barUI <- function(id) {

  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(id = "options_bar",
      shiny::div(class = "col-lg-6",
        shiny::fluidRow(
          shiny::div(class = "col-sm-6",
            shinyFiles::shinyDirButton(
              ns("data_dir"),
              "Select Verification Directory",
              "Please select a directory",
              buttonType = "harp",
              style = "width: 100%;",
              icon = shiny::icon("folder-open")
            )
          ),
          shiny::div(class = "col-sm-6",
            shiny::selectInput(ns("models"), "Model combination", "Waiting for valid directory", width = "100%")
          )
        )
      ),
      shiny::div(class = "col-lg-6",
        shiny::fluidRow(
          shiny::div(class = "col-sm-6",
            shiny::selectInput(ns("dates"), "Dates", "Waiting for valid directory", width = "100%")
          ),
          shiny::div(class = "col-sm-4",
            shiny::selectInput(ns("parameter"), "Parameter", "Waiting for valid directory", width = "100%")
          ),
          #classButton("load_data", "Load", icon = icon("upload"), class = "btn btn-primary action-button btn-block")
          shiny::div(class = "col-sm-2",
            shiny::actionButton(ns("load_data"), "Load", icon = icon("upload"), width = "100%")
          )
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
options_bar <- function(input, output, session) {

  volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), harp_getVolumes()())
  shinyFiles::shinyDirChoose(
    input, "data_dir", roots = volumes, session = session, restrictions = system.file(package = "base")
  )

  data_dir <- reactiveVal()

  shiny::observeEvent(input$data_dir, {
    data_dir(shinyFiles::parseDirPath(volumes, input$data_dir))
  })

  data_files <- shiny::reactiveValues(
    filenames  = NULL,
    parameters = NULL,
    dates      = NULL,
    models     = NULL
  )

  shiny::observeEvent(data_dir(), {
    shiny::req(data_dir())
    if(length(data_dir()) < 1) return()
    data_files$filenames  <- dir(data_dir(), pattern = "harpPointVerif*[[:graph:]]*.rds")
    harp_files            <- strsplit(data_files$filenames, ".harp.")
    data_files$models     <- gsub(".model.", " + ", unique(unlist(lapply(harp_files, `[`, 4))))
    data_files$models     <- gsub(".rds", "", data_files$models)

    shiny::updateSelectInput(session, "models", choices = "Waiting for valid directory")
    shiny::updateSelectInput(session, "models", choices = data_files$models)
  })

  shiny::observeEvent(input$models, {
    shiny::req(input$models)
    models                <- gsub(" \\+ ", ".model.", input$models)
    harp_files            <- strsplit(grep(models, data_files$filenames, value = TRUE), ".harp.")
    data_files$parameters <- unique(unlist(lapply(harp_files, `[`, 2)))

    shiny::updateSelectInput(session, "parameter", choices = "Waiting for valid directory")
    shiny::updateSelectInput(session, "parameter", choices = data_files$parameters)
  })

  shiny::observeEvent(list(input$models, input$parameter), {
    shiny::req(input$parameter)
    models       <- gsub(" \\+ ", ".model.", input$models)
    regexp       <- paste(input$parameter, models, sep = "*[[:graph:]]*")
    harp_files   <- strsplit(grep(regexp, data_files$filenames, value = TRUE), ".harp.")
    files_dates  <- unique(unlist(lapply(harp_files, `[`, 3)))

    if (!is.null(files_dates)) {
      names(files_dates) <- menu_dates_to_char(files_dates)
    }

    data_files$dates  <- files_dates

    shiny::updateSelectInput(session, "dates", choices = "Waiting for valid directory")
    shiny::updateSelectInput(session, "dates", choices = data_files$dates)
  })

  verif_file <- shiny::reactiveVal()

  shiny::observeEvent(list(input$parameter, input$dates, input$models), {
    shiny::req(input$models)
    models <- gsub(" \\+ ", ".model.", input$models)
    regexp <- paste(input$parameter, input$dates, models, sep = "*[[:graph:]]*")
    verif_file(file.path(data_dir(), grep(regexp, data_files$filenames, value = TRUE)))
  })

  verif_data <- shiny::reactiveVal()

  shiny::observeEvent(input$load_data, {
    valid_list_elements <- unlist(lapply(c("ens", "det"), paste, c("summary_scores", "threshold_scores"), sep = "_"))
    modal_footer <- shiny::tags$button(
      type = "button", class = "btn btn-danger", `data-dismiss` = "modal", shiny:::validateIcon(NULL), "Dismiss"
    )
    if(is.null(verif_file()) || length(verif_file()) < 1 || !file.exists(verif_file())) {
      shiny::showModal(
        shiny::modalDialog(title = "ERROR", "Cannot find file", size = "s", footer = modal_footer)
      )
    } else {
      verif_data(try(readRDS(verif_file()), silent = TRUE))
      if (inherits(verif_data(), "try-error")) {
        shiny::showModal(
          shiny::modalDialog(title = "ERROR", "Cannot read file", size = "s", footer = modal_footer)
        )
        verif_data(NULL)
      } else if (length(intersect(valid_list_elements, names(verif_data()))) < 1) {
        shiny::showModal(
          shiny::modalDialog(
            title = "ERROR", "File does not contain harp point verification scores", size = "s", footer = modal_footer
          )
        )
        verif_data(NULL)
      }
    }
  })

  return(verif_data)

}

menu_dates_to_char <- function(menu_dates) {
  split_dates <- strsplit(menu_dates, "-")
  dates_start <- purrr::map(split_dates, ~date_to_char(.x[1]))
  dates_end   <- purrr::map(split_dates, ~date_to_char(.x[2]))

  purrr::map2_chr(dates_start, dates_end, paste, sep = " - ")
}
