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

  app_start_dir <- shiny::getShinyOption("app_start_dir")
  volumes <- c(Home = fs::path_home(), harp_getVolumes()())
  if (!is.null(app_start_dir)) {
    volumes <- unclass(fs::path(app_start_dir))
    names(volumes)[1] <- app_start_dir
  }
  shinyFiles::shinyDirChoose(
    input, "data_dir", roots = volumes, session = session, restrictions = system.file(package = "base")
  )

  data_dir <- reactiveVal()

  shiny::observeEvent(input$data_dir, {
    data_dir(shinyFiles::parseDirPath(volumes, input$data_dir))
  })

  data_files <- shiny::reactiveValues(
    filenames  = NULL,
    parameters = "Waiting for valid directory",
    dates      = "Waiting for valid directory",
    models     = "Waiting for valid directory"
  )

  shiny::observeEvent(list(data_dir()), {
    shiny::req(data_dir())
    if(length(data_dir()) < 1) return()
    data_files$filenames  <- dir(data_dir(), pattern = "harpPointVerif*[[:graph:]]*.rds")
    harp_files            <- strsplit(data_files$filenames, ".harp.")
    data_files$models     <- gsub(".model.", " + ", unique(unlist(lapply(harp_files, `[`, 4))))
    data_files$models     <- gsub(".rds", "", data_files$models)

    selected_models <- NULL
    if (!is.null(input$models) && input$models %in% data_files$models) {
      selected_models <- input$models
    }
    shiny::updateSelectInput(
      session,
      "models",
      choices  = data_files$models,
      selected = selected_models
    )
  })

  shiny::observeEvent(input$models, {
    shiny::req(input$models)
    models       <- gsub(" \\+ ", ".model.", input$models)
    regexp       <- paste0(".harp.", models, ".rds")
    harp_files   <- strsplit(grep(regexp, data_files$filenames, value = TRUE), ".harp.")
    files_dates  <- unique(unlist(lapply(harp_files, `[`, 3)))


    if (!is.null(files_dates)) {
      files_dates <- data.frame(dates = files_dates) %>%
        tidyr::separate(.data$dates, c("startdate", "enddate"), "-") %>%
        dplyr::arrange(.data$startdate, .data$enddate) %>%
        dplyr::mutate(
          dates = paste(.data$startdate, .data$enddate, sep = "-")
        ) %>%
        dplyr::pull(.data$dates)

      names(files_dates) <- menu_dates_to_char(files_dates)
    }

    data_files$dates  <- files_dates

    selected_dates <- NULL
    if (!is.null(input$dates) && input$dates %in% data_files$dates) {
      selected_dates <- input$dates
    }

    shiny::updateSelectInput(
      session,
      "dates",
      choices  = data_files$dates,
      selected = selected_dates
    )
  })

  shiny::observeEvent(list(input$models, input$dates), {
    shiny::req(input$models, input$dates)
    models                <- gsub(" \\+ ", ".model.", input$models)
    dates                 <- input$dates
    if (grepl("Waiting", dates)) {
      dates <- ""
    }
    regexp                <- paste0(paste(dates, models, sep = ".harp."), ".rds")
    harp_files            <- strsplit(grep(regexp, data_files$filenames, value = TRUE), ".harp.")
    data_files$parameters <- unique(unlist(lapply(harp_files, `[`, 2)))

    selected_parameter <- NULL
    if (!is.null(input$parameter) && input$parameter %in% data_files$parameters) {
      selected_parameter <- input$parameter
    }
    shiny::updateSelectInput(
      session,
      "parameter",
      choices  = data_files$parameters,
      selected = selected_parameter
    )
  })

  verif_file <- shiny::reactiveVal()

  shiny::observeEvent(list(input$parameter, input$dates, input$models), {
    shiny::req(input$models)
    models <- gsub(" \\+ ", ".model.", input$models)
    regexp <- paste0(
      paste(input$parameter, input$dates, models, sep = "\\.harp\\."),
      ".rds$"
    )
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
  if (length(menu_dates) == 1 && grepl("Waiting", menu_dates)) return("")
  split_dates <- strsplit(menu_dates, "-")
  dates_start <- purrr::map(split_dates, ~date_to_char(.x[1]))
  dates_end   <- purrr::map(split_dates, ~date_to_char(.x[2]))

  purrr::map2_chr(dates_start, dates_end, paste, sep = " - ")
}
