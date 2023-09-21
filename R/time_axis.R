#' Shiny UI for selecting time axis
#'
#' @export
time_axisUI <- function(id) {

  ns <- shiny::NS(id)
  shiny::div(class = "col-sm-10 col-lg-11",
    shiny::div(class = "row", id = ns("time-axis-div-sel"))
  )

}

#' Shiny server for selecting time axis
#'
#' @export
time_axis <- function(input, output, session, verif_data) {

  ns <- session[["ns"]]

  time_axis_name <- shiny::reactiveVal(NULL)

  possible_time_axes <- c(
    "lead_time", "leadtime",
    "valid_dttm", "valid_dttm",
    "valid_hour"
  )

  shiny::observeEvent(verif_data(), {
    shiny::req(verif_data())
    verif_col_names <- Reduce(union, lapply(verif_data(), colnames))
    time_col_names  <- intersect(
      possible_time_axes,
      verif_col_names
    )

    names(time_col_names) <- gsub(
      "Dttm",
      "Date-Time",
      totitle(gsub("_", " ", time_col_names))
    )

    shiny::removeUI(paste0("#", ns("time-axis-div")))

    if (length(time_col_names) == 1) {
      time_axis_name(time_col_names)
      return()
    }

    shiny::insertUI(
      selector = paste0("#", ns("time-axis-div-sel")),
      where = "beforeEnd",
      shiny::div(
        id = ns("time-axis-div"),
        shiny::div(
          class = "col-lg-4",
          shiny::selectInput(
            ns("time_axis"),
            "Time axis",
            time_col_names,
            input[["time_axis"]]
          )
        )
      )
    )

  })

  shiny::observeEvent(input[["time_axis"]], {
    time_axis_name(input[["time_axis"]])
  })

  return(time_axis_name)
}
