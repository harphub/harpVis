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
  is_profile     <- shiny::reactiveVal(NULL)
  out_data       <- shiny::reactiveVal(NULL)

  possible_time_axes <- c(
    "lead_time", "leadtime",
    "valid_dttm", "validdate",
    "valid_hour"
  )

  shiny::observeEvent(verif_data(), {
    shiny::req(verif_data())

    out_data(verif_data())
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

    is_profile_data <- FALSE
    group_cols <- attr(verif_data(), "group_cols")
    if (!is.null(group_cols) && is.element("p", group_cols)) {
      all_levels <- Reduce(union, lapply(verif_data(), function(x) x[["p"]]))
      if (length(all_levels) > 1) {
        is_profile_data <- TRUE
      }
    }
    is_profile(is_profile_data)

    shiny::removeUI(paste0("#", ns("time-axis-div")))

    if (length(time_col_names) == 1) {
      time_axis_name(time_col_names)

      if (is_profile()) {
        selected_time <- input[["profile_time_select"]]
        data_times    <- Reduce(
          union, lapply(verif_data(), function(x) x[[time_col_names]])
        )
        data_times <- parse_times(data_times, time_col_names)
        if (is.null(selected_time) || !is.element(selected_time, data_times)) {
          selected_time <- data_times[1]
        }
        shiny::insertUI(
          selector = paste0("#", ns("time-axis-div-sel")),
          where = "beforeEnd",
          shiny::div(
            id = ns("time-axis-div"),
            shiny::div(
              class = "col-lg-4",
              shiny::selectInput(
                ns("profile_time_select"),
                names(time_col_names),
                data_times,
                selected_time
              )
            )
          )
        )
      }

      return()
    }

    time_col <- input[["time_axis"]]
    if (is.null(time_col) || !is.element(time_col, time_col_names)) {
      time_col <- time_col_names[1]
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
            time_col
          )
        )
      )
    )

    if (is_profile()) {
      selected_time <- input[["profile_time_select"]]
      data_times    <- Reduce(
        union, lapply(verif_data(), function(x) x[[time_col]])
      )
      data_times <- parse_times(data_times, time_col)
      if (is.null(selected_time) || !is.element(selected_time, data_times)) {
        selected_time <- data_times[1]
      }
      shiny::insertUI(
        selector = paste0("#", ns("time-axis-div")),
        where = "beforeEnd",
        shiny::div(
          class = "col-lg-4",
          shiny::selectInput(
            ns("profile_time_select"),
            names(time_col_names)[time_col_names == time_col],
            data_times,
            selected_time
          )
        )
      )
    }

  })

  shiny::observeEvent(input[["time_axis"]], {
    time_axis_name(input[["time_axis"]])
  })

  shiny::observeEvent(time_axis_name(), {

    shiny::req(verif_data())
    if (is_profile()) {
      selected_time <- input[["profile_time_select"]]
      data_times    <- Reduce(
        union, lapply(verif_data(), function(x) x[[time_axis_name()]])
      )
      data_times <- parse_times(data_times, time_axis_name())
      if (is.null(selected_time) || !is.element(selected_time, data_times)) {
        selected_time <- data_times[1]
      }
      shiny::updateSelectInput(
        session,
        "profile_time_select",
        gsub(
          "Dttm",
          "Date-Time",
          totitle(gsub("_", " ", time_axis_name()))
        ),
        data_times,
        selected_time
      )
    }
  })

  shiny::observeEvent(list(input[["profile_time_select"]], is_profile()), {

    shiny::req(is_profile())
    if (!is_profile()) return()
    shiny::req(input[["profile_time_select"]])
    attrs <- attributes(verif_data())
    filtered_data <- lapply(
      verif_data(),
      dplyr::filter,
      !!rlang::sym(time_axis_name()) == input[["profile_time_select"]]
    )
    attributes(filtered_data) <- c(attrs, list(is_profile = TRUE))
    out_data(filtered_data)
  })

  return(list(
    time_axis = time_axis_name,
    filtered_data = shiny::debounce(out_data, 200)
  ))
}

parse_times <- function(times, time_var) {
  times <- times[times != "All"]
  if (grepl("dttm|validdate|fcdate", time_var)) {
    times_dttm <- do.call(c, lapply(times, as.POSIXct, tz = "UTC"))
    times <- times[order(match(times, times_dttm))]
    names(times) <- format(times_dttm, "%H:%M %d %b %Y")
  } else {
    times <- sort(as.numeric(times))
  }
  times
}
