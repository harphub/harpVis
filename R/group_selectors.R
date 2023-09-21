#' Title
#'
#' @param id
#'
#' @return
#' @export
#'
#' @examples
group_selectorsUI <- function(id) {

  ns <- shiny::NS(id)

  shiny::div(class = "col-sm-10 col-lg-11",
    shiny::fluidRow(
      shiny::div(class = "col-lg-6",
        shiny::fluidRow(
          shiny::div(class = "col-sm-6",
            shiny::div(id = ns("group-dropdown-1"))
          ),
          shiny::div(class = "col-sm-6",
            shiny::div(id = ns("group-dropdown-2"))
          )
        )
      ),
      shiny::div(class = "col-lg-6",
        shiny::fluidRow(
          shiny::div(class = "col-sm-6",
            shiny::div(id = ns("group-dropdown-3"))
          ),
          shiny::div(class = "col-sm-6",
            shiny::div(id = ns("group-dropdown-4"))
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
#' @param verif_data
#'
#' @return
#' @export
#'
#' @examples
group_selectors <- function(input, output, session, verif_data) {

  ns <- session$ns

  std_columns <- union(
    get("meta_colnames", envir = harpVis_data),
    get("score_colnames", envir = harpVis_data)
  )

  # When there is new data get the grouping columns and remove all inserted UI

  grp_columns <- shiny::reactiveVal(NULL)
  new_data    <- shiny::reactiveVal(0)
  shiny::observeEvent(verif_data(), {
    shiny::req(verif_data())
    for (i in seq_along(grp_columns())) {
      if (i > 4) break
      shiny::removeUI(paste0("#", ns(paste0("dropdown_", grp_columns()[i]))))
    }
    verif_columns <- unique(unlist(lapply(verif_data(), names)))
    verif_columns <- gsub("_for_threshold_", "_", verif_columns)
    grp_columns(setdiff(verif_columns, std_columns))
    new_data(new_data() + 1)
  })

  # Reactive value for the filtered data

  out_data <- shiny::reactiveVal(NULL)
  shiny::observeEvent(new_data(), {
    if (length(grp_columns()) == 0) {
      out_data(verif_data())
    }
  })

  # function to spawn observers for new group columns

  generate_observers <- function() {
    filtered_data <- verif_data()
    lapply(
      seq_along(grp_columns()),
      function(i) {
        shiny::observeEvent(input[[paste0("group_", i)]], {
          verif_attr <- attributes(verif_data())
          # filter all the columns and update the selectInput for the
          # other columns based on the new filtered data
          for (j in seq_along(grp_columns())) {
            grp_col <- grp_columns()[j]
            grp_input <- input[[paste0("group_", j)]]
            filtered_data <- lapply(
              filtered_data, filter_group, grp_col, grp_input
            )
            other_cols <- seq_along(grp_columns())
            other_cols <- other_cols[other_cols != j]
            for (k in other_cols) {
              shiny::updateSelectInput(
                session,
                paste0("group_", k),
                choices = run_sort_choices(Reduce(
                  union,
                  lapply(
                    lapply(verif_data(), filter_group, grp_col, grp_input),
                    function(x) x[[grp_columns()[k]]])
                )),
                selected = input[[paste0("group_", k)]]
              )
            }
          }
          attributes(filtered_data) <- c(
            verif_attr, list(group_cols = grp_columns())
          )
          if (all(sapply(filtered_data, nrow) < 1)) {
            return()
          }
          out_data(filtered_data)
        })
      }
    )
  }

  # When there are new data, insert selectors for the group columns
  shiny::observeEvent(new_data(), {

    if (new_data() == 0) return()

    for (group_num in seq_along(grp_columns())) {

      div_num        <- (group_num - 1) %% 4 + 1
      select_label   <- grp_columns()[group_num]

      verif_df <- verif_data()$ens_summary_scores
      if (is.null(verif_df)) {
        verif_df <- verif_data()$det_summary_scores
      }

      select_choices <- run_sort_choices(verif_df[[grp_columns()[group_num]]])

      if (group_num > 4) {
        select_choices_before <- run_sort_choices(verif_df[[grp_columns()[group_num - 4]]])
        select_label_before   <- grp_columns()[group_num - 4]
      }

      ui_arg <- shiny::div(
        id = ns(paste0("dropdown_", select_label)),
        shiny::selectInput(
          ns(paste0("group_", group_num)),
          select_label,
          select_choices,
          select_choices[1],
          width = "100%"
        )
      )

      if (group_num > 4) {
        shiny::removeUI(paste0("#", ns(paste0("dropdown_", select_label_before))))
        ui_arg <- shiny::fluidRow(
          shiny::div(class = "col-sm-6",
            shiny::div(id = ns(paste0("dropdown_", select_label_before)),
              shiny::selectInput(
                ns(paste0("group_", group_num - 4)),
                select_label_before,
                select_choices_before,
                select_choices_before[1],
                width = "100%"
              )
            )
          ),
          shiny::div(class = "col-sm-6",
            shiny::div(id = ns(paste0("dropdown_", select_label)),
              shiny::selectInput(
                ns(paste0("group_", group_num)),
                select_label,
                select_choices,
                select_choices[1],
                width = "100%"
              )
            )
          )
        )
      }

      shiny::insertUI(
        selector = paste0("#", ns(paste0("group-dropdown-", div_num))),
        where    = "beforeEnd",
        ui       = ui_arg
      )

    }

    if (length(grp_columns()) > 0) generate_observers()
  })

  return(shiny::debounce(out_data, 100))

}

# Some helper functions

sort_choices <- function(regexp, choices) {
  choices <- sort(unique(choices))
  if (length(grep(regexp, choices)) > 0) {
    c(
      grep(regexp, choices, value = TRUE),
      grep(regexp, choices, value = TRUE, invert = TRUE)
    )
  } else {
    NULL
  }
}

run_sort_choices <- function(choices_to_sort) {
  select_choices <- Reduce(
    union,
    lapply(
      c(";", "^All$"),
      sort_choices,
      choices_to_sort
    )
  )
  if (is.null(select_choices)) {
    select_choices <- sort(unique(choices_to_sort))
  }
  select_choices
}

filter_group <- function(data, group_col, group_input) {
  if (is.null(group_input)) {
    return(data)
  }
  if (!is.element(group_col, colnames(data))) {
    if (group_input != "All") {
      return(data[0, ])
    }
    return(data)
  }
  dplyr::filter(data, .data[[group_col]] == group_input)
}
