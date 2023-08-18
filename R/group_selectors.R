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

  # The standard (std) column names come from the built in data

  std_ens_tables <- grep("ens_", names(harpVis::ens_verif_data))
  std_det_tables <- grep("det_", names(harpVis::det_verif_data))
  std_columns    <- unique(c(
    unlist(lapply(harpVis::ens_verif_data[std_ens_tables], names)),
    unlist(lapply(harpVis::det_verif_data[std_det_tables], names))
  ))
  std_columns <- union(
    std_columns,
    c(
      "lead_time",
      "sub_model",
      "member",
      "spread_skill_ratio",
      "dropped_members_spread_skill_ratio",
      "dropped_members_spread",
      "parameter",
      "dates",
      "num_stations",
      "fair_crps",
      "fair_brier_score",
      "lon",
      "lat",
      "elev"
    )
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
    res <- lapply(
      seq_along(grp_columns()),
      function(x) {
        shiny::observeEvent(input[[paste0("group_", x)]], {
          verif_attr <- attributes(verif_data())
          filtered_data <- verif_data()
          for (i in seq_along(grp_columns())) {
            filter_col <- rlang::sym(grp_columns()[i])
            filtered_data <- purrr::map_at(
              filtered_data,
              which(sapply(filtered_data, nrow) > 0),
              dplyr::filter,
              !! filter_col == input[[paste0("group_", i)]]
            )
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
    res
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

  return(out_data)

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

