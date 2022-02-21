#' Title
#'
#' @param id
#'
#' @return
#' @export
#'
#' @examples
download_verif_plotUI <- function(id) {

  ns <- shiny::NS(id)

  shiny::fluidRow(
     shiny::div(class = "col-sm-2 col-sm-offset-10", id = ns("save_plot_div"),
      customActionButton(
        ns("save_plot"),
        "Save",
        icon      = shiny::icon("save", class = "fas"),
        btn_class = "info")
    )
  )

}

#' Title
#'
#' @param input
#' @param output
#' @param session
#' @param verif_data
#' @param score_options
#' @param colour_table
#'
#' @return
#' @export
#'
#' @examples
download_verif_plot <- function(input, output, session, verif_data, score_options, colour_table) {

  ns <- session$ns

  plot_options <- shiny::reactiveValues(
    bg = "bw"
  )
  save_options <- shiny::reactiveValues(
    format = "png",
    width  = 25,
    height = 17,
    dpi    = 300
  )

  shiny::observeEvent(list(verif_data(), score_options()$score, input$close_save_modal), {
    plot_score <- gsub("[[:alpha:]]+_[[:alpha:]]+_scores_", "", score_options()$score)
    plot_options$title <- paste(
      totitle(gsub("_", " ", plot_score)),
      ":",
      paste0(
        date_to_char(attr(verif_data(), "start_date")),
        " - ",
        date_to_char(attr(verif_data(), "end_date"))
      )
    )
    plot_options$subtitle <- paste(attr(verif_data(), "num_stations"), "stations")
    plot_options$caption  <- paste("Verification for", attr(verif_data(), "parameter"))
    plot_models <- unique(unlist(purrr::map(verif_data(), ~ unique(.x$mname))))
    save_options$filename <- paste0(
      paste(
        score_options()$score,
        attr(verif_data(), "parameter"),
        paste(plot_models, collapse = "+"),
        paste(attr(verif_data(), "start_date"), attr(verif_data(), "end_date"), sep = "-"),
        sep = "_"
      ),
      ".",
      save_options$format
    )
  })

  shiny::observeEvent(input$save_plot, {
    shiny::showModal(
      shiny::modalDialog(
        title = "Save Options",
        shiny::fluidRow(
          shiny::radioButtons(
            ns("bg_colour"),
            "Background",
            choices = c(white = "bw", grey = "grey", dark = "harp_midnight"),
            inline = TRUE
          ),
          shiny::textInput(
            ns("plot_title"),
            "Title",
            value = plot_options$title,
            width = "100%"
          ),
          shiny::textInput(
            ns("plot_subtitle"),
            "Subtitle",
            value = plot_options$subtitle,
            width = "100%"
          ),
          shiny::textInput(
            ns("plot_caption"),
            "Caption",
            value = plot_options$caption,
            width = "100%"
          )
        ),
        shiny::fluidRow(
          shiny::plotOutput(ns("verif_plot"), height = "333px")
        ),
        shiny::fluidRow(
          shiny::br(),
          shiny::radioButtons(
            ns("plot_format"),
            "Format",
            choices = c("png", "pdf", "eps", "svg"),
            inline  = TRUE
          )
        ),
        shiny::fluidRow(
          shiny::column(4,
            shiny::numericInput(
              ns("plot_width"),
              "Width [cm]",
              value = save_options$width,
              min   = 5,
              max   = 30
            )
          ),
          shiny::column(4,
            shiny::numericInput(
              ns("plot_height"),
              "Height [cm]",
              value = save_options$height,
              min   = 5,
              max   = 30
            )
          ),
          shiny::column(4,
            shiny::numericInput(
              ns("plot_dpi"),
              "DPI",
              value = save_options$dpi,
              min   = 70,
              max   = 350
            )
          )
        ),
        shiny::fluidRow(
          shiny::textInput(
            ns("plot_filename"),
            "File name",
            value = save_options$filename,
            width = "100%"
          )
        ),
        footer = shiny::tagList(
          shiny::actionButton(ns("close_save_modal"), "Dismiss"),
          customDownloadButton(
            ns("save_customised_plot"), "Save", icon = "save", icon_class = "fas", bs_btn = "primary"
          )
        )
      )
    )
  })

  shiny::observeEvent(input$close_save_modal, {
    shiny::removeModal()
  })

  shiny::observeEvent(input$bg_colour, {
    plot_options$bg <- shiny::req(input$bg_colour)
  })

  shiny::observeEvent(input$plot_title, {
    plot_options$title <- shiny::req(input$plot_title)
  })

  shiny::observeEvent(input$plot_subtitle, {
    plot_options$subtitle <- shiny::req(input$plot_subtitle)
  })

  shiny::observeEvent(input$plot_caption, {
    plot_options$caption <- shiny::req(input$plot_caption)
  })

  shiny::observeEvent(input$plot_format, {
    save_options$format <- input$plot_format
    file_name <- save_options$filename
    file_name <- as.character(
      fs::path_ext_set(fs::path_ext_remove(file_name), save_options$format)
    )
    shiny::updateTextInput(session, "plot_filename", value = file_name)
  })

  shiny::observeEvent(input$plot_width, {
    save_options$width <- input$plot_width
  })

  shiny::observeEvent(input$plot_height, {
    save_options$height <- input$plot_height
  })

  shiny::observeEvent(input$plot_dpi, {
    save_options$dpi <- input$plot_dpi
  })

  shiny::observeEvent(input$plot_filename, {
    save_options$filename <- input$plot_filename
  })

  output$verif_plot <- shiny::renderPlot({

    score_type <- strsplit(score_options()$score, "_")[[1]][1]
    plot_score <- gsub("[[:alpha:]]+_[[:alpha:]]+_scores_", "", score_options()$score)

    plot_score  <- rlang::sym(plot_score)
    plot_x_axis <- rlang::sym(score_options()$x_axis)

    if (score_options()$line_cols == "mname") {

      line_cols  <- rlang::sym(score_options()$line_cols)
      score_plot <- harpVis::plot_point_verif(
        verif_data(),
        !!plot_score,
        verif_type       = score_type,
        x_axis           = !!plot_x_axis,
        colour_by        = !!line_cols,
        plot_num_cases   = score_options()$num_cases,
        extend_y_to_zero = score_options()$to_y_zero,
        facet_by         = score_options()$facets,
        filter_by        = score_options()$filters,
        colour_theme     = plot_options$bg,
        colour_table     = colour_table(),
        plot_title       = plot_options$title,
        plot_subtitle    = plot_options$subtitle,
        plot_caption     = plot_options$caption
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
        colour_theme     = plot_options$bg,
        colour_table     = member_cols,
        plot_title       = plot_options$title,
        plot_subtitle    = plot_options$subtitle,
        plot_caption     = plot_options$caption,
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

  })

  output$save_customised_plot <- shiny::downloadHandler(

    filename = function() {
      file_name <- save_options$filename
      corrected_extension <- fs::path_ext_set(
        fs::path_ext_remove(file_name),
        save_options$format
      )
      as.character(corrected_extension)
    },

    content = function(file) {

      score_type <- strsplit(score_options()$score, "_")[[1]][1]
      plot_score <- gsub("^[[:alpha:]]+_[[:alpha:]]+_scores_", "", score_options()$score)

      plot_score  <- rlang::sym(plot_score)
      plot_x_axis <- rlang::sym(score_options()$x_axis)

      if (score_options()$line_cols == "mname") {

        line_cols  <- rlang::sym(score_options()$line_cols)
        score_plot <- harpVis::plot_point_verif(
          verif_data(),
          !!plot_score,
          verif_type       = score_type,
          x_axis           = !!plot_x_axis,
          colour_by        = !!line_cols,
          plot_num_cases   = score_options()$num_cases,
          extend_y_to_zero = score_options()$to_y_zero,
          facet_by         = score_options()$facets,
          filter_by        = score_options()$filters,
          colour_theme     = plot_options$bg,
          colour_table     = colour_table(),
          plot_title       = plot_options$title,
          plot_subtitle    = plot_options$subtitle,
          plot_caption     = plot_options$caption
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
          colour_theme     = plot_options$bg,
          colour_table     = member_cols,
          plot_title       = plot_options$title,
          plot_subtitle    = plot_options$subtitle,
          plot_caption     = plot_options$caption,
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

      ggplot2::ggsave(
        file,
        score_plot,
        width  = save_options$width,
        height = save_options$height,
        dpi    = save_options$dpi,
        units  = "cm",
        device = save_options$format
      )
    }

  )

}

customDownloadButton <- function(
  outputId,
  label      = "Download",
  class      = NULL,
  bs_btn     = "default",
  icon       = "download",
  icon_class = NULL,
  ...
) {
  aTag <- shiny::tags$a(
    id       = outputId,
    class    = paste("btn", paste0("btn-", bs_btn), "shiny-download-link", class),
    href     = "",
    target   = "_blank",
    download = NA,
    shiny::icon(icon, class = icon_class),
    label,
    ...
  )
}

customActionButton <- function(
  inputId,
  label,
  icon      = NULL,
  width     = NULL,
  btn_class = "default",
  ...
) {
  value <- shiny::restoreInput(id = inputId, default = NULL)
  shiny::tags$button(
    id         = inputId,
    style      = if (!is.null(width)) paste0("width: ", shiny::validateCssUnit(width), ";"),
    type       = "button",
    class      = paste0("btn btn-", btn_class, " action-button"),
    `data-val` = value,
    list(shiny:::validateIcon(icon), label),
    ...
  )
}
