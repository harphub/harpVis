# UI for shiny_plot_point_verif

font_link <- shiny::tags$link("")
is_online <- shiny::getShinyOption("online", default = TRUE)

hostname  <- Sys.getenv("HOSTNAME")
if (is_online & (!is.null(hostname) && !grepl("^ecgb", hostname))) {
  font_link <- shiny::tags$link(
    href="https://fonts.googleapis.com/css?family=Comfortaa:400,700",  rel="stylesheet"
  )
}

app_theme <- shiny::getShinyOption("theme", default = "white")
css_file <- switch(
  app_theme,
  "dark"  = "harp_midnight.css",
  "light" = "harp_light.css",
  "white" = "harp_white.css"
)


ui <- shiny::tags$html(
  shiny::tags$head(
   font_link#,
    # shiny::tags$script('
    #   var dimension = [0, 0];
    #   $(document).on("shiny:connected", function(e) {
    #     dimension[0] = window.innerWidth;
    #     dimension[1] = window.innerHeight;
    #     Shiny.onInputChange("dimension", dimension);
    #   });
    #   $(window).resize(function(e) {
    #     dimension[0] = window.innerWidth;
    #     dimension[1] = window.innerHeight;
    #     Shiny.onInputChange("dimension", dimension);
    #   });
    # ')
  ),
  shiny::tags$body(
    shiny::tags$div(
      class = "harp_page_header",
      shiny::span(class = "harp_page_title", "harp : : Point Verification"),
      shiny::div(class = "harp_logo",
        shiny::img(src = "harp_logo_dark.svg", height = "70px")
      )
    ),

    shiny::fluidPage(
      title = "harp",

      shiny::includeCSS(css_file),

      harpVis::options_barUI("options_bar"),

      shiny::fluidRow(
        harpVis::group_selectorsUI("group_selectors")
      ),

      shiny::fluidRow(
        harpVis::time_axisUI("time_axis"),
        harpVis::colour_choicesUI("colour_choices")
      ),

      shiny::tabsetPanel(id = "tab_panel",

        shiny::tabPanel("Dashboard",
          harpVis::dashboard_point_verifUI("dashboard")
        ),

        shiny::tabPanel("Interactive",
          harpVis::interactive_point_verifUI("interactive"),
          harpVis::download_verif_plotUI("download_plot")
        )

      ) # end of tabsetPanel

    ) # end fluid page

  ) # end of tags$body

) # end of tags$html

