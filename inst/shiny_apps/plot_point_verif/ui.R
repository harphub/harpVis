# UI for shiny_plot_point_verif


ui <- tags$html(
  tags$head(
    tags$link(href="https://fonts.googleapis.com/css?family=Comfortaa:400,700",  rel="stylesheet"),
    tags$script('
      var dimension = [0, 0];
      $(document).on("shiny:connected", function(e) {
        dimension[0] = window.innerWidth;
        dimension[1] = window.innerHeight;
        Shiny.onInputChange("dimension", dimension);
      });
      $(window).resize(function(e) {
        dimension[0] = window.innerWidth;
        dimension[1] = window.innerHeight;
        Shiny.onInputChange("dimension", dimension);
      });
    ')
  ),
  tags$body(
    tags$div(
      class = "harp_page_header",
      span(class = "harp_page_title", "harp : : Point Verification"),
      div(class = "harp_logo",
        img(src = "harp_logo_dark.svg", height = "70px")
      )
    ),

    shiny::fluidPage(
      title = "harp",

      shiny::includeCSS("harp_midnight.css"),

      harpVis::options_barUI("options_bar"),

      shiny::tabsetPanel(id = "tab_panel",

        harpVis::dashboard_epsUI("dashboard"),

        harpVis::interactive_epsUI("interactive")

      ) # end of tabsetPanel

    ) # end fluid page

  ) # end of tags$body

) # end of tags$html

