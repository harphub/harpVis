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
        img(src = "harp_logo.svg", height = "70px")
      )
    ),

    shiny::fluidPage(
      title = "harp",

      shiny::includeCSS("harp_midnight.css"),
      # shiny::includeCSS("harp_midnight.css"),


      fluidRow(id = "options_bar",
        column(3,
          shiny::textInput(
            "data_dir",
            label = NULL,
            value = "/lustre/storeB/users/andrewts/HarpResults/verification",
            placeholder = "Data directory",
            width = "500px"
          )
        ),
        column(2,
          shiny::selectInput("parameter", "Parameter", "Waiting for valid directory", width = "200px")
        ),
        column(3,
          shiny::selectInput("dates", "Dates", "Waiting for valid directory", width = "250px")
        ),
        column(3,
          shiny::selectInput("models", "Model combination", "Waiting for valid directory", width = "500px")
        ),
        #classButton("load_data", "Load", icon = icon("upload"), class = "btn btn-primary action-button btn-block")
        column(1,
          shiny::actionButton("load_data", "Load", icon = icon("upload"))
        )
      ),
      shiny::tabsetPanel(id = "tab_panel",
        shiny::tabPanel("Dashboard",
          fluidRow(
            column(7,
              fluidRow(
                column(6,
                  tags$div(class = "dashboard-panel",
                    shiny::plotOutput("dashboard_spread_skill", height = "100%", width = "100%")
                  )
                ),
                column(6,
                  tags$div(class = "dashboard-panel",
                    shiny::plotOutput("dashboard_crps", height = "100%", width = "100%")
                  )
                )
              )
            ),
            column(5,
              tags$div(class = "dashboard-panel",
                shiny::plotOutput("dashboard_rank_hist", height = "100%", width = "100%")
              )
            )
          ),
          fluidRow(
            column(3,
              tags$div(class = "dashboard-panel",
                shiny::plotOutput("dashboard_reliability", height = "100%", width = "100%")
              )
            ),
            column(3,
              tags$div(class = "dashboard-panel",
                shiny::plotOutput("dashboard_roc", height = "100%", width = "100%")
              )
            ),
            column(6,
              tags$div(class = "dashboard-panel",
                shiny::plotOutput("dashboard_brier", height = "100%", width = "100%")
              )
            )
          )
        ),
        shiny::tabPanel("Interactive",
          fluidRow(
            column(2,
              shiny::selectInput("score", "Score", "Waiting for valid data"),
              shiny::tags$div(id = "scoreOptionsPlaceholder")
            ),
            column(8,
              shiny::plotOutput("verif_plot")
            )
          )

        ) # end of tabPanel

      ) # end of tabsetPanel

    ) # end fluid page

  ) # end of tags$body

) # end of tags$html

