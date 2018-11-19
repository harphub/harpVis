# UI for shiny_plot_point_verif


ui <- tags$html(
  tags$head(
    tags$link(href="https://fonts.googleapis.com/css?family=Comfortaa:400,700",  rel="stylesheet")
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


  #    shiny::tabsetPanel(
  #      shiny::tabPanel("Interactive",
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
          fluidRow(
            column(2,
              shiny::selectInput("score", "Score", "Waiting for valid data"),
              shiny::tags$div(id = "scoreOptionsPlaceholder")
            ),
            column(8,
              shiny::plotOutput("verif_plot")
            )
          )

#       ) # end of tabPanel

#      ) # end of tabsetPanel

    ) # end fluid page

  ) # end of tags$body

) # end of tags$html

