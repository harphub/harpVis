# UI for shiny_plot_spatial_verif

font_link <- shiny::tags$link("")
is_online <- shiny::getShinyOption("online")
if (is.null(is_online)) {
  is_online <- TRUE
}
hostname  <- Sys.getenv("HOSTNAME")
if (is_online & (!is.null(hostname) && !grepl("^ecgb", hostname))) {
  font_link <- shiny::tags$link(
    href="https://fonts.googleapis.com/css?family=Comfortaa:400,700",  rel="stylesheet"
  )
}
css_file <- switch(
  shiny::getShinyOption("theme"),
  "light" = "harp_light.css",
  "dark"  = "harp_midnight.css",
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
      shiny::span(class = "harp_page_title", "harp : : Spatial Verification"),
      shiny::div(class = "harp_logo",
                 shiny::img(src = "harp_logo_dark.svg", height = "70px")
      )
    ),

  shiny::fluidPage(

      # App title ----
      title = "harp",

      shiny::includeCSS(css_file),

      #harpVis::options_barUI("options_bar"),

      #shiny::fluidRow(
      #  harpVis::group_selectorsUI("group_selectors")
      #),

      #shiny::fluidRow(
      #  harpVis::time_axisUI("time_axis"),
      #  harpVis::colour_choicesUI("colour_choices")
      #),

      # Sidebar layout with input and output definitions ----
      sidebarLayout(

        # Sidebar panel for inputs ----
        sidebarPanel(width = 3,

			# Input: Select a file ----
			fileInput("filein", "Choose file (sqlite)", multiple = FALSE, accept = c(".sqlite")),
            
            conditionalPanel(
            condition = "output.fileUploaded != 0",
                # Input: Select score
                selectInput("score", "Select score",
                              choices = list("NA" = 1),
                                        selected = 1),

                # Input: Select a daterange
                dateRangeInput("dates","Date range"),

                # Input: Select model
                selectInput("model", "Select model(s)",
                              choices = list("NA" = 1),
                                        selected = 1),

                # Input: Select leadtimes
                selectInput("leadtime", "Select leadtimes (hours)",
                              choices = list("NA" = 1),
                                        selected = 1,
                                        multiple = TRUE),

                # Input: Select param
                selectInput("param", "Select parameter",
                              choices = list("NA" = 1),
                                        selected = 1),

                # Input: Plot/Show data using selections above
                actionButton("showdata","Show Data")
            )
        ),
		
        # Main panel for displaying outputs ----
        mainPanel(
        # Output: Data file ----
          tabsetPanel(id = "tab_panel",
                        tabPanel("Plot", plotOutput("plot")),
                        tabPanel("Table", DT::dataTableOutput("table")),
          )
        ) # end of main panel
      ) # end of sidebar layout
    ) # end of fluid page
  ) # end of tags$body
) # end of tags$html
