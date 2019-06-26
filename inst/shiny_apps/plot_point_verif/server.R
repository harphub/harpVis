# server for shiny_plot_point_verif

server <- function(input, output, session) {

  # bg_colour = "#D5D5D5"
  bg_colour = "#0A0A2C"

  verif_data <- shiny::callModule(harpVis::options_bar, "options_bar")


  ############################################################
  # DASHBOARD PLOTS                                          #
  ############################################################

  shiny::callModule(harpVis::dashboard_eps, "dashboard", verif_data)

  ############################################################
  # INTERACTIVE PLOT                                         #
  ############################################################

  shiny::callModule(harpVis::interactive_eps, "interactive", verif_data, bg_colour = bg_colour)

}
