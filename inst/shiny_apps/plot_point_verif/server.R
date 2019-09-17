# server for shiny_plot_point_verif

server <- function(input, output, session) {

  # bg_colour = "#D5D5D5"
  bg_colour = "#0A0A2C"


  ############################################################
  # LOAD DATA                                                #
  ############################################################

  verif_data <- shiny::callModule(harpVis::options_bar, "options_bar")

  ############################################################
  # FILTER DATA                                              #
  ############################################################

  filtered_data <- shiny::callModule(harpVis::group_selectors, "group_selectors", verif_data)

  ############################################################
  # GET COLOUR TABLE                                         #
  ############################################################

  colour_table <- shiny::callModule(harpVis::colour_choices, "colour_choices", filtered_data)

  ############################################################
  # DASHBOARD PLOTS                                          #
  ############################################################

  shiny::callModule(harpVis::dashboard_eps, "dashboard", filtered_data, colour_table)

  ############################################################
  # INTERACTIVE PLOT                                         #
  ############################################################

  shiny::callModule(harpVis::interactive_eps, "interactive", filtered_data, colour_table, bg_colour = bg_colour)

}
