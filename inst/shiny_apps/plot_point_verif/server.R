# server for shiny_plot_point_verif

server <- function(input, output, session) {

  ############################################################
  # LOAD DATA                                                #
  ############################################################

  verif_data <- shiny::callModule(harpVis::options_bar, "options_bar")

  ############################################################
  # FILTER DATA                                              #
  ############################################################

  filtered_data <- shiny::callModule(
    harpVis::group_selectors, "group_selectors", verif_data
  )

  ############################################################
  # GET TIME AXIS                                            #
  ############################################################

  time_axis_out <- shiny::callModule(harpVis::time_axis, "time_axis", filtered_data)

  ############################################################
  # GET COLOUR TABLE                                         #
  ############################################################

  colour_table <- shiny::callModule(
    harpVis::colour_choices, "colour_choices", time_axis_out$filtered_data
  )

  ############################################################
  # DASHBOARD PLOTS                                          #
  ############################################################

  shiny::callModule(
    harpVis::dashboard_point_verif, "dashboard", time_axis_out$filtered_data,
    colour_table, time_axis_out$time_axis
  )

  ############################################################
  # INTERACTIVE PLOT                                         #
  ############################################################

  score_optons <- shiny::callModule(
    harpVis::interactive_eps,
    "interactive",
    time_axis_out$filtered_data,
    colour_table,
    time_axis_out$time_axis
  )

  ############################################################
  # DOWNLOAD HANDLER                                         #
  ############################################################

  shiny::callModule(
    harpVis::download_verif_plot,
    "download_plot",
    time_axis_out$filtered_data,
    score_optons,
    colour_table
  )

}
