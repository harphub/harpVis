#' Start shiny app to plot point verification
#'
#' @export
shiny_plot_point_verif <- function() {
  app_dir <- system.file("shiny_apps/plot_point_verif", package = "harpVis")
  shiny::runApp(app_dir)
}
