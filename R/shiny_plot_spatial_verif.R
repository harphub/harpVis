#' Start shiny app to plot spatial verification
#'
#' @export
shiny_plot_spatial_verif <- function(start_dir = NULL, ...) {
  if (!is.null(start_dir) && !dir.exists(start_dir)) {
    stop("\"", start_dir, "\" does not exist on your system.", call. = FALSE)
  }
  app_dir <- system.file("shiny_apps/plot_spatial_verif", package = "harpVis")
  shiny::shinyOptions(app_start_dir = start_dir)
  shiny::runApp(app_dir, ...)
}
