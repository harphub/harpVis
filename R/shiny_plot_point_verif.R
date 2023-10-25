#' Start shiny app to plot point verification
#'
#' @export
shiny_plot_point_verif <- function(
  start_dir           = NULL,
  full_dir_navigation = TRUE,
  theme               = c("dark", "light", "white"),
  online              = TRUE,
  ...
) {
  if (!is.null(start_dir) && !dir.exists(start_dir)) {
    stop("\"", start_dir, "\" does not exist on your system.", call. = FALSE)
  }
  theme <- match.arg(theme)
  app_dir <- system.file("shiny_apps/plot_point_verif", package = "harpVis")
  shiny::shinyOptions(
    app_start_dir = start_dir, online = online, theme = theme,
    full_dir_navigation = full_dir_navigation
  )
  shiny::runApp(app_dir, ...)
}
