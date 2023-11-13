#' Start shiny app to plot point verification
#'
#' @param start_dir The directory start the app in. The directory tree above
#'   this directory will not be visible.
#' @param full_dir_navigation Whether to have full navigation of the directory
#'   tree below `app_start_dir` via a modal (TRUE - the default), or a dropdown
#'   selector of directories that contain harp point verification files (FALSE).
#' @param theme The colour theme of the app - can be one of "dark", "light" or
#'   "white".
#' @param online Logical. Whether the app should have access to the internet
#'   (e.g. to download fonts) or not.
#' @param ... Options to \code{\link[shiny]{runApp}}.
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
