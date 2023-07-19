#' Title
#'
#' @param map
#' @param dom
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
get_map <- function(map = "world", dom = NULL, poly = TRUE, ...) {

  if (meteogrid::is.geodomain(dom) || meteogrid::is.geofield(dom)) {

    dom <- meteogrid::as.geodomain(dom)

    map_data <- try(
      meteogrid::getmap(dom, fill = poly, map.database = map),
      silent = TRUE
    )

    if (inherits(map_data, "try-error")) {
      if (poly) {
        warning("Could not clip polygons properly. Paths returned.")
        poly     <- FALSE
        map_data <- meteogrid::getmap(dom, fill = poly, map.database = map)
      } else {
        stop("Could not extract domain")
      }
    }

    if (poly) {
      map_data <- ggplot2::fortify(map_data)
    } else {
      map_data <- data.frame(x = map_data[["x"]], y = map_data[["y"]])
    }

    names(map_data)[names(map_data) == "long"] <- "x"
    names(map_data)[names(map_data) == "lat"]  <- "y"

  }

  map_data

}

# ' Get domain information from a harp object
# '
# ' @param x A harp object - can be a geofield, geolist, harp spatial data frame,
# '   harp analysis or harp forecast
# ' @param col The column from which to extract the domain for harp objects that
# '   include data frames
# '
# ' @return An object (or list of objects) of class "geodomain"
# ' @export
# '
# ' @examples
#' get_domain <- function(x, ...) {
#'   UseMethod("get_domain")
#' }
#'
#' #' @export
#' get_domain.geofield <- function(x) {
#'   meteogrid::as.geodomain(x)
#' }
#'
#' #' @export
#' get_domain.geolist <- function(x) {
#'   get_domain(x[[1]])
#' }
#'
#' #' @export
#' get_domain.harp_spatial_fcst <- function(x, col) {
#'   get_domain(dplyr::pull(x, {{col}}))
#' }
#'
#' #' @export
#' get_domain.harp_analysis <- function(x, col) {
#'   res <- lapply(x, get_domain, col = {{col}})
#'   names(res) <- names(x)
#'   if (length(res) < 2) {
#'     res <- res[[1]]
#'   }
#'   res
#' }
#'
#' #' @export
#' get_domain.harp_fcst <- function(x, col) {
#'   res <- lapply(x, get_domain, col = {{col}})
#'   names(res) <- names(x)
#'   if (length(res) < 2) {
#'     res <- res[[1]]
#'   }
#'   res
#' }
