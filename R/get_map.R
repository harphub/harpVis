#' Get map data for a given geodomain
#'
#' Given a specific domain, a map is returned on that domain. The function is
#' intended to be used in conjunction with \code{\link{geom_geocontour}} and /
#' or \code{\link{geom_georaster}} when plotting using `ggplot()`.
#'
#' @param dom A `geodomain`, `geofield`, `geolist`, or data frame with `geolist`
#'   columns.
#' @param map The map database from which to get the map data. The default is
#'   \code{\link[maps]{world}}. Datasets from the \code{rnaturalearth}
#'   package may also be used, but only paths will be returned.
#' @param polygon Logical. Whether to return the data as polygons (`TRUE`), or
#'   paths (`FALSE`). The default is for polygons to be returned.
#' @param col For a `harp_grid_df` data frame, the column from which to get the
#'   domain for the map. Uses the same semantics as \code{\link[dplyr]{select}}.
#' @param ... Not used.
#'
#' @return A data frame with columns "x", "y", and for `polygon = TRUE` an
#'   additional "group" column.
#' @export
#'
#' @examples
#' # Define a domain and make some data
#' arr <- array(dim = c(200, 200))
#' for (i in 1:200) {
#'   for (j in 1:200) {
#'     arr[i, j] <- sin(i / 10) + sin(j / 10)
#'   }
#' }
#'
#' dom <- define_domain(10, 60, 200, 10000)
#' geo <- geofield(arr, domain = dom)
#' data <- data.frame(geo = geolist(geo))
#'
#' # Use polygons
#' map <- get_map(geo)
#' ggplot() +
#'   geom_polygon(
#'     aes(x, y, group = group), map,
#'     fill = "seagreen", colour = "grey10"
#'   ) +
#'   geom_geocontour(aes(geofield = geo, colour = after_stat(level)), data) +
#'   coord_equal(expand = FALSE) +
#'   theme_harp_map()
#'
#' # Use paths
#' map <- get_map(geo, polygon = FALSE)
#' ggplot() +
#'   geom_georaster(aes(geofield = geo), data) +
#'   geom_path(aes(x, y), map, colour = "grey30") +
#'   coord_equal(expand = FALSE) +
#'   theme_harp_map()
#'
get_map <- function(dom = NULL, map = "world", polygon = TRUE, ...) {
  UseMethod("get_map")
}

#' @export
get_map.geodomain <- function(dom = NULL, map = "world", polygon = TRUE, ...) {

  # Fudge for meteogrid to understand "longlat" projection

  if (dom[["projection"]][["proj"]] == "longlat") {
    dom[["projection"]][["proj"]] <- "latlong"
  }

  map_data <- try(
    meteogrid::getmap(dom, fill = polygon, map.database = map),
    silent = TRUE
  )

  if (inherits(map_data, "try-error")) {
    if (polygon) {
      warning("Could not clip polygons properly. Paths returned.")
      polygon     <- FALSE
      map_data <- meteogrid::getmap(dom, fill = polygon, map.database = map)
    } else {
      stop("Could not extract domain")
    }
  }

  if (polygon) {
    map_data <- ggplot2::fortify(map_data)
  } else {
    map_data <- data.frame(x = map_data[["x"]], y = map_data[["y"]])
  }

  names(map_data)[names(map_data) == "long"] <- "x"
  names(map_data)[names(map_data) == "lat"]  <- "y"

  map_data

}

#' @export
get_map.geofield <- function(dom = NULL, map = "world", polygon = TRUE, ...) {
  get_map(harpCore::get_domain(dom), map, polygon, ...)
}

#' @export
get_map.harp_geolist <- get_map.geofield

#' @export
get_map.data.frame <- function(dom = NULL, map = "world", polygon = TRUE, col, ...) {
  col <- rlang::ensym(col)
  geo <- dplyr::pull(dom, !!col)

  if (!harpCore::is_geolist(geo)) {
    col_name <- rlang::as_name(col)
    cli::cli_abort(c(
      "Cannot get domain from {.var col = {col_name}}",
      "x" = "You've supplied a {.cls {class(geo)}} column",
      "i" = "{.arg col} must be a {.cls geolist} column."
    ))
  }

  get_map(harpCore::get_domain(geo), map, polygon, ...)
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
# get_domain <- function(x, ...) {
#   UseMethod("get_domain")
# }
#
# #' @export
# get_domain.geofield <- function(x) {
#   meteogrid::as.geodomain(x)
# }
#
# #' @export
# get_domain.geolist <- function(x) {
#   get_domain(x[[1]])
# }
#
# #' @export
# get_domain.harp_spatial_fcst <- function(x, col) {
#   get_domain(dplyr::pull(x, {{col}}))
# }
#
# #' @export
# get_domain.harp_analysis <- function(x, col) {
#   res <- lapply(x, get_domain, col = {{col}})
#   names(res) <- names(x)
#   if (length(res) < 2) {
#     res <- res[[1]]
#   }
#   res
# }
#
# #' @export
# get_domain.harp_fcst <- function(x, col) {
#   res <- lapply(x, get_domain, col = {{col}})
#   names(res) <- names(x)
#   if (length(res) < 2) {
#     res <- res[[1]]
#   }
#   res
# }
