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
get_map <- function(map = "world2", dom = NULL, ...) {


  map <- ggplot2::map_data(map, ...)
  # Antarctica causes problems
  map <- map[map$region != "Antarctica", ]

  if (isTRUE(meteogrid::is.geodomain(dom) || meteogrid::is.geofield(dom))) {
    dom       <- meteogrid::as.geodomain(dom)
    glimits   <- meteogrid::DomainExtent(dom)
    map_names <- unique(paste(map$region, map$group, sep = ":"))
    map       <- split(map, map$group)
    map       <- dplyr::bind_rows(lapply(map, rbind, NA))
    map       <- map[1:(nrow(map) - 1), ]

    map       <- as.list(meteogrid::project(map, proj = dom$projection))
    map$names <- map_names

    map <- maps::map.clip.poly(
      as.list(map),
      xlim = c(glimits$x0, glimits$x1) + glimits$dx * c(-1, 1) / 2,
      ylim = c(glimits$y0, glimits$y1) + glimits$dy * c(-1, 1) / 2,
      poly = TRUE
    )

    map <- ggplot2::map_data(map)

    names(map)[names(map) == "long"] <- "x"
    names(map)[names(map) == "lat"]  <- "y"

  }

  map

}
#ggplot(map_poly, aes(x, y, group = group)) + geom_polygon(fill = "seagreen3", colour = "grey30", size = 2)
