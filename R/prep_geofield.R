prep_geofield <- function(
  geo,
  upscale_factor      = 1,
  method              = "mean",
  downsample_location = "centre",
  data_frame          = TRUE,
  ...
) {

  stopifnot(meteogrid::is.geofield(geo))

  if (upscale_factor > 1) {
    geo <- harpCore::geo_upscale(
      geo,
      factor              = upscale_factor,
      method              = method,
      downsample_location = downsample_location,
      ...
    )
  }

  if (data_frame) {

    geo_extent <- meteogrid::DomainExtent(geo)

    df   <- expand.grid(
      x = seq(geo_extent$x0, geo_extent$x1, geo_extent$dx),
      y = seq(geo_extent$y0, geo_extent$y1, geo_extent$dy)
    )
    df$z <- as.vector(geo)

    df

  } else {

    geo

  }

}

get_crs <- function(geo) {

  stopifnot(meteogrid::is.geofield(geo) || meteogrid::is.geodomain(geo))

  crs <- meteogrid::proj4.list2str(attr(geo, "domain")[["projection"]])

  paste0("+", paste(crs, collapse = " +"))

}

get_bbox <- function(geo) {

  stopifnot(meteogrid::is.geofield(geo) || meteogrid::is.geodomain(geo))

  bbox <- meteogrid::DomainExtent(geo)

  c(
    xmin = bbox$x0 - bbox$dx / 2,
    ymin = bbox$y0 - bbox$dy / 2,
    xmax = bbox$x1 + bbox$dx / 2,
    ymax = bbox$y1 + bbox$dy / 2
  )

}

crop_geofield <- function(geo, bbox) {

  stopifnot(meteogrid::is.geofield(geo))

  domain_atts <- attr(geo, "domain")

  geo_bbox <- meteogrid::DomainExtent(geo)
  geo_x    <- seq(geo_bbox$x0, geo_bbox$x1, length.out = geo_bbox$nx)
  geo_y    <- seq(geo_bbox$y0, geo_bbox$y1, length.out = geo_bbox$ny)

  if (bbox["xmin"] > min(geo_x) && bbox["xmin"] < max(geo_x)) {
    x_min <- closest(geo_x, bbox["xmin"], "lt")
  } else {
    x_min <- 1
  }

  if (bbox["xmax"] > min(geo_x) && bbox["xmax"] < max(geo_x)) {
    x_max <- closest(geo_x, bbox["xmax"], "gt")
  } else {
    x_max <- length(geo_x)
  }

  if (bbox["ymin"] > min(geo_y) && bbox["ymin"] < max(geo_y)) {
    y_min <- closest(geo_y, bbox["ymin"], "lt")
  } else {
    y_min <- 1
  }

  if (bbox["ymax"] > min(geo_y) && bbox["ymax"] < max(geo_y)) {
    y_max <- closest(geo_y, bbox["ymax"], "gt")
  } else {
    y_max <- length(geo_y)
  }

  geo_atts          <- attributes(geo)
  geo               <- geo[x_min:x_max, y_min:y_max]
  geo_atts[["dim"]] <- dim(geo)
  attributes(geo)   <- geo_atts

  domain_atts[["nx"]] <- x_max - x_min + 1
  domain_atts[["ny"]] <- y_max - y_min + 1
  domain_atts[["SW"]] <- as.numeric(
    meteogrid::project(geo_x[x_min], geo_y[y_min], proj = domain_atts$projection, inv = TRUE)
  )
  domain_atts[["NE"]] <- as.numeric(
    meteogrid::project(geo_x[x_max], geo_y[y_max], proj = domain_atts$projection, inv = TRUE)
  )

  attr(geo, "domain") <- domain_atts

  geo

}

# Find the closest index in a vector to a value
# Can find the absolute closest, or the closest index
# that is less than or greater than the value
closest <- function(vec, val, dirn = c("abs", "lt", "gt")) {

  dirn <- match.arg(dirn)

  closest_index <- findInterval(val, vec)

  mini_vec <- vec[c(closest_index -1, closest_index, closest_index + 1)]

  if (dirn == "abs") {
    return(closest_index)
  }

  if (dirn == "lt") {
    mini_vec <- val - mini_vec
  }

  if (dirn == "gt") {
    mini_vec <- mini_vec - val
  }

  mini_vec[mini_vec < 0] <- NA

  which.min(mini_vec) - 2 + closest_index

}

