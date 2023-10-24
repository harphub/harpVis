CoordGeo <- ggproto("CoordGeo", CoordSf,

  setup_params = function(self, data) {
    crs  <- self$determine_crs(data)
    bbox <- self$determine_bbox(data)

    params <- list(
      crs = crs,
      default_crs = self$default_crs %||% crs,
      bbox = bbox
    )
    self$params <- params

    params
   },

  determine_crs = function(self, data) {
    if (!is.null(self$crs)) {
      return(self$crs)
    }

    for (layer_data in data) {
      if (has_geolist(layer_data)) {
        crs <- first_crs(layer_data)
      } else {
        next
      }

      if (is.na(crs)) {
        next
      }

      return(crs)
    }

    NULL
  },

  determine_bbox = function(self, data) {
    if (!is.null(self$bbox)) {
      return(self$bbox)
    }

    for (layer_data in data) {
      if (has_geolist(layer_data)) {
        bbox <- first_bbox(layer_data)
      } else {
        next
      }

      if (all(is.na(bbox))) {
        next
      }

      return(bbox)
    }

    NULL
  },

  # Transform all layers to common CRS (if provided)
  setup_data = function(data, params) {
    if (is.null(params$crs))
      return(data)

    data <- lapply(data, function(layer_data) {
      if (ggplot2:::is_sf(layer_data)) {
        return(sf::st_transform(layer_data, params$crs))
      } else if(has_geolist(layer_data)) {
        if (first_crs(layer_data) != params$crs) {
          rlang::abort("Cannot reproject gridded geofields.")
        }
        return(layer_data)
      } else {
        return(layer_data)
      }
    })

    if (is.null(params$bbox)) {
      return(data)
    }

    lapply(data, function(layer_data) {
      if (ggplot2:::is_sf(layer_data)) {
        layer_data <- sf::st_buffer(layer_data, dist = 0)
        return(suppressWarnings(sf::st_crop(layer_data, params$bbox)))
      } else if(has_geolist(layer_data)) {
        return(crop_geolist(layer_data, params$bbox))
      } else {
        return(layer_data)
      }
    })

  }

)

#' Co-ordinates for geofield geoms
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This is basically just an implementation of \code{\link[ggplot2]{coord_sf()}}
#' for \code{\link{geom_georaster}}. It requires the \code{\link[sf]} package
#' and should very much be considered experimental. It is recommended to use
#' \code{\link[ggplot2]{coord_equal()}} for the time being as errors are likely.
#'
#' @inheritParams ggplot2::coord_sf
#' @export
#'
coord_geo <- function(xlim = NULL, ylim = NULL, expand = TRUE,
  crs = NULL, default_crs = sf::st_crs(4326),
  datum = sf::st_crs(4326),
  label_graticule = waiver(),
  label_axes = waiver(), lims_method = c("cross", "box", "orthogonal", "geometry_bbox"),
  ndiscr = 100, default = FALSE, clip = "on") {

  if (ggplot2:::is.waive(label_graticule) && ggplot2:::is.waive(label_axes)) {
    # if both `label_graticule` and `label_axes` are set to waive then we
    # use the default of labels on the left and at the bottom
    label_graticule <- ""
    label_axes <- "--EN"
  } else {
    # if at least one is set we ignore the other
    label_graticule <- label_graticule %|W|% ""
    label_axes <- label_axes %|W|% ""
  }

  if (is.character(label_axes)) {
    label_axes <- parse_axes_labeling(label_axes)
  } else if (!is.list(label_axes)) {
    abort("Panel labeling format not recognized.")
    label_axes <- list(left = "N", bottom = "E")
  }

  if (is.character(label_graticule)) {
    label_graticule <- unlist(strsplit(label_graticule, ""))
  } else {
    abort("Graticule labeling format not recognized.")
    label_graticule <- ""
  }

  # switch limit method to "orthogonal" if not specified and default_crs indicates projected coords
  if (is.null(default_crs) && is_missing(lims_method)) {
    lims_method <- "orthogonal"
  } else {
    lims_method <- match.arg(lims_method)
  }

  ggproto(NULL, CoordGeo,
    limits = list(x = xlim, y = ylim),
    lims_method = lims_method,
    datum = datum,
    crs = crs,
    default_crs = default_crs,
    label_axes = label_axes,
    label_graticule = label_graticule,
    ndiscr = ndiscr,
    expand = expand,
    default = default,
    clip = clip
  )
}

parse_axes_labeling <- function(x) {
  labs = unlist(strsplit(x, ""))
  list(top = labs[1], right = labs[2], bottom = labs[3], left = labs[4])
}

has_geolist <- function(data) {
  any(sapply(data, inherits, what = "geolist"))
}

first_crs <- function(data) {
  geolist_col <- which(sapply(data, inherits, what = "geolist"))
  if (length(geolist_col) > 1) geolist_col <- geolist_col[1]
  get_crs(data[[geolist_col]][[1]])
}

first_bbox <- function(data) {
  geolist_col <- which(sapply(data, inherits, what = "geolist"))
  if (length(geolist_col) > 1) geolist_col <- geolist_col[1]
  get_bbox(data[[geolist_col]][[1]])
}

crop_geolist <- function(data, bbox) {
  geolist_col <- which(sapply(data, inherits, what = "geolist"))
  if (length(geolist_col) > 1) geolist_col <- geolist_col[1]
  data[[geolist_col]] <- structure(
    lapply(data[[geolist_col]], crop_geofield, bbox),
    class = c("geolist", class(data[[geolist_col]]))
  )
  data
}
