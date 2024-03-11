#' @param col The column to plot
#' @export
plot.harp_grid_df <- function(
  x,
  col    = NULL,
  map_db = "world",
  poly   = FALSE,
  ...
) {

  col <- rlang::enquo(col)
  if (rlang::quo_is_null(col)) {
    geolist_cols <- find_geolist_cols(x)
  } else {
    geolist_cols <- gsub(
      "c\\(| |\\)",
      "",
      strsplit(rlang::quo_name(col), ",")[[1]]
    )
  }

  if (length(geolist_cols) > 1) {
    cli::cli_abort(c(
      "More than one {.cls geolist} columns in data frame.",
      "x" = "You can only plot one {.cls geolist} column.",
      "i" = "You can select which column to plot by passing one of",
      "i" = "{glue::glue_collapse(geolist_cols, last = ' or ')} to the {.arg col} argument."
    ))
  }

  col <- rlang::enquo(geolist_cols)

  land_map <- get_map(x, map = map_db, polygon = poly, col = !!col)

  ggplot2::ggplot() +
    geom_georaster(ggplot2::aes(geofield = .data[[col]]), x[1, ]) +
    geom_path(aes(x, y), land_map)


}

find_geolist_cols <- function(x) {
  names(which(sapply(x, harpCore::is_geolist)))
}

check_quo_col_length <- function(x, max_len = 1) {
  x <- rlang::enquo(x)
  cols <- strsplit(rlang::quo_name(x), ",")
  cols <- gsub("")
}


