# Themes for harp plots


#' @export
theme_harp_grey <- function (base_size = 11, base_family = "", base_line_size = base_size/22,
  base_rect_size = base_size/22)
{
  ggplot2::theme_grey(base_size = base_size, base_family = base_family,
    base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace%
    ggplot2::theme(
      plot.background   = ggplot2::element_rect(fill = "#D5D5D5", colour = NA),
      panel.background  = ggplot2::element_rect(fill = "#D5D5D5", colour = NA),
      panel.border      = ggplot2::element_rect(fill = NA, colour = "grey20"),
      panel.grid        = ggplot2::element_line(colour = "grey70"),
      panel.grid.minor  = ggplot2::element_line(size = ggplot2:::rel(0.5)),
      strip.background  = ggplot2::element_rect(fill = "#AAAAAA", colour = "grey20"),
      legend.key        = ggplot2::element_rect(fill = "#D5D5D5", colour = NA),
      legend.background = ggplot2::element_rect(fill = "#D5D5D5", colour = NA),
      complete          = TRUE
    )
}

#' @export
theme_harp_black <- function (base_size = 11, base_family = "", base_line_size = base_size/22,
  base_rect_size = base_size/22)
{
  ggplot2::theme_grey(base_size = base_size, base_family = base_family,
    base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace%
    ggplot2::theme(
      text = element_text(
        family = base_family,
        face = "plain",
        colour = "grey72",
        size = base_size,
        lineheight = 0.9,
        hjust = 0.5,
        vjust = 0.5,
        angle = 0,
        margin = margin(),
        debug = FALSE
      ),
      axis.text         = element_text(size = rel(0.8), colour = "grey50"),
      plot.background   = ggplot2::element_rect(fill = "#000000", colour = NA),
      panel.background  = ggplot2::element_rect(fill = "#000000", colour = NA),
      panel.border      = ggplot2::element_rect(fill = NA, colour = "grey92"),
      panel.grid        = ggplot2::element_line(colour = "grey30"),
      panel.grid.minor  = ggplot2::element_line(size = ggplot2:::rel(0.5)),
      strip.background  = ggplot2::element_rect(fill = "grey22", colour = "grey92"),
      strip.text        = ggplot2::element_text(colour = "grey92"),
      legend.key        = ggplot2::element_rect(fill = "#000000", colour = NA),
      legend.background = ggplot2::element_rect(fill = "#000000", colour = NA),
      complete          = TRUE
    )
}

#' @export
theme_harp_midnight <- function (base_size = 11, base_family = "", base_line_size = base_size/22,
  base_rect_size = base_size/22)
{
  ggplot2::theme_grey(base_size = base_size, base_family = base_family,
    base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace%
    ggplot2::theme(
      text = element_text(
        family = base_family,
        face = "plain",
        colour = "grey72",
        size = base_size,
        lineheight = 0.9,
        hjust = 0.5,
        vjust = 0.5,
        angle = 0,
        margin = margin(),
        debug = FALSE
      ),
      axis.text         = element_text(size = rel(0.8), colour = "grey50"),
      plot.background   = ggplot2::element_rect(fill = "#0A0A2C", colour = NA),
      panel.background  = ggplot2::element_rect(fill = "#0A0A2C", colour = NA),
      panel.border      = ggplot2::element_rect(fill = NA, colour = "grey92"),
      panel.grid        = ggplot2::element_line(colour = "grey30"),
      panel.grid.minor  = ggplot2::element_line(size = ggplot2:::rel(0.5)),
      strip.background  = ggplot2::element_rect(fill = "#4E5C68", colour = "grey92"),
      strip.text        = ggplot2::element_text(colour = "grey92"),
      legend.key        = ggplot2::element_rect(fill = "#0A0A2C", colour = NA),
      legend.background = ggplot2::element_rect(fill = "#0A0A2C", colour = NA),
      complete          = TRUE
    )
}

#' @export
theme_harp_light <- function (base_size = 11, base_family = "", base_line_size = base_size/22,
  base_rect_size = base_size/22)
{
  ggplot2::theme_bw(base_size = base_size, base_family = base_family,
    base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace%
    ggplot2::theme(
      text = element_text(
        family = base_family,
        face = "plain",
        colour = "#062F4F",
        size = base_size,
        lineheight = 0.9,
        hjust = 0.5,
        vjust = 0.5,
        angle = 0,
        margin = margin(),
        debug = FALSE
      ),
      axis.text         = ggplot2::element_text(size = rel(0.8), colour = "#22477E"),
      axis.title        = ggplot2::element_text(colour = "#062F4F"),
      plot.background   = ggplot2::element_rect(fill = "#F0F1F2", colour = NA),
      panel.background  = ggplot2::element_rect(fill = "#F0F1F2", colour = NA),
      panel.border      = ggplot2::element_rect(fill = NA, colour = "#062F4F"),
      panel.grid        = ggplot2::element_line(colour = "#AABBDD"),
      panel.grid.minor  = ggplot2::element_line(size = ggplot2:::rel(0.5)),
      strip.background  = ggplot2::element_rect(fill = "#22477E", colour = "#062F4F"),
      strip.text        = ggplot2::element_text(colour = "#F0F1F2"),
      legend.key        = ggplot2::element_rect(fill = "#F0F1F2", colour = NA),
      legend.background = ggplot2::element_rect(fill = "#F0F1F2", colour = NA),
      complete          = TRUE
    )
}


#' @export
theme_harp_map <- function (base_size = 11, base_family = "", base_line_size = base_size/22,
  base_rect_size = base_size/22)
{
  ggplot2::theme_bw(base_size = base_size, base_family = base_family,
    base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace%
    ggplot2::theme(
      axis.ticks = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.text  = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
    )
}
