
<!-- README.md is generated from README.Rmd. Please edit that file -->

# harpVis <img src='man/figures/harp_logo_dark.svg' align="right" width = "80" />

*Visualization of meteorological and verification data*

harpVis provides functions for plotting data produced by other harp
packages, whether that be gridded forecast data from harpIO or
verification data produced by harpPoint and harpSpatial.

Most of the plotting functions (except for `plot_field()`) use the
[ggplot2](https://ggplot2.tidyverse.org/) package in the background,
meaning that the plots can be saved with
[`ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html).

In addition to direct plotting, harpVis provides
[shiny](https://shiny.posit.co/) apps for interactively displaying
verification plots in a web browser, along with the option to build your
own apps from modules.

## Installation

You can install harpVis from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("harphub/harpVis")
```
