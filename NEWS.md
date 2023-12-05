# harpVis v0.2.1

* Hotfix release that adds functionality to plot new spatial verification
scores

# harpVis v0.2.0

This is a major update. The main focus has been on ensuring that plot functions
for verification scores can handle outputs from harpPoint 0.2.0 while also 
being backwards compatible with outputs from older versions of harpPoint. 

In addition there are some new functionalities. In particular new geoms for 
plotting harp georeferenced gridded data in ggplot, with options to thin the 
data to speed up plotting. 

### Breaking changes

* `plot_scatter()` is soft deprecated. Since the data are computed in 
verification functions, scatter (or more accurately hexbin) plots can now be 
made with `plot_point_verif(..., score = hexbin)`

* `plot_station_eps()` is defunct. `plot_station_ts()` should be used instead. 

### Selected new features

* Plot functions are backwards compatible so that verification data produced by 
old versions of _{harpPoint}_ with `leadtime`, `mname`, `fcdate`, `validdate` 
included in the column names will still work.

* `geom_georaster()`, `geom_geocontour()` and `geom_geocontour_filled()` are new 
functions for plotting gridded data using 
[ggplot](https://ggplot2.tidyverse.org/index.html). An upscaling option is added 
to enable faster plotting of rasters and calculation / smoothing of contours. 

* Map data for plots projected to the domain of a gridded data field can be 
retrieved using `get_map()`.

* The shiny app for point verification can now select from different time axes 
and will recognise and plot vertical profile verifications. 

* The shiny app for point verification has gained two new options: 
`full_dir_navigation` controls whether a modal is opened for selecting data 
directories (the old behaviour), or simply populating a dropdown selector. 
`theme` allows you to control the overall appearance of the app and you can 
choose between "dark", "light" and "white".



# harpVis v0.0.9

* This is the version that is basically unchanged since late 2021 / early 2022. 

* It was officially tagged v0.0.9 in November 2023
