## which parameters are given as choice for verification?
## give the printed description and the internal SQLite column name
prmList <- c(
             "Precipitation"="Pcp",
             "Total cloud cover"="cct",
             "2m Temperature"="t2m"
            )
accum_list <- c("1h"=1, "3h"=3, "6h"=6, "12h"=12)

scoreList_fuzzy <- c("FSS"="fss","ETS"="ets")
styleList_fuzzy <- c("colour", "graph", "table")

scoreList_basic <- c("BIAS"="bias")
styleList_basic <- c("timeline", "table")

styleList_sal <- c("plot", "table")

