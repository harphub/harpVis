begindate <- reactive({ 
                 if (input$vPeriod=='month') {
                   bdy <- input$vYear
                   bdm <- input$vMonth
                   bd <- paste(bdy,bdm,1,sep="-")
                 } else if (input$vPeriod=='season') {
                   bdm <- switch(input$vSeason,
                               "winter"=12,"spring"=3,"summer"=6,"autumn"=9)
                   bdy <-  as.numeric(input$vYear) - (input$vSeason=="winter")
                   bd <- paste(bdy,bdm,1,sep="-")
                 } else if (input$vPeriod=="recent") {
                   bd <- format(Sys.Date()-31,"%Y-%m-%d")
                 } else if (input$vPeriod=="custom") {
                   bd <- input$vDateRange[1]
                 }
                 as.POSIXct(bd,format="%Y-%m-%d",tz="UTC") 
})

enddate <- reactive({ 
                 if (input$vPeriod=='month') {
                   edy <- input$vYear
                   edm <- input$vMonth
                   ed <- paste(edy,edm,DaysInMonth(edy,edm),sep="-")
                 } else if (input$vPeriod=='season') {
                   edm <- switch(input$vSeason,
                               "winter"=2,"spring"=5,"summer"=8,"autumn"=10)
                   edy <- input$vYear
                   ed <- paste(edy,edm,DaysInMonth(edy,edm),sep="-")
                 } else if (input$vPeriod=="recent") {
                   ed <- format(begindate()+24*3600*30,"%Y-%m-%d")
                 } else if (input$vPeriod=="custom") {
                   ed <- input$vDateRange[2]
                 }
                 as.POSIXct(ed,format="%Y-%m-%d",tz="UTC") 
})

