library(shiny)
library(DBI)
library(dplyr)
library(ggplot2)

shinyServer(function(input, output,session) {
  source("./helpers.R", local=TRUE)

## begindate() and enddate() reactives
  source("./set_dates.R", local=TRUE)

## SAL and Fuzzy plotting
  source("./plot_functions.R", local=TRUE)

########################
## LOAD STANDARD DATA ##
########################

#  model <- reactive({ input$model })
#  ldt <- reactive({ as.numeric(input$ldt) })

  dbf_verif <- reactive({
## start with default value
## update when the "OK" is clicked!
## then isolate the value
    input$SQLbutton
    sqlfile <- isolate(input$sqlfile)
    if (!is.null(sqlfile) && file.exists(sqlfile)) src_sqlite(sqlfile)
    else {
      warning(paste("file", sqlfile, "not found"))
      NULL
    }
  })

# don't filter on model and parameter here: do that in the plot routines
# ALSO: avoid collect() if you can: that's when you actually run the SQL command
# TODO: maybe it is more efficient to have data sets for the different scores
#       then you can switch between scores much faster
  myData <- reactive({
    if (is.null(dbf_verif())) {
      NULL 
    } else {
# TODO: use harpenv$score.tables(input$tab1)$tabname
      tabname <- switch(input$tab1, "SAL"="basic", "fuzzy"="fuzzy")
    
      tbl(dbf_verif(), tabname) # %>%
#              filter(model==myModel, prm==myParam) %>%
#              collect() %>%
#              filter(between(fcdate,bdate, edate))
    }
  })

  daterange <- reactive({
    if (is.null(myData())) {
      maxdate <- Sys.Date() - 1
      mindate <- maxdate - 30
      startdate <- mindate
    } else {
      # FIXME: myData()$fcdate needs to be transformed to date!
      maxd <- collect(myData() %>% summarize_at("fcdate", max))$fcdate
      mind <- collect(myData() %>% summarize_at("fcdate", min))$fcdate
      mindate <- lubridate::ymd(mind)#, format="%Y%m%d") # lubridate::ymd_h(, truncated=2)
      maxdate <- lubridate::ymd(maxd)
      startdate <- max(mindate, maxdate - 30)
    }
    format(c(mindate, maxdate, startdate), "%Y-%m-%d")
  })
  output$UI_daterange <- renderUI({
    dateRangeInput( "vDateRange", "Date range:", format="yyyy-mm-dd",
                    min=daterange()[1], max=daterange()[2],
                    start=daterange()[3], end=daterange()[2])
  })


  prmlist <- reactive({ if (is.null(myData())) character(0) else collect(myData() %>% distinct(prm))$prm })
  modellist <- reactive({ if (is.null(myData())) character(0) else collect(myData() %>% distinct(model))$model })
  ldtlist <- reactive({ if (is.null(myData()) || is.null(input$model) || is.null(input$prm)) character(0) 
                        else {
                          myModel <- input$model
                          myPrm <- input$prm
                          datasub <- myData() %>% filter(model==myModel, prm==myPrm)
                          sort(collect(datasub %>% distinct(leadtime))$leadtime)
                        }
  })

  runlist <- reactive({ if (is.null(myData()) || is.null(input$model) || is.null(input$prm)) character(0) 
                        else {
                          myModel <- input$model
                          myPrm <- input$prm
                          datasub <- myData() %>% filter(model==myModel, prm==myPrm)
                          sort(collect(datasub %>% distinct(fctime))$fctime)
                        }
  })

               
 
### which parameters are available for chosen data set:
### "selected=input$prm" is a bit risky, but it works
### I guess input$prm would only be changed AFTER the change in prmlist
### so setting the default to de current value works OK.
### BAD side effect: none found as yet...
  output$UI_prmlist <- renderUI({
    selected.def <- if (is.null(input$prm)) prmlist()[1] else isolate(input$prm)
    selectInput("prm", "Parameter", choices=prmlist(), selected=selected.def)
  })

  output$UI_modellist <- renderUI({
    selected.def <-  isolate(input$model)
    selectInput("model", "Model", choices=modellist(), selected=selected.def)
  })

  output$UI_ldtlist <- renderUI({
    selected.def <-  isolate(input$ldt)
    selectInput("ldt", "Lead time", choices=ldtlist(), selected=selected.def)
  })

  output$UI_runlist <- renderUI({
    selected.def <-  isolate(input$fct)
    selectInput("fct", "Run time", choices=fctlist(), selected=selected.def)
  })



#  output$verifPrm <- renderText({ input$vPrm })
#  output$verifPeriod <- renderText({ paste(begindate(),"to",enddate(), "(",as.integer(begindate()),",",as.integer(enddate()),")") })
#  output$verifTest <- renderText({ input[["range1"]] })

  plot_data <- reactive({
    if (is.null(myData())) stop("No data loaded (yet)")
    # 1. data selection
    myModel <- input$model
    myParam <- input$prm
    myLdt <- as.numeric(input$ldt)
    myFct <- as.numeric(input$fct)
    bdate <- VF_fcdate_format(begindate())
    edate <- VF_fcdate_format(enddate())
#    stop(paste(myModel,myParam,myLdt,myFct,bdate,edate))
    # 
    # using %in% with a length one vector doesn't work in dplyr
    if (length(myFct) > 1) {
      plot_data <- myData() %>%
        filter(model==myModel, prm==myParam, 
               leadtime==myLdt, between(fcdate, bdate, edate),
               fctime %in% myFct ) %>%
        collect()
    } else if (length(myFct) == 1){
      plot_data <- myData() %>%
        filter(model==myModel, prm==myParam, 
               leadtime==myLdt, between(fcdate, bdate, edate),
               fctime==myFct ) %>%
        collect()
    } else {
      stop("No forecast time selected.")
    }

#    str(plot_data)
#    cat("test")
    plot_data
    
  })


  output$plotSAL <- renderPlot({
    myModel <- input$model
    myParam <- input$prm
    myLdt <- as.numeric(input$ldt)
    myFct <- as.numeric(input$fct)
       
#    VerifPlot <- fPlotSAL(plot_data(), myModel, myParam, begindate(),enddate())
    VerifPlot <- harpVis::plot_spatial_verif(plot_data(),
                                             filter_by=vars(model    == input$model,
                                                            prm      == input$prm,
                                                            leadtime == input$ldt,
                                                            fctime   == as.numeric(input$fct)),
                                             score="sal")
    print(VerifPlot)
  }, width=500,height=500)

  output$plotFuzzy <- renderPlot({
    
    myModel <- input$model
    myParam <- input$prm
    myLdt <- as.numeric(input$ldt)
    myFct <- as.numeric(input$fct)
    myScore <- input$fuzzyScore
    myStyle <- input$fuzzyStyle
    VerifPlot <- fPlotFuzzy(plot_data(), myModel, myParam, myScore, myStyle, begindate(),enddate())
    ## FIXME: the following fails because score=input$fuzzyScore is interpreted as a name, not a variable
    ## So we keep the old code until we fix this:
    #    VerifPlot <- harpVis::plot_spatial_verif(plot_data(),
#                                             filter_by=vars(model    == input$model,
#                                                            prm      == input$prm,
#                                                            leadtime == input$ldt,
#                                                            fctime   == as.numeric(input$fct)),
#                                             score=input$fuzzyScore)
    print(VerifPlot)
  }, width=500,height=450)
  
  # output$plotBasic <- renderPlot({
  #   
  #   plot_data <- plot_data()
  #   myModel <- input$model
  #   myParam <- input$prm
  #   myLdt <- as.numeric(input$ldt)
  #   myFct <- as.numeric(input$fct)
  #   bdate <- VF_fcdate_format(begindate())
  #   edate <- VF_fcdate_format(enddate())
  #   myScore <- input$basicScore
  #   myStyle <- input$basicStyle
  #   
  #   VerifPlot <- fPlotBasic(plot_data, myModel, myParam, myScore, myStyle, begindate(),enddate())
  #   print(VerifPlot())
  #   
  # }, width=500,height=450)
  
  output$DataTable <- renderDataTable({
    
    data4plot <- plot_data()
  
    # leadtime <- input$ldt
    # fctime <- input$fct
    # model <- input$model
    # prm <- input$prm
    # score <- input$fuzzyScore
    # 
    # data4plot <- collect(myData() %>% 
    #                        filter(model==input$model, prm==input$prm, score==input$fuzzyScore, 
    #                               leadtime==input$ldt, fctime==input$fct))
    
    # str(data4plot)
   # str(model)
    data4plot
    
  })

})
