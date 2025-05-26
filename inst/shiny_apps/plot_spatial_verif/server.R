# server for shiny_plot_spatial_verif
library("harpIO")
library("tidyverse")
library("dplyr")
library("RSQLite")
library("DT")
library("DBI")
options(shiny.maxRequestSize=20*1024^2)
Sys.setenv(TZ='UTC')

read_sql <- function(filepath,score=NULL){
    #TODO: this function might need a more appropriate name
    sql_object <- harpIO:::dbopen(gsub("\\\\","/",filepath))
    scores <- dbListTables(sql_object)
    if(is.null(score)) {score=scores[1]}
    verif_data <- as.data.frame(harpIO:::dbquery(sql_object, paste("SELECT * FROM ",score))) #can choose first score as default
    verif_data <- verif_data %>% dplyr::mutate(
      dates = lubridate::as_datetime(fcdate,
                                     origin = lubridate::origin,
                                     tz = "UTC")
    )
    if (!("fcst_cycle" %in% names(verif_data))) {
      verif_data <- verif_data %>% dplyr::mutate(
        fcst_cycle = substr(harpIO::YMDh(dates),9,10)
      )
    }
    harpIO:::dbclose(sql_object)
    items <- list("verif_data" = verif_data, "scores" = scores)
    return(items) #returns a list of dataframe and list of scores
}

update_options <- function(input,scores,session) {
    #TODO: this function might need a more appropriate name
    dates  <- unique(input$dates)
    cycles <- sort(unique(input$fcst_cycle))
    models <- sort(unique(input$model))
    ref_models <- c("NA",models)
    params <- unique(input$prm)
    params_pcp <- params[grepl("AccPcp",params,fixed=T)]
    params_oth <- params[!grepl("AccPcp",params,fixed=T)]
    if (length(params_pcp) > 0){
      params_ord <- order(as.numeric(gsub("h","",gsub("AccPcp","",params_pcp))))
      params_pcp <- params_pcp[params_ord]
    }
    params    <- c(params_pcp,params_oth)
    leadtimes <- sort(unique(input$leadtime)/3600)
    updateSelectInput(session,'score',      choices=c(scores),
                                                selected=c(scores)[1])
    updateDateRangeInput(session,'dates',   start=dates[1],
                                            end=tail(dates,n=1),
                                            min=min(dates),
                                            max=max(dates))
    updateSelectInput(session,'model',      choices=c(models),
                                                selected=c(models)[1])
    updateSelectInput(session,'ref_model',
                      choices=c(ref_models),
                      selected=c(ref_models)[1])
    updateSelectInput(session,'cycle',
                      choices = c(cycles),
                      selected = c(cycles))
    updateSelectInput(session,'leadtime',   choices=c(leadtimes),
                                                selected = c(leadtimes))
    updateSelectInput(session,'param',      choices=c(params),
                                                selected=c(params)[1])
    if ("scale" %in% names(input)) {
      scales <- sort(unique(input$scale))
      updateSelectInput(session,'scales',
                        choices=c(scales),
                        selected=c(scales))
    }
    if ("threshold" %in% names(input)) {
      thresholds <- sort(unique(input$threshold))
      updateSelectInput(session,'thresholds',
                        choices=c(thresholds),
                        selected=c(thresholds))
    }
}

server <- function(input, output, session) {


  ############################################################
  # LOAD DATA                                                #
  ############################################################
  app_start_dir <- shiny::getShinyOption("app_start_dir")
  if (!is.null(app_start_dir)) {
    
      if (dir.exists(app_start_dir)) {
        volumes <- unclass(fs::path(app_start_dir))
        names(volumes)[1] <- app_start_dir
      } else {
        stop("app_start_dir not found on the system")
      }
      shinyFiles::shinyFileChoose(input,
                                  'filein',
                                  roots = volumes,
                                  filetypes = c('sqlite'))
      filein <- shiny::reactiveVal()
      shiny::observeEvent(input$filein, {
        filein(shinyFiles::parseFilePaths(volumes, input$filein))
      })
      
      getData <- shiny::reactiveVal()
      shiny::observeEvent(filein(),{
        shiny::req(filein())
        if (nrow(filein()) == 1) {
          getData(read_sql((filein()$datapath)))
        } else {
          return()
        }
      })
      
  } else {
    
    getData <- reactive({
      if(is.null(input$filein)) return(NULL)
      read_sql(input$filein$datapath)
    })
    filein <- shiny::reactiveVal()
    shiny::observeEvent(input$filein, {
      filein(input$filein)
    })
  
  }
  
  output$inputfile <- shiny::renderText({
    paste0("Selected file: ",as.character(filein()$name))
  })

  output$frt <- shiny::renderUI({
    if (!is.null(app_start_dir)){
      shinyFiles::shinyFilesButton("filein",
                                   "Select a file",
                                   title = "Select a harpSpatial sqlite file",
                                   multiple = FALSE,
                                   buttonType = "default",
                                   viewtype = "detail")
    } else {
      shiny::fileInput("filein", "Choose file (sqlite)",
                multiple = FALSE, accept = c(".sqlite"))
    }
  })

  # getData is a list! getData()$verif_data is a tibble and getData()$scores is a vector
  output$fileUploaded <- reactive({
    return(!is.null(getData()))
  })

  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)

  observe({
    shiny::req(getData())
    verif_data <- getData()$verif_data
    scores <- getData()$scores
    update_options(verif_data,scores,session)
  })
    
  ############################################################
  # SHOW DATA                                                #
  ############################################################

  ## needs to check selection on the form and pass them as plotting options to plot_spatial_verif!
  ## if score name changes with selection then read_sql needs to be called again to get the correct dataframe!
  
  figsize <- reactive({
    
    req(input$showdata)
    if (nrow(filein()) == 1) { 
      fw <- 1000
      fh <- 600
      if (isolate(input$score) == "NACT") {
        fw <- 1200
        fh <- 800
        if (length(isolate(input$model)) > 1) {
          fw <- 1600
        }
      } else if (isolate(input$score) == "FSS") {
        if (length(isolate(input$model)) > 2) {
          fw <- 1200
          fh <- 800
        }
      } 
      list("fw" = fw,
           "fh" = fh)
    } else {
      list("fw" = 1000,
           "fh" = 600)
    }
  })

  output$plot <- renderPlot({

    req(input$showdata)
    # This if avoids error when changing selected file
    if (nrow(filein()) == 1) { 
      score <- isolate(input$score)
      nact_score <- isolate(input$nact_score)
      scales <- isolate(input$scales)
      thresholds <- isolate(input$thresholds)
      models <- isolate(input$model)
      ref_model <- isolate(input$ref_model)
      leadtimes <- isolate(input$leadtime)
      cycles    <- isolate(input$cycle)
      fcdate_range <- isolate(input$dates)
  #    thresholds <- isolate(input$threshold) #TODO, coming with plotting options
  #    scales <- isolate(input$scale)         #TODO, coming with plotting options
      params <- isolate(input$param)
      
      fcbdate <- fcdate_range[1]
      fcedate <- fcdate_range[2]
  
      verif_data <- read_sql(filein()$datapath, score)$verif_data
      filter_by <- vars(
        model    %in% models, 
        leadtime %in% leadtimes,
        fcst_cycle %in% cycles,
        as_date(fcdate) >= as_date(fcbdate) & as_date(fcdate) <= as_date(fcedate),
  #      threshold   %in% thresholds,         #TODO, dependent on score 
  #      scale   %in% scales,                 #TODO, dependent on score 
        prm      %in% params,
      )
      #plot_opts = ...                        # TODO, include plotting options to interface
      if ("scale" %in% names(verif_data)) {
        filter_by <- c(filter_by,
                       vars(scale %in% scales))
      }
      if ("threshold" %in% names(verif_data)) {
        filter_by <- c(filter_by,
                       vars(threshold %in% thresholds))
      }

      harpVis:::plot_spatial_verif(verif_data, {{score}}, filter_by = filter_by,
                                   plot_num_cases = input$showcases,
                                   plot_opts = list(ref_model = ref_model,
                                                    nact_scores = nact_score))
    } else {
      return()
    }
    
  })
  
  output$plot.ui <- renderUI({
    plotOutput("plot",width=figsize()$fw,height=figsize()$fh)
  })
  
  output$table <- renderDataTable({

    req(input$showdata)
    if (nrow(filein()) == 1) {
      score <- input$score
      verif_data <- read_sql(filein()$datapath, score)$verif_data
      return(verif_data)
    } else {
      return()
    }
  })

}
