# server for shiny_plot_spatial_verif

read_sql <- function(filepath,score=NULL){
    #this function might need a more appropriate name
    print("THIS IS READING FUNCTION CALL")
    sql_object <- harpVis:::dbopen(gsub("\\\\","/",filepath))
    scores <- dbListTables(sql_object)
    if(is.null(score)) {score=scores[1]}
    verif_data <- as.data.frame(harpVis:::dbquery(sql_object, paste("SELECT * FROM ",score))) #can choose first score as default
    harpVis:::dbclose(sql_object)
    items <- list("verif_data" = verif_data, "scores" = scores)
    return(items) #returns a list of dataframe and list of scores
}

update_options <- function(input,scores,session) {
    #this function might need a more appropriate name
    print("THIS IS OPTIONS FUNCTION CALL")
    dates <- unique(lubridate::as_datetime(input$fcdate,
                                           origin = lubridate::origin,
                                           tz = "UTC"))
    models <- unique(input$model)
    params <- unique(input$prm)
    leadtimes <- unique(input$leadtime)/3600
    updateSelectInput(session,'score',      choices=c(scores),
                                                selected=c(scores)[1])
    updateDateRangeInput(session,'dates',   start=dates[1],
                                            end=tail(dates,n=1),
                                            min=min(dates),
                                            max=max(dates))
    updateSelectInput(session,'model',      choices=c(models),
                                                selected=c(models)[1])
    updateSelectInput(session,'leadtime',   choices=c(leadtimes),
                                                selected = c(leadtimes)[1])
    updateSelectInput(session,'param',      choices=c(params),
                                                selected=c(params)[1])

}

server <- function(input, output, session) {


  ############################################################
  # LOAD DATA                                                #
  ############################################################
  getData <- reactive({
    if(is.null(input$filein)) return(NULL)
    read_sql(input$filein$datapath)
  })

  # getData is a list! getData()$verif_data is a tibble and getData()$scores is a vector
  output$fileUploaded <- reactive({
    print("THIS IS SERVER REACTIVE CALL")
    return(!is.null(getData()))
  })

  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)

  observe({
    req(input$filein)
    verif_data <- getData()$verif_data
    scores <- getData()$scores
    update_options(verif_data,scores,session)
  })
    
  ############################################################
  # SHOW DATA                                                #
  ############################################################

  ## needs to check selection on the form and pass them as plotting options to plot_spatial_verif!
  ## if score name changes with selection then read_sql needs to be called again to get the correct dataframe!

  output$plot <- renderPlot({

    req(input$showdata)
    
    score <- input$score
    verif_data <- read_sql(input$filein$datapath, score)$verif_data
    #filter_by <- vars(fcdates == input$dates, model == input$model, leadtime == input$leadtime) #this seems to break plotting at the moment
    #plot_opts = ...

    harpVis:::plot_spatial_verif(verif_data, score, show_info=TRUE)
    #harpVis:::plot_spatial_verif(verif_data, score, filter_by = filter_by)

  },width = 1000, height = 600)

  output$table <- renderDataTable({

    req(input$showdata)
    score <- input$score
    verif_data <- read_sql(input$filein$datapath, score)$verif_data
    return(verif_data)
  })

}
