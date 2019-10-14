library(shiny)
# global.R hasn't been read yet, so you need it here:
source("settings/set_parameters.R")
# TODO: lead time!
shinyUI(
  fluidPage(
    tags$head(tags$script(src = "message-handler.js")),
    headerPanel("HARP spatial verification"),
    sidebarLayout(
        sidebarPanel(
# SQLITE FILE
         fluidRow(
           column(10,textInput("sqlfile","SQLite file:", value=NULL,
                  placeholder="/path/to/file.sqlite")),
           column(1,actionButton("SQLbutton","OK"))
         ),
        tags$style(type='text/css', "#SQLbutton { margin-top: 25px;}"),
# DATE SETTINGS:
         fluidRow( 
            column(4, selectInput("vPeriod","Period",
                      choices = c("Monthly"="month","Season"="season","Recent"="recent","Custom"="custom"),
                      multiple=FALSE,selected="custom")),
            column(5, selectInput("fct","Run time:",choices=c(0,6,12,18),multiple=TRUE,selected=0 ))),
         conditionalPanel( condition="input.vPeriod=='month' || input.vPeriod=='season' ", fluidRow(
           column(4, selectInput("vYear","Year:",choices=yearMIN:yearMAX,multiple=FALSE,selected=yearMAX)),
           conditionalPanel(condition="input.vPeriod=='month'",
             column(4, selectInput("vMonth","Month:",choices=1:12,multiple=FALSE,selected=1)) ),
           conditionalPanel( condition="input.vPeriod=='season'",
             column(6, selectInput("vSeason","Season:",
                           choices=c("Winter (DJF)"="winter","Spring (MAM)"="spring",
                                    "Summer (JJA)"="summer","Autumn (SON)"="autumn"),
                           multiple=FALSE,selected="winter")) ))),
                     ## the "custom" date must have limits adapted to the data
                     ## so it's 
         conditionalPanel( condition="input.vPeriod=='custom'",
            uiOutput("UI_daterange")
#            dateRangeInput( "vDateRange","Date range:", format="yyyy-mm-dd",
#                            min=dateMIN,max=dateMAX,start="2016-01-01",end="2016-01-31")
         ),
# PARAMETER & MODEL SELECTION
         fluidRow(
           column(4,uiOutput("UI_prmlist")),
           column(4,uiOutput("UI_ldtlist")),
           column(4,uiOutput("UI_modellist"))
         ),
# SCORE dependent
         conditionalPanel( condition="input.tab1 == 'fuzzy'",
           fluidRow(
             column(6, selectInput("fuzzyScore","Score",choices=scoreList_fuzzy)),
             column(6, selectInput("fuzzyStyle","Style",choices=styleList_fuzzy))
           )
         ),
#         conditionalPanel( condition="input.tab1 == 'SAL'",
#           fluidRow(
#             column(6, selectInput("salStyle","Style",choices=styleList_sal))
#           )
#         ),
         conditionalPanel( condition="input.tab1 == 'basic'",
           fluidRow(
             column(6, selectInput("basicScore","Score",choices=scoreList_basic)),
             column(6, selectInput("basicStyle","Score",choices=styleList_basic))
           )
         )
       ), # sidebarPanel
        mainPanel(
          tabsetPanel(
#            tabPanel("basic", plotOutput("plotBasic")),
            tabPanel("fuzzy", plotOutput("plotFuzzy")),
### unfortunately, the following doesn't work at all:
#              conditionalPanel( condition="input.fuzzyStyle == 'table' && input.tab1 == 'fuzzy'",
#                                dataTableOutput("DataTable")),
#              conditionalPanel( condition="input.fuzzyStyle == 'color' && input.tab1 == 'fuzzy'",
#                                plotOutput("plotFuzzy"))),
            tabPanel("SAL", plotOutput("plotSAL")),
#              conditionalPanel( condition="input.salStyle == 'table' && input.tab1 == 'SAL'",
#                                dataTableOutput("DataTable")),
#              conditionalPanel( condition="input.salStyle == 'plot' && input.tab1 == 'SAL'",
#                                plotOutput("plotSAL"))),
            tabPanel("fuzzyData",dataTableOutput("DataTable")),
            id="tab1",selected="SAL"
          )
        ) # mainPanel
     ) #sidebarLayout
  ) #fluidPage
) # shinyUI

