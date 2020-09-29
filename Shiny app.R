library(tidyr)
library(plyr)
library(dplyr)
library(ggplot2)
library(shiny)

source('R/utils.R')
source('R/calc_metrics.R')
source('R/CalcMetricsModule.R')
source('R/downloadModule.R')
source('R/tabPanelModule.R')
source('R/customHTML.R')

ui<-shinyUI(fluidPage(

  
  tags$head(
     customHTML_head()
   ),
  
  fluidRow(
  
    sidebarPanel(
      fileInput('file1', 'Choose CSV File',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
      
      metricsUI('metrics'),
      downloadUI('download'),
      
      hr(),
      hr(),
      hr(),
      #h6("Authored by Steve Lund"),
      h6("Statistical Engineering Division"),
      h6("Information Technology Laboratory"),
      h6("National Institute of Standards and Technology")
    ),
    
    mainPanel( tabsetPanel(
      tabPanel("Data Overview",
               tp1UI('tp1')
               ),
      
      tabPanel("Metrics",
               tp2UI('tp2')))
    ),
  

    ), # end fluidRow
  
  fluidRow(
    customHTML_foot()
  )

))

server<-function(input, output, session) {
  
  input_file <- reactive(input$file1)
  Metrics <- metricsServer('metrics',input$file1)
  
  downloadServer('download',input$file1, Metrics)

  tp1Server('tp1', input_file, Metrics) #putting reactive around these works
  tp2Server('tp2', input_file, Metrics)

}

shinyApp(ui = ui, server = server)