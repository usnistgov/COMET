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
source('R/tabPanelModule2.R')
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
      br(),
      br(),
      downloadUI('download'),
      
      hr(),
      hr(),
      hr(),
      h6("Authored by Steve Lund and David Newton"),
      h6("Statistical Engineering Division"),
      h6("Information Technology Laboratory"),
      h6("National Institute of Standards and Technology")
    ),
    
    mainPanel( tabsetPanel(
      tabPanel("Data Overview",
               tp1UI('tp1')
               ),
      
      tabPanel("Metrics Plots",
               tp2UI('tp2')),
      
      tabPanel("Metrics Tables",
               tp5UI('tp5')),
      
      tabPanel("Experimental Design",
               tp3UI('tp3')),
      
      tabPanel("Stat Analysis",
               tp6UI('tp6')),
      
      tabPanel("Compare Methods",
               tp7UI('tp7'))
      
    )),
  

    ), # end fluidRow
  
  fluidRow(
    customHTML_foot()
  )

))

server<-function(input, output, session) {
  
  input_file <- reactive(input$file1)
  Metrics <- metricsServer('metrics', input_file)
  downloadServer('download', input_file, Metrics)

  tp1Server('tp1', input_file, Metrics) 
  tp2Server('tp2', input_file, Metrics)
  tp3Server('tp3', input_file, Metrics)
  tp5Server('tp5', input_file, Metrics)
  tp6Server('tp6', input_file, Metrics)
  tp7Server('tp7', Metrics)
  
  

}

shinyApp(ui = ui, server = server)