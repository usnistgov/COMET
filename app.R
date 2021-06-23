library(tidyr)
library(plyr)
library(dplyr)
library(ggplot2)
library(shiny)
library(shinythemes)

source('R/utils.R')
source('R/calc_metrics.R')
source('R/CalcMetricsModule.R')
source('R/downloadModule.R')
source('R/tabPanelModule.R')
source('R/tabPanelModule2.R')
source('R/templateModule.R')
source('other/descriptions.R')

options(dplyr.summarise.inform = FALSE)

ui<-shinyUI(fluidPage(theme=shinytheme('spacelab'),

  br(),
  
  tags$h1(
    "COMET: Counting Method Evaluation Tool"
  ),
  
  br(),
  
  fluidRow(
  
    sidebarPanel(
      fileInput('file1', 'Choose .csv File', accept='.csv'),
      
      #templateUI('template_files'),
      
      metricsUI('metrics'),
      br(),
      br(),
      downloadUI('download'),

      hr(),
      hr(),
      hr(),
      h6(paste("Code:", sample(c("Steve Lund and David Newton","David Newton and Steve Lund"),1))),
      h6("Technical Contacts: Sumona Sarkar and Laura Pierce"),
      h6("National Institute of Standards and Technology"),
      hr(),
      h6("Contact david.newton@nist.gov regarding any bugs.")
    ), # end sidebar

    mainPanel( tabsetPanel(
      tabPanel("Data Overview",
               tp1UI('tp1')
               ),

      tabPanel("Quality Indicators",
                tp2UI('tp2')),

      tabPanel("Metrics Tables",
              tp5UI('tp5')),


      tabPanel("Experimental Design",
              tp3UI('tp3')),

      tabPanel("Stat Analysis",
               tp6UI('tp6')),

      tabPanel("Compare Methods",
               tp7UI('tp7')),

      tabPanel("Experiment Integrity",
               tpDilutionUI('tpdil')),

      tabPanel("Viability Analysis",
               tpViabilityUI('tpvia'))

    ) #end main panel
    ), # end fluidRow


    ) # end fluidRow
  

))

server<-function(input, output, session) {
  
  input_file <- reactive(input$file1)
  Metrics <- metricsServer('metrics', input_file)
  downloadServer('download', input_file, Metrics)
  #templateServer('template_files')
  tp1Server('tp1', input_file, Metrics)
  tp2Server('tp2', input_file, Metrics)
  tp3Server('tp3', input_file, Metrics)
  tp5Server('tp5', input_file, Metrics)
  tp6Server('tp6', input_file, Metrics)
  tp7Server('tp7', Metrics)
  tpDilutionServer('tpdil', Metrics)
  tpViabilityServer('tpvia', Metrics)

}

shinyApp(ui = ui, server = server)