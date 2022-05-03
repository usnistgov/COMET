library(tidyverse)
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
      p("Welcome to the Counting Method Evalution Tool.",
        "Begin by uploading your dataset in the proper format,",
        "and then selecting the desired options for analysis.",
        "We have also provided a readme, app instrutions, blank template files,",
        "as well as template examples for download, if desired."),
      br(),
      br(),
      templateUI('template_files'),
      fileInput('file1', 'Upload .csv (full template) or .xlsx (simple template) file for analysis.', accept=c('.csv','.xlsx')),
      
      metricsUI('metrics'),
      br(),
      br(),
      downloadUI('download'),

      hr(),
      hr(),
      hr(),
      h5("Technical Consultants/Contacts:"),
      tags$ul(
        tags$li("Sumona Sarkar (sumona.sarkar@nist.gov)"), 
        tags$li("Laura Pierce (laura.pierce@nist.gov)")
      ),
      h5("Software Authors/Contacts:"),
      tags$ul(
        tags$li("David Newton (david.newton@nist.gov)"),
        tags$li("Steve Lund")
      ),
      br(),
      p("Disclaimer: This application is hosted using Shinyapps.io (which uses AWS).",
        "Shinyapps.io runs applications in their own protected environments",
        "with encrypted SSL connection. The application is currently pending",
        "NIST security approval."),
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
  templateServer('template_files')
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