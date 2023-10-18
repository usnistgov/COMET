library(ggplot2)
library(dplyr)
library(readr)
library(shiny)
library(tidyr)
library(shinythemes)
library(shinycssloaders)

source('R/utils.R')
source('R/plot_utils.R')
source('R/CalcMetricsModule.R')
source('R/downloadModule.R')
source('R/tabPanelModule.R')
source('R/tabPanelModule2.R')
source('R/templateModule.R')
source('other/descriptions.R')

options(dplyr.summarise.inform = FALSE)

ui<-shinyUI(
  
  fluidPage(id='fullpage',#theme=shinytheme('spacelab'),

  tags$link(rel="stylesheet",href='my_style.css'),
  tags$link(rel='stylesheet',href='nist_style.css'),
  tags$head(tags$link(rel="shortcut icon", href="favicon.ico")),
  tags$head(HTML("<title>COMET: Counting Method Evaluation Tool</title>")),
  
  HTML(
    '<head>
      <!-- Google tag (gtag.js) -->
      <script async src="https://www.googletagmanager.com/gtag/js?id=G-WBWKF12V6X"></script>
      <script>
        window.dataLayer = window.dataLayer || [];
        function gtag(){dataLayer.push(arguments);}
        gtag("js", new Date());
        gtag("config", "G-WBWKF12V6X");
      </script>
     </head>'
  ),
  
  tags$head(
    tags$style(HTML("
      .shiny-output-error-validation {
        color: #C0392B;
        font-weight: bold;
      }
    "))
  ),
  
  tags$div(HTML(nist_header_html)),
  
  HTML(
    "<h1>&nbsp COMET: Counting Method Evaluation Tool</h1>"
  ),
  
  br(),
  
  fluidRow(
    column(width=3,
    sidebarPanel(width=12,
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
      HTML("
      <p>The source code for the COMET application can be found <a href='https://github.com/usnistgov/COMET'>here</a>.<p>
           ")
    )), # end sidebar

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


    ), # end fluidRow
  
  tags$div(HTML(nist_footer_html))
  
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