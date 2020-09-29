tp1UI <- function(id) {
  ns <- NS(id)
  tagList(
    h2(textOutput(ns("Title")) ), 
    plotOutput(ns("Data_Plot"), height = "600px"),
    plotOutput(ns("Data_Plot2"), height = "600px")
  )
}

tp2UI <- function(id) {
  ns <- NS(id)
  tagList(
    plotOutput(ns("Metrics_Plot"), height = "600px"),
    fluidRow(plotOutput(ns("Means_Plot")),
             plotOutput(ns("CV_Plot")))
  )
}

tp1Server <- function(id, input_file, Metrics) {
  moduleServer(
    id,
    function(input, output, session) {
      
      output$Title <- renderText({
        if (is.null(input_file())) {
          return("Please select input file for analysis")
        }
  
        print(Metrics()$Title)

      }) 
      
      output$Data_Plot <- renderPlot({
        if (is.null(input_file())) {
          return(NULL)
        }
        print(Metrics()$overview.plot)
      })
      
      output$Data_Plot2 <- renderPlot({
        if (is.null(input_file() )) {
          return(NULL)
        }
        print(Metrics()$overview.plot2)
      })
    }
  )
}



tp2Server <- function(id, input_file, Metrics) {
  moduleServer(
    id,
    function(input, output, session) {

      output$Metrics_Plot <- renderPlot({
        if (is.null(input_file() )) {
          return(NULL)
        }
          
        print(Metrics()$metrics.plot)
      })
      
      output$Means_Plot <- renderPlot({
        if(is.null(input_file() )) {
          return(NULL)
        }
          
        print(Metrics()$means_plot)
      })
      
      output$CV_Plot <- renderPlot({
        if (is.null(input_file() )) {
          return(NULL)
        }
          
        print(Metrics()$cv_plot)
      })
      
      
    }
  )
}