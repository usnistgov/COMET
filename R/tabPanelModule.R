# Overview, Metrics, and Experimental Design module

tp1UI <- function(id) {
  ns <- NS(id)
  tagList(
    h2(textOutput(ns("Title")) ), 
    plotOutput(ns("Data_Plot"), height = "600px"),
    plotOutput(ns("Data_Plot2"), height = "600px")
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


tp2UI <- function(id) {
  ns <- NS(id)
  tagList(
    plotOutput(ns("Metrics_Plot"), height = "600px"),
    fluidRow(plotOutput(ns("Means_Plot")),
             plotOutput(ns("CV_Plot")))
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

tp3UI <- function(id) {
  ns <- NS(id)
  tagList(
    br(),
    h3('Methods and Target Dilution Fractions',align='center'),
    br(),
    DT::dataTableOutput(ns('table1')),
    br(),
    h3('Pipetting Error',align='center'),
    br(),
    DT::dataTableOutput(ns('table2')),
    br(),
    h3('Number of Replicate Samples',align='center'),
    br(),
    DT::dataTableOutput(ns('table3')),
    br(),
    h3('Number of Replicate Observations per Sample',align='center'),
    br(),
    DT::dataTableOutput(ns('table4')),
    br()
  )
}

tp3Server <- function(id, input_file, Metrics){ 
  moduleServer(
    id,
    function(input,output,session) {
      req(Metrics,input_file)
      
      
      output$table1 <- DT::renderDataTable({
        methods = unique(Metrics()$dat$counting_method)
        dfs = unique(Metrics()$dat$target_dilution_fraction)
        outdf = matrix(rep(dfs, length(methods)),nrow=length(dfs) )
        colnames(outdf) = methods
        outdf = as.data.frame(outdf)
        first_col = paste('df',1:length(dfs),sep='')
        first_col = data.frame(df=first_col)
        outdf = cbind(first_col,outdf)
        outdf
      },options=list(searching=FALSE,ordering=FALSE))
      
      
      output$table2 <- DT::renderDataTable({
        
        outdf = Metrics()$dat %>%
          select(counting_method,replicate_sample,rep_obsv,
                 target_dilution_fraction,measured_dilution_fraction)
        
        outdf$measured_dilution_fraction <- round(outdf$measured_dilution_fraction,4)
        
        colnames(outdf) = c('Counting_Method','Sample','Obs','Target_df',
                            'Measured_df')
        outdf
      },options=list(searching=FALSE,ordering=FALSE))
      
      
      output$table3 <- DT::renderDataTable({
        # number replicate samples
        outdf = Metrics()$dat %>% 
          select(counting_method,target_dilution_fraction,replicate_sample) %>%
          group_by(counting_method,target_dilution_fraction) %>%
          summarise(count=length(unique(replicate_sample) )) %>%
          tidyr::pivot_wider(names_from=counting_method,values_from=count)
        
        colnames(outdf)[1]='Target DF'
        outdf
      },options=list(searching=FALSE,ordering=FALSE))
      
      
      output$table4 <- DT::renderDataTable({
        # number replicate obs

        outdf = Metrics()$dat %>% 
          select(counting_method,target_dilution_fraction,replicate_sample) %>%
          group_by(counting_method,target_dilution_fraction,replicate_sample) %>%
          summarise(count=n()) %>%
          tidyr::pivot_wider(names_from=counting_method,values_from=count,
                             values_fill=0)
        
        colnames(outdf)[1]='Target DF'
        outdf
      },options=list(searching=FALSE,ordering=FALSE))
      
    }
    
    
    
    
  )
}



