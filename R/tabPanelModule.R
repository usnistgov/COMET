# Overview, Metrics, and Experimental Design module

tp1UI <- function(id) {
  ns <- NS(id)
  tagList(
    br(),
    plotOutput(ns("raw_data_plot")),
    br(),
    helpText(descriptions[['tp1_raw_data_plot']]),
    br(),
    plotOutput(ns("data_plot")),
    br(),
    fluidRow(column(4),column(2,actionButton(ns('flex_mod'),'Toggle Flexible Model',align='center'))),
    br(),
    helpText(descriptions[['tp1_data_plot']]),
    br(),
    plotOutput(ns("residual_plot")),
    br(),
    helpText(descriptions[['tp1_residual_plot']])

    
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
      
      
      flexible <- reactiveValues(show=FALSE)
      
      observeEvent(input$flex_mod, {
        flexible$show = !flexible$show
      })
      
      
      output$data_plot <- renderPlot({
        if (is.null(input_file())) {
          return(NULL)
        }
        
        dat = Metrics()$dat
        overview.plot = Metrics()$overview.plot 
        df_for_poly = Metrics()$df_for_poly
        
        the_dfs = dat %>%
          select(measured_dilution_fraction,target_dilution_fraction,counting_method,cell_conc) %>%
          group_by(target_dilution_fraction,counting_method) %>%
          summarise(mean_mdf = mean(measured_dilution_fraction),mean_conc=mean(cell_conc)) 
        
        the_dfs$lwr = rep(0,nrow(the_dfs))
        the_dfs$upr = rep(0,nrow(the_dfs))
        
        for(ii in 1:nrow(the_dfs)) {
          sub_df = df_for_poly[df_for_poly$comp_level == the_dfs$counting_method[ii],]
          the_dfs$lwr[ii] = sub_df$lwr[which.min(abs(the_dfs$mean_mdf[ii] - sub_df$x))]
          the_dfs$upr[ii] = sub_df$upr[which.min(abs(the_dfs$mean_mdf[ii] - sub_df$x))]
        }
        
        overview.plot = overview.plot + ylim(0,max(the_dfs$upr))
        
        #names(the_dfs)[which(names(the_dfs) == 'counting_method')] = 'comp_level'
      
        
        if(flexible$show) {
          # add smooth fit and prediction intervals to plot
          overview.plot<-overview.plot+
            geom_errorbar(data=the_dfs,aes(x=mean_mdf,ymin=lwr,ymax=upr),color='gray50',width=.025) 
          
          overview.plot
          
        } else {
          overview.plot 
        }
        
      })
      
      output$raw_data_plot <- renderPlot({
        if(is.null(input_file())) {
          return(NULL)
        }
        
        dat = Metrics()$dat
        
        ggplot(dat, aes(x=measured_dilution_fraction, y=cell_conc, col=factor(stock_solution) )) + 
          geom_point(alpha=.5) +
          facet_wrap(~counting_method) + 
          ggtitle("Raw Data") +
          xlab("Dilution Fraction") +
          ylab("Cell Concentration (cells/mL)") +
          theme_bw() +
          theme(plot.title = element_text(hjust = 0.5)) +
          labs(col='Stock Solution')
        
      })
      
      output$residual_plot <- renderPlot({
        if(is.null(input_file())) {
          return(NULL)
        }
        print(Metrics()$residual.plot)
      })
      
    }
  )
}


tp2UI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("R Squared"),
    plotOutput(ns("Metrics_Plot_r2"), height = "400px"),
    p('The plot above gives the R-squared value of each flexible model',
      'for each counting method. The vertical bars represent bootstrap confidence',
      'intervals computed at the requested confidence level.'),
    br(),
    h3("Proportionality Indices"),
    plotOutput(ns("Metrics_Plot"), height = "600px"),
    p('The plot above gives the PI value of each flexible model',
      'for each counting method. The vertical bars represent bootstrap confidence',
      'intervals computed at the requested confidence level.'),
    br(),
    h3("Mean Concentration vs. Dilution Fraction"),
    plotOutput(ns("Means_Plot")),
    p(paste("In the plot above, mean cell count is given",
            "for each method, at each dilution fraction.",
            "The vertical bars represent the mean plus/minus 1 standard error",
            "of the mean (standard deviation / sqrt(# replicate samples).")),
    br(),
    h3("Coefficient of Variation (CV) at each Dilution Fraction"),
    plotOutput(ns("CV_Plot")),
    p(paste("In the plot above, mean percent CV is computed",
            "for each method, at each dilution fraction.",
            "The vertical bars represent the mean percent CV",
            "plus minus the standard error of the mean CV.")),
    br(),
    h3("Proportionality Constants"),
    plotOutput(ns('prop_const_plot')),
    p(paste("The above plot shows the proportionality (slope) estimates for",
            "each counting method, along with bootstrap confidence intervals.",
            "Non-overlapping intervals suggests that the true proportionality",
            "constants are different for the counting methods. Note that this",
            "difference does not suggest one method is more accurate than",
            "the other, only a difference in their proportional fits.")),
    br()
  )
}


tp2Server <- function(id, input_file, Metrics) {
  moduleServer(
    id,
    function(input, output, session) {
      
      output$Metrics_Plot_r2 <- renderPlot({
        if (is.null(input_file() )) {
          return(NULL)
        }
        
        print(Metrics()$metrics.plot.r2)
      })

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
      
      output$prop_const_plot <- renderPlot({
        outdf <- Metrics()$metrics %>%
          dplyr::select(counting_method,cell_type,Metric,Value,lower,upper) %>%
          dplyr::filter(Metric == 'Prop.Const.x') %>%
          dplyr::select(-Metric)
        
        for(var in c('Value','lower','upper')) {
          outdf[,var] <- sapply(outdf[,var],round_or_truncate,3)
        }
        
        colnames(outdf) = c('countingMethod','cellType','propConst','lowerCL','upperCL')
        
        
        ggplot(outdf, aes_string(x='countingMethod',y='propConst')) + 
          geom_point() +
          geom_errorbar(aes_string(ymin='lowerCL',ymax='upperCL'),width=.2) +
          #ggtitle("Proportionality Constants") +
          xlab("Method") +
          ylab("Proportionality Constant") +
          theme(plot.title = element_text(hjust = 0.5,size=20))
        
        
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



