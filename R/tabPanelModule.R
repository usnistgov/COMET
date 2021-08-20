# Overview, Metrics, and Experimental Design module

tp1UI <- function(id) {
  ns <- NS(id)
  tagList(
    br(),
    br(),
    plotOutput(ns("raw_data_plot")),
    br(),
    helpText(descriptions[['tp1_raw_data_plot']]),
    br(),
    plotOutput(ns("data_plot")),
    br(),
    fluidRow(column(4),column(2,actionButton(ns('flex_mod'),'Show Prediction Intervals',align='center'))),
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
        the_dfs$fit = rep(0,nrow(the_dfs))
        
        for(ii in 1:nrow(the_dfs)) {
          sub_df = df_for_poly[df_for_poly$comp_level == the_dfs$counting_method[ii],]
          the_dfs$lwr[ii] = sub_df$lwr[which.min(abs(the_dfs$mean_mdf[ii] - sub_df$x))]
          the_dfs$upr[ii] = sub_df$upr[which.min(abs(the_dfs$mean_mdf[ii] - sub_df$x))]
          the_dfs$fit[ii] = sub_df$y[which.min(abs(the_dfs$mean_mdf[ii] - sub_df$x))]
        }
        
        overview.plot = overview.plot + ylim(0,max(the_dfs$upr))
        
        
        if(Metrics()$log_scale) {
          
          slope_vals = Metrics()$metrics %>%
            filter(Metric == 'Prop.Const.x') %>% 
            select(counting_method,Value)
          
          cms = unique(slope_vals$counting_method)
          
          line_data = data.frame(counting_method = rep(cms,each=100),
                                 x=0)
          
          
          for(ii in 1:length(cms)) {
            
            minval = min(dat$target_dilution_fraction[dat$counting_method == cms[ii]])
            maxval = max(dat$target_dilution_fraction[dat$counting_method == cms[ii]])
            
            xvals = exp(seq(log(minval),log(maxval),length.out = 100))
            
            t_slope = slope_vals$Value[slope_vals$counting_method == cms[ii]]
            
            line_data$x[line_data$counting_method == cms[ii]] = xvals
            line_data$y[line_data$counting_method == cms[ii]] = xvals*t_slope
            
          }
          
          overview.plot = overview.plot +
            scale_x_continuous(trans='log10') +
            scale_y_continuous(trans='log10') + 
            geom_line(data=line_data,aes(x=x,y=y),color='black')
        }
        
      
        
        if(flexible$show) {
          # add smooth fit and prediction intervals to plot
          
          overview.plot<-overview.plot+
            geom_errorbar(data=the_dfs,aes(x=mean_mdf,ymin=lwr,ymax=upr),color='gray50',width=0) 
          
          return(overview.plot)
          
        } else {
          return(overview.plot)
        }
        
      })
      
      output$raw_data_plot <- renderPlot({
        if(is.null(input_file())) {
          return(NULL)
        }
        
        dat = Metrics()$dat
        
        p = ggplot(dat, aes(x=measured_dilution_fraction, y=cell_conc, col=counting_method )) + 
          geom_point(alpha=.5) +
          facet_wrap(~counting_method) + 
          ggtitle("Raw Data") +
          xlab("Dilution Fraction") +
          ylab("Cell Concentration (cells/mL)") +
          theme_bw() +
          theme(plot.title = element_text(hjust = 0.5)) +
          labs(col='Counting Method')
        
        if(Metrics()$log_scale) {
          p = p + 
            scale_x_continuous(trans='log10') +
            scale_y_continuous(trans='log10')
        }
        
        return(p)
        
      })
      
      output$residual_plot <- renderPlot({
        if(is.null(input_file())) {
          return(NULL)
        }
        
        p = Metrics()$residual.plot
        
        if(Metrics()$log_scale) {
          p = p + 
            scale_x_continuous(trans='log10')
        }
        
        p
        
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
    p('The plot above gives the PI value',
      'for each counting method, using the desired flexible model.',
      'The vertical bars represent bootstrap confidence',
      'intervals computed at the requested confidence level.'),
    br(),
    h3("Mean Concentration vs. Dilution Fraction"),
    plotOutput(ns("Means_Plot")),
    p(paste("In the plot above, mean cell count is given",
            "for each method, at each dilution fraction.",
            "The vertical bars represent the mean plus/minus 1 standard error",
            "of the mean (standard deviation / sqrt(# replicate samples)).")),
    br(),
    h3("Coefficient of Variation (CV) at each Dilution Fraction"),
    plotOutput(ns("CV_Plot")),
    p(paste("In the plot above, mean percent CV is computed",
            "for each method, at each dilution fraction.",
            "The vertical bars represent the mean percent CV",
            "plus minus the standard error of the mean CV",
            "(standard deviation / sqrt(# replicate samples)).")),
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
        
        p = Metrics()$means_plot
        
          
        p
      })
      
      output$CV_Plot <- renderPlot({
        if (is.null(input_file() )) {
          return(NULL)
        }
        
        p = Metrics()$cv_plot
        
        p
          
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
    br(),
    downloadButton(ns("downloadData"),"Download All Tables")
  )
}

tp3Server <- function(id, input_file, Metrics){ 
  moduleServer(
    id,
    function(input,output,session) {
      
      
      table1 = reactive({
        
        req(Metrics,input_file)
        methods = unique(Metrics()$dat$counting_method)
        dfs = unique(Metrics()$dat$target_dilution_fraction)
        outdf = matrix(rep(dfs, length(methods)),nrow=length(dfs) )
        colnames(outdf) = methods
        outdf = as.data.frame(outdf)
        first_col = paste('df',1:length(dfs),sep='')
        first_col = data.frame(df=first_col)
        outdf = cbind(first_col,outdf)
        outdf
        
      })
      
      output$table1 <- DT::renderDataTable({
        
        return(table1())

      },options=list(searching=FALSE,ordering=FALSE))
      
      
      table2 = reactive({
        
        req(Metrics,input_file)
        
        outdf = Metrics()$dat %>%
          select(counting_method,replicate_sample,rep_obsv,
                 target_dilution_fraction,measured_dilution_fraction)
        
        outdf$measured_dilution_fraction <- round(outdf$measured_dilution_fraction,4)
        
        colnames(outdf) = c('Counting_Method','Sample','Obs','Target_df',
                            'Measured_df')
        return(outdf)
        
      })
      
      output$table2 <- DT::renderDataTable({
        
        return(table2())
        
      },options=list(searching=FALSE,ordering=FALSE))
      
      
      table3 = reactive({
        
        # number replicate samples
        outdf = Metrics()$dat %>% 
          select(counting_method,target_dilution_fraction,replicate_sample) %>%
          group_by(counting_method,target_dilution_fraction) %>%
          summarise(count=length(unique(replicate_sample) )) %>%
          tidyr::pivot_wider(names_from=counting_method,values_from=count)
        
        colnames(outdf)[1]='Target DF'
        
        return(outdf)
        
      })
      
      output$table3 <- DT::renderDataTable({
        
        return(table3())
        
      },options=list(searching=FALSE,ordering=FALSE))
      
      
      table4 = reactive({
        
        # number replicate obs
        
        outdf = Metrics()$dat %>% 
          select(counting_method,target_dilution_fraction,replicate_sample) %>%
          group_by(counting_method,target_dilution_fraction,replicate_sample) %>%
          summarise(count=n()) %>%
          tidyr::pivot_wider(names_from=counting_method,values_from=count,
                             values_fill=0)
        
        colnames(outdf)[1]='Target DF'
        
        return(outdf)
        
        
      })
      
      output$table4 <- DT::renderDataTable({
        
        return(table4())
        
      },options=list(searching=FALSE,ordering=FALSE))
      
      
      output$downloadData = downloadHandler(
        filename = function() {
          return("Experimental_Design.csv")
        },
        
        content = function(file) {
          
          cat("Methods and Target DFs \n",file=file)
          write.table(table1(),file=file,append=TRUE,row.names = FALSE,sep=',')
          cat("\n",file=file,append=TRUE)
          
          cat("Pipetting Error \n",file=file,append=TRUE)
          write.table(table2(),file=file,append=TRUE,row.names = FALSE,sep=',')
          cat("\n",file=file,append=TRUE)
          
          cat("Number of Replicate Samples \n",file=file,append=TRUE)
          write.table(table3(),file=file,append=TRUE,row.names = FALSE,sep=',')
          cat("\n",file=file,append=TRUE)
          
          cat("Number of Replicate Observations per Sample \n",file=file,append=TRUE)
          write.table(table4(),file=file,append=TRUE,row.names = FALSE,sep=',')
          
        }
      )
      
    }
    
    
    
    
  )
}



