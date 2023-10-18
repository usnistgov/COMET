# Metrics Table and Statistical Analysis Modules (Tabs)

tp5UI <- function(id) {
  ns <- NS(id)
  tagList(
    withMathJax(),
    br(),
    span(textOutput(ns("insufficient_design")),style='color:Tomato'),
    br(),
    h3('Mean cell concentration for each dilution fraction',align='center'),
    br(),
    DT::dataTableOutput(ns('table1')),
    br(),
    h3('Mean %CV for each dilution fraction',align='center'),
    br(),
    DT::dataTableOutput(ns('table2')),
    br(),
    h3('\\( R^2 \\) for proportional model fit',align='center'),
    br(),
    DT::dataTableOutput(ns('table3')),
    br(),
    h3('Proportionality Indices for model fit',align='center'),
    br(),
    DT::dataTableOutput(ns('table4')),
    br(),
    #h3('Other Diagnostic Metrics',align='center'),
    br(),
    #DT::dataTableOutput(ns('table5')), # plots non 'smoothed' metrics
    br(),
    downloadButton(ns('downloadData'),"Download All Tables"),
    br(),
    br()
  )
}

tp5Server <- function(id, input_file, Metrics){ 
  moduleServer(
    id,
    function(input,output,session) {
      
      output$insufficient_design <- renderText({
        
        if(!is.null(Metrics()$exp_des_flag) && Metrics()$exp_des_flag) {
          return(descriptions$design_disclaimer)
          
        } else{
          return(NULL)
        }
        
      })
      
      table1 = reactive({
        
        out_df <- Metrics()$metrics %>% 
          dplyr::select(counting_method, Metric, Value, lower, upper) %>%
          dplyr::filter(grepl('mean_conc',Metric))
        
        out_df$Metric <- gsub('mean_conc_','',out_df$Metric)
        
        for(var in c('Value','lower','upper')) {
          out_df[,var] = round(out_df[,var],0)
        }
        
        
        #means = Metrics()$means %>%
        #  arrange(target_dilution_fraction,counting_method)
        
        out_df = out_df %>%
          arrange(Metric,counting_method)
        
        #out_df$se = means$std_err_mn
        
        out_df = as.data.frame(out_df)
        
        colnames(out_df) <- c('Counting Method','Dilution Fraction',
                              'Mean Concentration','Bootstrap Lower CL','Bootstrap Upper CL')
        
        #out_df$`Std Err` = signif(out_df$`Std Err`,5)
        
        out_df
        
      })
      
      output$table1 <- DT::renderDataTable({
        
        table1()
        
      },options=list(searching=FALSE))
      
      
      table2 = reactive({
        
        out_df <- Metrics()$metrics %>% 
          dplyr::select(counting_method, Metric, Value, lower, upper) %>%
          dplyr::filter(grepl('mean_CV',Metric))
        
        out_df$Metric <- gsub('mean_CV_','',out_df$Metric)
        
        for(var in c('Value','lower','upper')) {
          out_df[,var] = round(out_df[,var],3)
        }
        
        colnames(out_df) <- c('Counting Method','Dilution Fraction','Mean %CV','Bootstrap Lower CL','Bootstrap Upper CL')
        
        out_df
        
      })
      
      output$table2 <- DT::renderDataTable({
        
        table2()
        
      },options=list(searching=FALSE))
      
      
      table3 = reactive({
        
        out_df <- Metrics()$metrics %>% 
          as.data.frame() %>%
          dplyr::select(counting_method, Metric, Value, lower, upper) %>%
          dplyr::filter(Metric == 'R.squared') %>% 
          dplyr::select(-Metric)
        
        for(var in c('Value','lower','upper')) {
          out_df[,var] <- sapply(out_df[,var],round,3)
        }
        
        colnames(out_df) <- c('Counting Method','R squared', 'Lower CL','Upper CL')
        
        out_df
        
      })
      
      output$table3 <- DT::renderDataTable({
        
        table3()
        
      },options=list(searching=FALSE))
      
      
      table4 = reactive({
        
        out_df <- Metrics()$metrics %>% 
          as.data.frame() %>%
          dplyr::select(counting_method, Metric, Value, lower, upper) %>%
          dplyr::filter(Metric %in% c(metrics_key_value(as.numeric(Metrics()$perf_metrics)))) %>%
          dplyr::filter(grepl('smooth',Metric,ignore.case = TRUE))
        
        colnames(out_df) <- c('Counting Method','Type of PI', 'PI', 'Bootstrap Lower CL','Bootstrap Upper CL')
        
        for(var in c('PI','Bootstrap Lower CL','Bootstrap Upper CL')) {
          out_df[,var] <- sapply(out_df[,var],round_or_truncate,3)
        }
        
        out_df
        
      })
      
      output$table4 <- DT::renderDataTable({
        
        table4()
        
      },options=list(searching=FALSE))
      
      output$downloadData = downloadHandler(
        filename = function() {
          return("Metrics_Tables.csv")
        },
        
        content = function(file) {
          
          if(Metrics()$exp_des_flag) {
            cat(paste(descriptions$design_disclaimer,'\n\n'),file=file)
          }
          
          cat("Mean cell concentration for each dilution fraction \n",file=file,append=TRUE)
          write.table(table1(),file=file,append=TRUE,row.names = FALSE,sep=',')
          cat("\n",file=file,append=TRUE)
          
          cat("Mean %CV for each dilution fraction \n",file=file,append=TRUE)
          write.table(table2(),file=file,append=TRUE,row.names = FALSE,sep=',')
          cat("\n",file=file,append=TRUE)
          
          cat("R squared for proportional model fit \n",file=file,append=TRUE)
          write.table(table3(),file=file,append=TRUE,row.names = FALSE,sep=',')
          cat("\n",file=file,append=TRUE)
          
          cat("Proportionality Indices for model fit \n",file=file,append=TRUE)
          write.table(table4(),file=file,append=TRUE,row.names = FALSE,sep=',')
          
        }
      )
     
      
    })
}


tp6UI <- function(id) {
  ns <- NS(id)
  tagList(
    withMathJax(),
    br(),
    span(textOutput(ns("insufficient_design")),style='color:Tomato'),
    br(),
    h3("Statistical Modeling Details",align='center'),
    br(),
    p('In the following equations, the subscript \\( i \\) represents
      the index for the target dilution fraction, and \\( j \\) represents 
      the sample index nested within the \\( i^{th} \\) target dilution fraction.
      (Note that the measured dilution fraction \\( \\text{DF}^{measured}_{ij} \\)
      may be different than the target dilution fraction \\( \\text{DF}^{target}_{i} \\) ).
      The observed cell counts are denoted as \\( Y_{ij} \\).
      \\( \\lambda_{ij} := \\mathbb{E} (Y_{ij}) \\) represent the true cell 
      concentrations for a particular \\( (i,j). \\), and 
      \\( \\sigma^2_{ij} := \\text{Var} (Y_{ij}) \\) indicate the variance of the 
      observed count at \\( (i,j) \\)'),
    br(),
    h4('Proportional Model Assumption:
        $$\\lambda_{ij} = \\beta_1 \\text{DF}^{measured}_{ij}$$'),
    br(),
    h4(textOutput(ns('var_assumption'))),
    br(),
    h4('Estimates for proportionality constant \\( \\beta_1 \\)'),
    br(),
    DT::dataTableOutput(ns('prop_const')),
    br(),
    h4('Calculating PI'),
    p('For all PI formulas, we define \\( e^{(s)}_{ij} = \\hat{Y}_{ij}^{polynomial} - \\hat{Y}_{ij}^{proportional} \\).'),
    uiOutput(ns('pi_formulas')),
    br(),
    h4('Smoothing Approach'),
    br(),
    uiOutput(ns('smoothing_approach')),
    p('The above equation represents the form of the fitted flexible model.'),
    br(),
    h4(textOutput(ns('n_boot'))),
    
  )
}

tp6Server <- function(id, input_file, Metrics) {
  moduleServer(
    id,
    function(input,output,session) {
      
      output$insufficient_design <- renderText({
        
        if(!is.null(Metrics()$exp_des_flag) && Metrics()$exp_des_flag) {
          return(descriptions$design_disclaimer)
          
        } else{
          return(NULL)
        }
        
      })
      
      
      output$n_boot <- renderText({
        paste('Number of bootstrap iterations conducted:',Metrics()$n_boot)
      })
      
      
      output$smoothing_approach <- renderUI({
        
        degree = Metrics()$smooth_df
        
        slash_str = "\\"
        
        outstr = paste0("$$y_i =",slash_str,"hat{",slash_str,"beta}_0 +",slash_str,"hat{",slash_str,"beta}_1 DF_i")
        
        for(ii in 2:degree) {
          outstr = paste(outstr,'+ ',slash_str,'hat{',slash_str,'beta}_',ii,' DF_i^{',ii,'}',sep='')
        }
        
        outstr = paste0(outstr,'$$')
        
        return(tagList(withMathJax(),p(outstr) ))
        
      })
      
      
      output$pi_formulas <- renderUI({
        
        # note: req() did not work here on Metrics to prevent math output

        if(is.null(Metrics()$n_boot)) {
          return(NULL)
        }
    
        formulas = list(
          "Smoothed.R-squared" = 
            '$$ R^2_{smooth} = 1 - \\frac{\\big(e^{(s)}_{ij}\\big)^2}{SST} $$',
          "Smoothed.Sum.Squared.Error" = 
            '$$ SSE_{smooth} = \\frac{1}{\\hat{\\lambda}_1^2} \\sum_{i} \\sum_{j} \\Big( e^{(s)}_{ij} \\Big)^2  $$',
          "Smoothed.Scaled.Sum.Squared.Error" = 
            '$$ SSE_{smooth,scaled} = \\sum_{i} \\sum_{j} \\bigg( \\frac{e^{(s)}_{ij}}{\\hat{\\lambda}_{ij}} \\bigg)^2 $$',
          "Smoothed.Sum.Absolute.Error"=
            '$$ SAE_{smooth} = \\frac{1}{\\hat{\\lambda}_1 } \\sum_{i} \\sum_{j} \\big| e^{(s)}_{ij} \\big|  $$',
          "Smoothed.Scaled.Sum.Absolute.Error"=
            '$$ SAE_{smooth,scaled} =  \\sum_{i} \\sum_{j} \\frac{\\big| e^{(s)}_{ij} \\big|}{\\hat{\\lambda}_{ij}}  $$',
          'Variance.Stabilized.Smoothed.Sum.Squared.Error'=
            '$$ SSE_{VS-smooth,scaled} =  \\sum_{i} \\sum_{j} \\frac{\\Big( e^{(s)}_{ij} \\big)^2}{\\hat{\\lambda}_{ij}}  $$',
          'Variance.Stabilized.Smoothed.Sum.Absolute.Error'=
            '$$ SAE_{VS-smooth,scaled} =  \\sum_{i} \\sum_{j} \\frac{\\big| e^{(s)}_{ij} \\big|}{\\sqrt{\\hat{\\lambda}_{ij}}}  $$')
        
        inds = which(names(formulas) %in% get_metrics_names()$all_metrics[as.numeric(Metrics()$perf_metrics)])
        string = ''
        for(i in inds) {
          string = paste(string,formulas[[i]],sep='\n')
        }
        
        return(htmltools::tagList(withMathJax(),string))
        

      })
      
      output$var_assumption <- renderText({
        
        var_func = as.numeric(Metrics()$var_func_ind)
        
        assump = c('Variance in concentration is proportional to the mean concentration',
                   'Variance in concentration is constant across all dilution fractions.',
                   'The standard deviation of the concentration is proportional to the mean concentration')[var_func]
        
        return(paste0("Variance assumption: ",assump))
          
        
      })
      
      output$prop_const <- DT::renderDataTable({
        outdf <- Metrics()$metrics %>%
          dplyr::select(counting_method,cell_type,Metric,Value,lower,upper) %>%
          dplyr::filter(Metric == 'Prop.Const.x') %>%
          dplyr::select(-Metric)
        
        for(var in c('Value','lower','upper')) {
          outdf[,var] <- sapply(outdf[,var],round_or_truncate,3)
        }
        
        colnames(outdf) = c('Counting Method','Cell Type','Prop. Const.','Bootstrap Lower CL','Bootstrap Upper CL')
        
        outdf
      },options=list(searching=FALSE,paging=FALSE))
      
      
    })
}


tp7UI <- function(id) {
  ns <- NS(id)
  tagList(
    withMathJax(),
    br(),
    span(textOutput(ns("insufficient_design")),style='color:Tomato'),
    br(),
    h4("Quality Metric Comparison Description:"),
    p("When multiple cell counting methods are present in the uploaded dataset,",
      "the following",
      "quality metrics are compared between all pairs of methods: Proportionality Index, R-squared, and percent CV.",
      "Results are presented in tables below.",
      "The metrics are considered statistically different if the bootstrap confidence interval for",
      "the given ratio does not contain 1. This is indicated with a 'yes' or 'no' in the",
      "column labeled 'Significant'."),
    br(),
    h3("Proportionality Index Comparison Table",align = 'center'),
    br(),
    DT::dataTableOutput(ns('pi_comparison_table')),
    br(),
    h3("R Squared Comparison Table",align = 'center'),
    br(),
    DT::dataTableOutput(ns('r2_comparison_table')),
    br(),
    h3("Proportionality Constant Comparison Table",align = 'center'),
    br(),
    DT::dataTableOutput(ns('pc_comparison_table')),
    br(),
    h3("CV Comparison Table",align = 'center'),
    br(),
    DT::dataTableOutput(ns('cv_comparison_table')),
    br(),
    h3("Discrimination Plots",align = 'center'),
    br(),
    plotOutput(ns('method_precision_plot')),
    br(),
    p('The above plot uses the fitted flexible model to estimate',
      'the range of instrument readings that could pluasibly have generated',
      'the observed reading at the dilution fraction on the x-axis. For example, at the x-axis',
      'value of 0.50, the vertical range on the y-axis gives the range of dilution',
      'fractions that could have generated the cell count observed (or predicted) at 0.50.',
      'The range is computed by gathering all target (or measured) DF values',
      'whose prediction intervals contain the cell count corresponding to the desired',
      'dilution fraction. If a counting method produces prediction intervals that are non-monotonic,',
      'the method will not be displayed in the above plot.'),
    br(),
    plotOutput(ns('method_precision_plot_conc')),
    br(),
    p('The above plot uses the fitted flexible model to estimate',
      'the range of concentration readings (y-axis) that are nominally similar to those at a given concentration (x-axis) based on the fitted model.',
      'If a counting method produces prediction intervals that are non-monotonic,',
      'the method will not be displayed in the above plot.'),
    br(),
    plotOutput(ns('prediction_interval_plot')),
    br(),
    p("The above plot displays the predicted values (solid line) along with prediction intervals (dashed lines) for each counting method.")

  )
}

tp7Server <- function(id, Metrics) {
  moduleServer(
    id,
    function(input,output,session) {
      
      output$insufficient_design <- renderText({
        
        if(!is.null(Metrics()$exp_des_flag) && Metrics()$exp_des_flag) {
          return(descriptions$design_disclaimer)
          
        } else{
          return(NULL)
        }
        
      })
      
      output$method_precision_plot <- renderPlot({
        
        return(discrimination_bands_plot(Metrics())$db_plot_df)
        
      })
      
      output$method_precision_plot_conc <- renderPlot({
        
        return(discrimination_bands_plot(Metrics())$db_plot_conc)
        
      })
      
      output$prediction_interval_plot <- renderPlot({
        
        return(discrimination_bands_plot(Metrics())$pred_ints_plot)
        
      })
      
      output$pi_comparison_table <- DT::renderDataTable({
        
        if(is.null(Metrics()$compare)) {
          return(NULL)
        }
        
        if(!Metrics()$multiple_methods) {
          return(NULL)
        }
        
        outdf <- Metrics()$compare %>%
          as.data.frame() %>%
          dplyr::filter(Metric %in% c(metrics_key_value(as.numeric(Metrics()$perf_metrics)))) 
        
        for(var in c('Ratio','lower','upper')) {
          outdf[,var] = sapply(outdf[,var],signif,5)
        }
        
        outdf$sig = c('no','yes')[as.numeric( (1 < outdf$lower) | (outdf$upper < 1)) + 1 ]
        
        colnames(outdf) = c('Metric','Method 1','Method 2',
                            'Ratio','Bootstrap Lower CL','Bootstrap Upper CL','Significant')
        
        outdf$Metric <- gsub('Mean.Absolute.Error','MAE',outdf$Metric)
        outdf$Metric <- gsub('Mean.Squared.Error','MSE',outdf$Metric)
        
        outdf
        
      },options=list(searching=FALSE,ordering=FALSE))
      
      output$r2_comparison_table <- DT::renderDataTable({
        
        if(is.null(Metrics()$compare)) {
          return(NULL)
        }
        
        if(!Metrics()$multiple_methods) {
          return(NULL)
        }
        
        outdf <- Metrics()$compare %>%
          as.data.frame() %>%
          dplyr::filter(Metric == "R.squared")
        
        for(var in c('Ratio','lower','upper')) {
          outdf[,var] = sapply(outdf[,var],signif,5)
        }
        
        outdf$sig = c('no','yes')[as.numeric( (1 < outdf$lower) | (outdf$upper < 1)) + 1 ]
        
        colnames(outdf) = c('Metric','Method 1','Method 2',
                            'Ratio','Bootstrap Lower CL','Bootstrap Upper CL','Significant')
        
        outdf
        
      },options=list(searching=FALSE,ordering=FALSE))
      
      output$pc_comparison_table <- DT::renderDataTable({
        
        if(is.null(Metrics()$compare)) {
          return(NULL)
        }
        
        if(!Metrics()$multiple_methods) {
          return(NULL)
        }
        
        outdf <- Metrics()$compare %>%
          as.data.frame() %>%
          dplyr::filter(Metric == "Prop.Const.x")
        
        for(var in c('Ratio','lower','upper')) {
          outdf[,var] = sapply(outdf[,var],signif,5)
        }
        
        outdf$sig = c('no','yes')[as.numeric( (1 < outdf$lower) | (outdf$upper < 1)) + 1 ]
        
        colnames(outdf) = c('Metric','Method 1','Method 2',
                            'Ratio','Bootstrap Lower CL','Bootstrap Upper CL','Significant')
        
        outdf
        
      },options=list(searching=FALSE,ordering=FALSE))
      
      output$cv_comparison_table <- DT::renderDataTable({
        
        if(is.null(Metrics()$compare)) {
          return(NULL)
        }
        
        if(!Metrics()$multiple_methods) {
          return(NULL)
        }
        
        outdf <- Metrics()$compare %>%
          as.data.frame() 
        
        outdf = outdf[grep('CV',outdf$Metric,TRUE),]
        
        for(var in c('Ratio','lower','upper')) {
          outdf[,var] = sapply(outdf[,var],signif,5)
        }
        
        outdf$sig = c('no','yes')[as.numeric( (1 < outdf$lower) | (outdf$upper < 1)) + 1 ]
        
        colnames(outdf) = c('Metric','Method 1','Method 2',
                            'Ratio','Bootstrap Lower CL','Bootstrap Upper CL','Significant')
        
        outdf
        
      },options=list(searching=FALSE,ordering=FALSE))
      
  
    }
  )
}