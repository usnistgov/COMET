# Metrics Table and Statistical Analysis Modules (Tabs)

tp5UI <- function(id) {
  ns <- NS(id)
  tagList(
    withMathJax(),
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
    h3('Other Diagnostic Metrics',align='center'),
    br(),
    DT::dataTableOutput(ns('table5')),
    br()
  )
}

tp5Server <- function(id, input_file, Metrics){ 
  moduleServer(
    id,
    function(input,output,session) {
      req(Metrics,input_file)
      
      
      output$table1 <- DT::renderDataTable({
        
        out_df <- Metrics()$metrics %>% 
          dplyr::select(counting_method, Metric, Value, lower, upper) %>%
          dplyr::filter(grepl('mean_conc',Metric))
        
        out_df$Metric <- gsub('mean_conc_','',out_df$Metric)
        
        for(var in c('Value','lower','upper')) {
          out_df[,var] = round(out_df[,var],0)
        }
        
        colnames(out_df) <- c('Counting Method','Dilution Fraction','Mean Concentration','Lower CL','Upper CL')
        
        out_df
  
        
      },options=list(searching=FALSE))
      
      
      output$table2 <- DT::renderDataTable({
        
        out_df <- Metrics()$metrics %>% 
          dplyr::select(counting_method, Metric, Value, lower, upper) %>%
          dplyr::filter(grepl('pooled_cv',Metric))
        
        out_df$Metric <- gsub('pooled_cv_','',out_df$Metric)
        
        for(var in c('Value','lower','upper')) {
          out_df[,var] = round(out_df[,var],3)
        }
        
        colnames(out_df) <- c('Counting Method','Dilution Fraction','Mean %CV','Lower CL','Upper CL')
        
        out_df
        
      },options=list(searching=FALSE))
      
      
      output$table3 <- DT::renderDataTable({
        
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
        
      },options=list(searching=FALSE))
      
      
      output$table4 <- DT::renderDataTable({
        
        out_df <- Metrics()$metrics %>% 
          as.data.frame() %>%
          dplyr::select(counting_method, Metric, Value, lower, upper) %>%
          dplyr::filter(Metric %in% c(Metrics()$metrics_to_plot)) %>%
          dplyr::filter(grepl('smooth',Metric,ignore.case = TRUE))
        
        colnames(out_df) <- c('Counting Method','Type of PI', 'PI', 'Lower CL','Upper CL')
        
        for(var in c('PI','Lower CL','Upper CL')) {
          out_df[,var] <- sapply(out_df[,var],round_or_truncate,3)
        }
        
        out_df
        
      },options=list(searching=FALSE))
      
      
      output$table5 <- DT::renderDataTable({
        
        out_df <- Metrics()$metrics %>% 
          as.data.frame() %>%
          dplyr::select(counting_method, Metric, Value, lower, upper) %>%
          dplyr::filter(Metric %in% c(Metrics()$metrics_to_plot)) %>%
          dplyr::filter(!grepl('smooth',Metric,ignore.case = TRUE)) %>%
          dplyr::filter(Metric != 'R.squared')
        
        colnames(out_df) <- c('Counting Method','Metric', 'Value', 'Lower CL','Upper CL')
        
        for(var in c('Value','Lower CL','Upper CL')) {
          out_df[,var] <- sapply(out_df[,var],round_or_truncate,3)
        }
        
        out_df
        
      },options=list(searching=FALSE))
     
      
    })
}

tp6UI <- function(id) {
  ns <- NS(id)
  tagList(
    withMathJax(),
    br(),
    h3("Statistical Modelling Details",align='center'),
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
    plotOutput(ns('prop_const_plot')),
    br(),
    h4('Calculating PI'),
    p('For all PI formulas, we define \\( e^{(s)}_{ij} = \\hat{Y}_{ij}^{polynomial} - \\hat{Y}_{ij}^{proportional} \\).'),
    uiOutput(ns('pi_formulas')),
    br(),
    h4('Smoothing Approach'),
    br(),
    h4(textOutput(ns('n_boot'))),
    
  )
}

tp6Server <- function(id, input_file, Metrics) {
  moduleServer(
    id,
    function(input,output,session) {
      req(Metrics,input_file)
      
      output$n_boot <- renderText({
        paste('Number of bootstrap iterations conducted:',Metrics()$n_boot)
      })
      
      output$pi_formulas <- renderUI({
        
        # note: req() did not work here on Metrics to prevent math output
        res = tryCatch(is.null(Metrics()$n_boot),
                       error = function(e) return(TRUE))
        
        if(!res) {
          
          formulas = list(
            "Smoothed.R-squared" = 
              '$$ R^2_{smooth} = 1 - \\frac{\\big(e^{(s)}_{ij}\\big)^2}{SST} $$',
            "Smoothed.Sum.Squared.Error" = 
              '$$ SSE_{smooth} = \\frac{1}{\\hat{\\beta}_1 N} \\sum_{i} \\sum_{j} \\Big( e^{(s)}_{ij} \\Big)^2  $$',
            "Smoothed.Scaled.Sum.Squared.Error" = 
              '$$ SSE_{smooth,scaled} = \\frac{1}{N} \\sum_{i} \\sum_{j} \\bigg( \\frac{e^{(s)}_{ij}}{\\hat{\\lambda}_{ij}} \\bigg)^2 $$',
            "Smoothed.Sum.Absolute.Error"=
              '$$ SAE_{smooth} = \\frac{1}{\\hat{\\beta}_1 N} \\sum_{i} \\sum_{j} \\big| e^{(s)}_{ij} \\big|  $$',
            "Smoothed.Scaled.Sum.Absolute.Error"=
              '$$ SAE_{smooth,scaled} = \\frac{1}{N} \\sum_{i} \\sum_{j} \\frac{\\big| e^{(s)}_{ij} \\big|}{\\hat{\\lambda}_{ij}}  $$')
          
          
          inds = which(names(formulas) %in% Metrics()$metrics_to_plot)
          string = ''
          for(i in inds) {
            string = paste(string,formulas[[i]],sep='\n')
          }
          
          withMathJax(
            helpText(string))
        }
        

      })
      
      output$var_assumption <- renderText({
        
        f_body = deparse(body(Metrics()$var_func))
        f_body_len = length(strsplit(f_body,'')[[1]])
        
        if(f_body == 'mn') {
            
            return('Variance Assumption: Variance of cell count is proportional to dilution fraction.')
            
        } else if(f_body == '1') {
          withMathJax()
          return('Variance Assumption: Variance of cell count is constant across dilution fractions.')
        
        } else {
          return(paste('Variance Assumption: Variance is proportional to:',body(var_func)))
        }
        
      })
      
      output$prop_const <- DT::renderDataTable({
        outdf <- Metrics()$metrics %>%
          dplyr::select(counting_method,cell_type,Metric,Value,lower,upper) %>%
          dplyr::filter(Metric == 'Prop.Const.x') %>%
          dplyr::select(-Metric)
        
        for(var in c('Value','lower','upper')) {
          outdf[,var] <- sapply(outdf[,var],round_or_truncate,3)
        }
        
        colnames(outdf) = c('Counting Method','Cell Type','Prop. Const.','Lower CL','Upper CL')
        
        outdf
      },options=list(searching=FALSE,paging=FALSE))
      
      
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
          ggtitle("Proportionality Constants") +
          xlab("Method") +
          ylab("Proportionality Constant (with Bootstrap CI)") +
          theme(plot.title = element_text(hjust = 0.5,size=20))
        
        
      })
    })
}

tp7UI <- function(id) {
  ns <- NS(id)
  tagList(
    br(),
    h3("Comparison Table",align = 'center'),
    br(),
    DT::dataTableOutput(ns('comparison_table')),
    br(),
    h3("Method Precision Plot",align = 'center'),
    br(),
    plotOutput(ns('method_precision_plot')),
    br(),
    h3("Bias Comparison Table",align = 'center'),
    br(),
    DT::dataTableOutput(ns('bias_table'))
  )
}

tp7Server <- function(id, Metrics) {
  moduleServer(
    id,
    function(input,output,session) {
      req(Metrics)
      
      output$method_precision_plot <- renderPlot({
        if(is.null(Metrics()$compare)) {
          return(NULL)
        }
        
        outdf = Metrics()$prediction.ints
        
        # to make the bands look nice
        # subtract mean and add the line y=x
        outdf$lwr_centered = outdf$lwr - outdf$y 
        outdf$upr_centered = outdf$upr - outdf$y 
        
        p = ggplot(outdf) + 
          geom_line(aes(x=x,y=lwr_centered,color=comp_level)) +
          geom_line(aes(x=x,y=upr_centered,color=comp_level)) +
          ylab('Centered Cell Concentration') +
          xlab('Dilution Fraction')
        
        print(p)
      })
      
      output$comparison_table <- DT::renderDataTable({
        
        if(is.null(Metrics()$compare)) {
          return(NULL)
        }
        
        outdf <- Metrics()$compare %>%
          as.data.frame() %>%
          dplyr::filter(Metric %in% Metrics()$metrics_to_plot)
          
        
        for(var in c('Ratio','lower','upper')) {
          outdf[,var] = sapply(outdf[,var],round_or_truncate,3)
        }
        
        outdf$sig = c('no','yes')[as.numeric( (1 < outdf$lower) | (outdf$upper < 1)) + 1 ]
        
        colnames(outdf) = c('Metric','Method 1','Method 2',
                            'Ratio','Lower CL','Upper CL','Significant')
        
        outdf$Metric <- gsub('Mean.Absolute.Error','MAE',outdf$Metric)
        outdf$Metric <- gsub('Mean.Squared.Error','MSE',outdf$Metric)
        
        outdf
        
      },options=list(searching=FALSE,ordering=FALSE))
      
      
      output$bias_table <- DT::renderDataTable({
        
        if(is.null(Metrics()$compare)) {
          return(NULL)
        }
        
        outdf <- Metrics()$compare %>%
          as.data.frame() %>%
          dplyr::filter(Metric == 'Prop.Const.x')
        
        
        for(var in c('Ratio','lower','upper')) {
          outdf[,var] = sapply(outdf[,var],round_or_truncate,3)
        }
        
        outdf$sig = c('no','yes')[as.numeric( (1 < outdf$lower) | (outdf$upper < 1)) + 1 ]
        
        outdf$bias = 100*(1 - outdf$Ratio)
        outdf$bias_upper = apply(cbind(100*(1 - outdf$upper), 100*(1 - outdf$lower)),MARGIN=1,max )
        outdf$bias_lower = apply(cbind(100*(1 - outdf$upper), 100*(1 - outdf$lower)),MARGIN=1,min )
        
        outdf = outdf[,c('level1','level2','bias','bias_lower','bias_upper','sig')]
        
        colnames(outdf) = c('Method 1', 'Method 2','%Bias', 'Lower CL', 'Upper CL', 'Significant')
        
        outdf
        
      },options=list(searching=FALSE,ordering=FALSE))
  
    }
  )
}