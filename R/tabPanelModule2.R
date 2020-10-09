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
  
        
      },options=list(searching=FALSE,ordering=FALSE))
      
      
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
        
      },options=list(searching=FALSE,ordering=FALSE))
      
      
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
        
      },options=list(searching=FALSE,ordering=FALSE))
      
      
      output$table4 <- DT::renderDataTable({
        
        out_df <- Metrics()$metrics %>% 
          as.data.frame() %>%
          dplyr::select(counting_method, Metric, Value, lower, upper) %>%
          dplyr::filter(Metric %in% c(Metrics()$metrics_to_plot)) %>%
          dplyr::filter(Metric != 'R.squared')
        
        colnames(out_df) <- c('Counting Method','Type of PI', 'PI', 'Lower CL','Upper CL')
        
        for(var in c('PI','Lower CL','Upper CL')) {
          out_df[,var] <- sapply(out_df[,var],round_or_truncate,3)
        }
        
        out_df
        
      },options=list(searching=FALSE,ordering=FALSE))
     
      
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
    h4('Variance assumption: 
       $$\\sigma^2_{ij} =  \\phi \\lambda_{ij} $$'),
    br(),
    h4('Estimates for proportionality constant \\( \\beta_1 \\)'),
    DT::dataTableOutput(ns('prop_const')),
    br(),
    h4('Calculating PI'),
    br(),
    h4('Smoothing Approach'),
    br(),
    h4('Number of bootstrap iterations conducted:'),
    h4(textOutput(ns('n_boot')),align='center'),
    
  )
}

tp6Server <- function(id, input_file, Metrics) {
  moduleServer(
    id,
    function(input,output,session) {
      req(Metrics,input_file)
      
      output$n_boot <- renderText({
        Metrics()$n_boot
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
      })
    })
}