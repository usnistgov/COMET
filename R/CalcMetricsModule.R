# options module

metricsUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(

    selectInput(ns("na_lab"), label = h4("How are missing values denoted?"),
              choices = c("Blank or indicated with 'NA' (default).",
                          "Other")),
    
    conditionalPanel(condition="input.na_lab == 'Other'",
                     textInput(ns('na_lab_custom'),label='Enter how missing values are indicated:',
                               value="Missing"),
                     ns = ns),
    actionButton(ns("help_na_lab"), "Help"),
    hr(),
    
    selectInput(ns("var_func"), label = h4("Variance assumption:"),
                choices = c('Variance proportional to mean (default)',
                            'Standard deviation proportional to mean',
                            'Constant variance'), selected = 'Variance proportional to mean (default)'),
    
    conditionalPanel(
      condition = "input.var_func == 'Custom'",
      textInput(ns('cus_var_func'),label='Variance is proportional to',value='mn^2'), 
      ns = ns
    ),
    
    actionButton(ns("help_var_func"), "Help"),
    hr(),


    selectInput(ns("smooth_df"), label = h4("Order of smoothing polynomial"),
                choices=c('Default (recommended)','Custom'), selected = 'Default (recommended)'),
    conditionalPanel(
      condition = "input.smooth_df == 'Custom'",
      numericInput(ns('smooth_df_cus'),label='Order of smoothing polynomial',value='5',min=2,max=NA,step=1), 
      ns = ns
    ),
    actionButton(ns("help_smooth_df"), "Help"),
    hr(),
    
    
    selectInput(ns("n_boot"), label = h4("Number of Bootstrap Iterations"), 
                choices = c('50 (test run)','1000 (final run)','custom'), selected = "50 (test run)"),
    conditionalPanel(
      condition = "input.n_boot == 'custom'",
      sliderInput(ns('n_boot_cus'),label='Number of Bootstrap Iterations',
                  min=50,max=2000,value=100,step=50), 
      ns = ns
    ),
    actionButton(ns("help_n_boot"), "Help"),
    hr(),
    
    
    sliderInput(ns("conf_lev"), label = h4("Confidence Level"), 
                min=.80,max=.99,step = .01,
                value = .95),
    actionButton(ns("help_conf_lev"), "Help"),
    hr(),
    

    checkboxGroupInput(ns("perf_metrics"), label = h4("Proportionality Indices"), 
                       choices = list(#"R-squared"=1,
                                      #"Sum Sq Error"=2,
                                      #"Sum Abs Error"=3,
                                      #"Scaled Sum Sq Error"=4,
                                      #"Scaled Sum Abs Error"=5,
                                      #"Smoothed R-squared"=6,
                                      "Smoothed Sum Sq Error"=7,
                                      "Smoothed Scaled Sum Sq Error (default)"=8,
                                      "Smoothed Sum Abs Error" = 9,
                                      "Smoothed Scaled Sum Abs Error"=10,
                                      "Variance Stabilized Sum Sq Error"=11,
                                      "Variance Stabilized Sum Abs Error"=12),
                       selected = 8),
    actionButton(ns("help_pis"),"Help"),
    hr(),
    #checkboxInput(ns('log_scale'),'Use Log Dilution Fraction?',value=FALSE),
    #hr(),
    actionButton(ns("goButton"), "Run Analysis")
  )

}

metricsServer <- function(id,input_file) {
  moduleServer(
    id,
    
    function(input,output,session) {
      
      observeEvent(input$help_na_lab, {
        showModal(modalDialog(
          title = "Help: Missing value expression",
          paste("Please type how missing values are written in the input file.",
                "(If there are no missing values, you may leave the input as is.)")
        ))
      })
      
      observeEvent(input$help_var_func, {
        showModal(modalDialog(
          title = "Help: Variance assumption",
          paste("Indicate the assumption regarding the variance of the measurements to be used for the modelling.",
                "Options: (1) variance is constant across all dilution fractions,",
                "(2) variance is proportional to the dilution fraction,",
                "or (3) variance is proportional to your custom function of the mean (mn).")
        ))
      })
      
      observeEvent(input$help_smooth_df, {
        showModal(modalDialog(
          title = "Help",
          "Input the order of the polynomial regression function used to calculated
          the smoothed residuals. The default is set to be the number of target 
          dilution fractions minus one. "
        ))
      })
      
      observeEvent(input$help_n_boot, {
        showModal(modalDialog(
          title = "Help: Number of bootstrap iterations",
          paste("Indicate the number of bootstrap samples to be run.",
                "These samples will be used to calculate confidence intervals",
                "of the computed metrics.",
                "A larger sample yields more accurate intervals,",
                "but takes longer to run.",
                "We recommend using a smaller number (e.g. 25) as a test run",
                "and then a larger sample size (e.g. 1000) for the final run.")
        ))
      })
      
      observeEvent(input$help_conf_lev, {
        showModal(modalDialog(
          title = "Help: Confidence level",
          paste("Please enter the desired level of confidence for the calculated metrics.",
                "A .95 level of confidence means that, roughly, you are '95% confident' that the",
                "true metric lies within the presented 95% confidence interval.",
                "A larger confidence level will necessarily yield wider intervals.")
        ))
      })
      
      observeEvent(input$help_pis, {
        showModal(modalDialog(
          title = "Help: Proportionality Indices",
          paste(descriptions$help_pis)
        ))
      })
      
      Metrics<-eventReactive(input$goButton, {
        
        
        # note: if 'mn' and '1' are changed, need to update stat analysis tab
        var_func <- switch(input$var_func,
                           'Variance proportional to mean (default)' = 1,
                           'Constant variance' = 2,
                           'Standard deviation proportional to mean' = 3)
        
        
        n_boot <- switch(input$n_boot,
                         '50 (test run)' = 50,
                         '1000 (final run)' = 1000,
                         'custom' = input$n_boot_cus)
        
        
        inFile <- input_file() 
        
        validate(
          need(!is.null(inFile),
               "No file uploaded. Please upload data file to begin analysis.")
        )
        
        # read file
        if(grepl('.xlsx$',inFile$datapath)) {
          
          dat <- tryCatch({
            simple_to_gui(readxl::read_excel(inFile$datapath,col_names = FALSE))
          }, error = function(e) { 
            return(data.frame())
          })
          
        } else {
          
          dat <- tryCatch({
            as.data.frame(readr::read_csv(inFile$datapath))
          }, error = function(e) {
            return(data.frame())
          })
          
        }
    
        # if they have a custom 'NA'
        expected_colnames = get_expected_colnames()
        
        if(input$na_lab == 'Other') {
          for(ii in 1:length(expected_colnames)) {
            
            dat[,expected_colnames[ii]][dat[,expected_colnames[ii]] == input$na_lab_custom] = NA
            
          }
        }
        
        if(input$smooth_df == 'Default (recommended)') {
          smooth_df = 0
        } else{
          smooth_df = input$smooth_df_cus
        }
        
        metrics = run_comet(dataset=dat,
                            n_boot=n_boot,
                            seed=123,
                            var_func=var_func,
                            smooth_df=smooth_df,
                            perf_metrics=input$perf_metrics,
                            conf_lev=as.numeric(input$conf_lev)) 
      
        
        
        return(metrics)
        
      })
    }
  )
}

