# Dilution Integrity, Viability


tpDilutionUI <- function(id) {
  ns = NS(id)
  tagList(
    br(),
    plotOutput(ns('exp_diff')),
    br(),
    br(),
    plotOutput(ns('pip_err'))
  )
}

tpDilutionServer <- function(id,Metrics) {
  moduleServer(
    id,
    function(input,output,session) {
      
      output$pip_err <- renderPlot({
        
        data = Metrics()$dat %>% 
          group_by(counting_method,random_sample_number,target_dilution_fraction) %>%
          summarise(mdf = mean(measured_dilution_fraction))
        
        x = data$target_dilution_fraction
        y = data$mdf
        
        res = summary(lm(y~x-1))
        b_est = res$coefficients[1]
        b_se = res$coefficients[2]
        lab = paste("Slope (Std. Err): ",round(b_est,3)," (",round(b_se,4),")",sep='')
      
        
        ggplot(data,aes(x=target_dilution_fraction,y=mdf)) +
          geom_point() +
          geom_abline(slope=1,intercept=0) + geom_label(x=.4,y=.8,label=lab,size=5) +
          ylab("Measured Dilution Fraction") +
          xlab("Target Dilution Fraction") +
          ggtitle("Measured vs. Target DF Integrity") +
          theme(plot.title = element_text(hjust = 0.5,size=15))
        
      })
      
      output$exp_diff <- renderPlot({
        if (is.null(Metrics()$dat )) {
          return(NULL)
        }
        print(Metrics()$overview.plot2)
      })
      
    }
  )
  
  
}


tpViabilityUI <- function(id) {
  ns = NS(id)
  tagList(
    h4("Viability Comparison",align='center'),
    textOutput(ns('no_via_col')),
    plotOutput(ns('hist_plot')),
    br(),
    h4("Viability Over Time Comparison",align='center'),
    plotOutput(ns("via_over_time")),
    br(),
    h4("Empirical CDF", align='center'),
    plotOutput(ns("emp_cdf")),
    br(),
    h4("Empirical CDF (by replicate sample)", align='center'),
    plotOutput(ns("emp_cdf_fct")),
    br()
  )
}

tpViabilityServer <- function(id,Metrics) {
  moduleServer(
    id,
    function(input,output,session) {
      
      output$no_via_col <- renderText({
        
        data = Metrics()$dat
        
        if(is.null(data$viability)) {
          return("No viability column detected in dataset.")
        
        } else {
          return(NULL)
        }
        
      })
      
      output$hist_plot <- renderPlot({
        
        data = Metrics()$dat
        
        if(is.null(data$viability)) {
          return(NULL)
        }
        
        ggplot(data,aes(x=viability,fill=counting_method)) + 
          geom_density(alpha=.3) + 
          geom_histogram(aes(y=stat(count)/sum(count)),position='dodge') +
          facet_wrap(~replicate_sample) +
          ylab("")
        
      })
      
      
      output$via_over_time <- renderPlot({
        
        data = Metrics()$dat
        
        if(is.null(data$viability)) {
          return(NULL)
        }
        
        ggplot(data,aes(x=time_elapsed, y=viability, color=counting_method)) + 
          geom_point() + 
          facet_wrap(~replicate_sample) +
          geom_smooth(method='lm',se=FALSE) +
          xlab("Time Elapsed") +
          ylab("% Viable")
        
      })
      
      
      output$emp_cdf <- renderPlot({
        
        data = Metrics()$dat
        
        if(is.null(data$viability)) {
          return(NULL)
        }
        
        data <- data %>%
          group_by(counting_method) %>%
          summarise(via_centered = viability - median(viability))
        
        data$counting_method = factor(data$counting_method)
        
        ggplot(data, aes(x=via_centered,colour=counting_method)) + 
          stat_ecdf() +
          xlab("Viability (median-centered)")
        
      })
      
      
      output$emp_cdf_fct <- renderPlot({
        
        data = Metrics()$dat
        
        if(is.null(data$viability)) {
          return(NULL)
        }
        
        data <- data %>%
          group_by(counting_method,replicate_sample) %>%
          summarise(via_centered = viability - median(viability))
        
        data$counting_method = factor(data$counting_method)
        data$replicate_sample = factor(data$replicate_sample)
        
        ggplot(data, aes(x=via_centered,colour=counting_method)) + 
          stat_ecdf() +
          facet_wrap(~replicate_sample) +
          xlab("Viability (median-centered)")
        
      })
      
    }
  )
}