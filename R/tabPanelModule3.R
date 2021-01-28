# Dilution Integrity, Viability


tpDilutionUI <- function(id) {
  ns = NS(id)
  tagList(
    br(),
    plotOutput(ns('exp_diff')),
    br(),
    plotOutput(ns('pip_err')),
    br(),
    h4("Viability Over Time Comparison",align='center'),
    textOutput(ns('no_via_col')),
    plotOutput(ns("via_over_time")),
    br(),
    fluidRow(
      column(width=6,offset=2,DT::dataTableOutput(ns('via_over_time_tests'),width='80%'),align='center')
    )

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
          ggtitle("Pipetting Error") +
          theme(plot.title = element_text(hjust = 0.5,size=15))
        
      })
      
      output$exp_diff <- renderPlot({
        if (is.null(Metrics()$dat )) {
          return(NULL)
        }
        print(Metrics()$overview.plot2)
      })
      
      output$no_via_col <- renderText({
        
        data = Metrics()$dat
        
        if(is.null(data$percent_viable_cells)) {
          return("No column named 'percent_viable_cells' detected in dataset.")
          
        } else if (sum(!is.na(data$percent_viable_cells) ) < 2){
          return("'percent_viable_cells column' contains no observed values.")
          
        } else {
          return(NULL)
        }
        
      })
      
      output$via_over_time <- renderPlot({
        
        data = Metrics()$dat
        
        if(is.null(data$percent_viable_cells)) {
          return(NULL)
        }
        
        good_inds = !is.na(data$percent_viable_cell)
        
        ggplot(data[good_inds,],aes(x=time_elapsed, y=percent_viable_cells, color=counting_method)) + 
          geom_point() + 
          facet_wrap(~stock_solution) +
          geom_smooth(method='lm',se=FALSE) +
          xlab("Time Elapsed") +
          ylab("% Viable")
        
      })
      
      output$via_over_time_tests <- DT::renderDataTable({
        
        data = Metrics()$dat
        
        if(is.null(data$percent_viable_cells)) {
          return(NULL)
        }
        
        data_sub = data[!is.na(data$percent_viable_cell),]
        
        cms = unique(data_sub$counting_method)
        ncms = length(cms)
        
        outdf = data.frame("Method"=cms,"Slope Est."=0,"p"=0)
        
        for(ii in 1:ncms) {
          this_data = data_sub[data_sub$counting_method == cms[ii],]
          mod = summary(lm(percent_viable_cells~time_elapsed,data=this_data))
          outdf[ii,2:3] = round(c(mod$coefficients[2,1],mod$coefficients[2,4]),4)
        }
        
        names(outdf) = c("Method","Slope Estimate","p-value")
        outdf
        
      },options=list(searching=FALSE,paging=FALSE))
      
      
      
    }
  )
  
  
}


tpViabilityUI <- function(id) {
  ns = NS(id)
  tagList(
    br(),
    h4("Viability Comparison",align='center'),
    textOutput(ns('no_via_col')),
    plotOutput(ns('hist_plot')),
    br(),
    br(),
    h4("Empirical CDF", align='center'),
    plotOutput(ns("emp_cdf")),
    br(),
    h4("Pairwise Kolmogorov-Smirnov Test",align='center'),
    textOutput(ns('only_one_group')),
    fluidRow(column(width=6,offset=2,align='center',tableOutput(ns('pairwise_ks')))),
    br(),
    br(),
    h4("Empirical CDF (by stock solution)", align='center'),
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
        
        if(is.null(data$percent_viable_cells)) {
          return("No column named 'percent_viable_cells' detected in dataset.")
        
        } else if (sum(!is.na(data$percent_viable_cells) ) < 2){
          return("'percent_viable_cells column' contains no observed values.")
          
        } else {
          return(NULL)
        }
        
      })
      
      output$only_one_group <- renderText({
        
        data = Metrics()$dat
        
        if(is.null(data$percent_viable_cells)) {
          return(NULL)
        }
        
        good_inds = !is.na(data$percent_viable_cell)
        
        if(length(unique(data$counting_method[good_inds])) <= 1) {
          return(paste("Only one counting method has viability data.",
                       "Thus, the KS test cannot be computed."))
        
          } else {
          return(NULL)
        }
        
      })
      
      output$hist_plot <- renderPlot({
        
        data = Metrics()$dat
        
        if(is.null(data$percent_viable_cells)) {
          return(NULL)
        }
        
        good_inds = !is.na(data$percent_viable_cell)
        
        ggplot(data[good_inds,],aes(x=percent_viable_cells,fill=counting_method)) + 
          geom_density(alpha=.3) + 
          geom_histogram(aes(y=stat(count)/sum(count)),position='dodge') +
          facet_wrap(~stock_solution) +
          ylab("")
        
      })
      
      output$emp_cdf <- renderPlot({
        
        data = Metrics()$dat
        
        if(is.null(data$percent_viable_cells)) {
          return(NULL)
        }
        
        good_inds = !is.na(data$percent_viable_cell)
        
        data = data[good_inds,]
        
        data <- data %>%
          group_by(counting_method) %>%
          summarise(via_centered = percent_viable_cells - median(percent_viable_cells))
        
        data$counting_method = factor(data$counting_method)
        
        ggplot(data, aes(x=via_centered,colour=counting_method)) + 
          stat_ecdf() +
          xlab("percent_viable_cells (median-centered)")
        
      })
      
      output$pairwise_ks <- renderTable({
        data = Metrics()$dat
        
        if(is.null(data$percent_viable_cells)) {
          return(NULL)
          
        } 
        
        good_inds = !is.na(data$percent_viable_cell)
        
        data = data[good_inds,]
        
        if(length(unique(data$counting_method)) == 1) {
          return(NULL)
        }
        
        data <- data %>%
          group_by(counting_method) %>%
          summarise(via_centered = percent_viable_cells - median(percent_viable_cells))
        
        cms = unique(data$counting_method)
        outmat = matrix(1,nrow=length(cms),ncol=length(cms))
        
        for(i in 1:(length(cms)-1) ) {
          for(j in (i+1):length(cms)) {
            ksres = ks.test(data$via_centered[data$counting_method == cms[i]],
                            data$via_centered[data$counting_method == cms[j]])
            outmat[i,j] = ksres$p.value
            outmat[j,i] = ksres$p.value
          }
        }
        
        
        colnames(outmat) = cms
        rownames(outmat) = cms
        
        
        return(outmat)
        
        
      },
      rownames = TRUE, 
      striped = TRUE,
      hover = TRUE,
      bordered = TRUE)
      
      output$emp_cdf_fct <- renderPlot({
        
        data = Metrics()$dat
        
        if(is.null(data$percent_viable_cells)) {
          return(NULL)
        }
        
        good_inds = !is.na(data$percent_viable_cell)
        
        data = data[good_inds,]
        
        data <- data %>%
          group_by(counting_method,stock_solution) %>%
          summarise(via_centered = percent_viable_cells - median(percent_viable_cells))
        
        data$counting_method = factor(data$counting_method)
        data$stock_solution = factor(data$stock_solution)
        
        ggplot(data, aes(x=via_centered,colour=counting_method)) + 
          stat_ecdf() +
          facet_wrap(~stock_solution) +
          xlab("percent_viable_cells (median-centered)")
        
      })
      
      
    }
  )
}