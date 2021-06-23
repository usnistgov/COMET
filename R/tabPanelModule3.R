# Dilution Integrity, Viability


tpDilutionUI <- function(id) {
  ns = NS(id)
  tagList(
    br(),
    plotOutput(ns('exp_diff')),
    uiOutput(ns('select_method')),
    br(),
    p('The above plot shows the absolute difference between the measured concentration',
      'and the expected concentration for each dilution fraction replicate observation',
      'over time.  Each replicate observation appears as a different color dot.',
      'The size of the dots correspond to each dilution fraction.',
      'The toggle button allows the user to choose one method to plot at a time,',
      'or all methods can be viewed simultaneously.'),
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
      
      output$select_method <- renderUI({
        
        req(Metrics()$dat)
        
        dat = Metrics()$dat
        
        tagList(
          selectInput(session$ns('which_method'),"Which Method",choices=c('All',unique(dat$counting_method)))
        )
        
      })
      
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
        if (is.null(Metrics()$dat) || is.null(input$which_method)) {
          return(NULL)
        }
        
        dat = Metrics()$dat
        
        if(input$which_method == 'All') {
          p = ggplot(dat,aes(x=time_elapsed,y=cell_conc-starting_soln_conc*target_dilution_fraction,
                         color=as.factor(rep_obsv),
                         shape=as.factor(analyst),
                         size=target_dilution_fraction))+
            geom_point(alpha=.7)+
            scale_size_continuous(range = c(2,6)) +
            geom_hline(yintercept=0)+
            xlab("Time Elapsed")+
            ylab("Difference from Expected Concentration")+
            guides(color=guide_legend(title = "Obs. Rep."))+
            guides(shape=guide_legend(title = "Analyst"))+
            theme_bw()+
            ggtitle("Sample Integrity Over Time")+
            theme(plot.title = element_text(hjust = 0.5,size=15)) +
            scale_size_continuous(breaks=unique(dat$target_dilution_fraction)) +
            facet_grid(counting_method~.)
          
        } else {
          p = ggplot(dat[dat$counting_method == input$which_method,],aes(x=time_elapsed,y=cell_conc-starting_soln_conc*target_dilution_fraction,
                         color=as.factor(rep_obsv),
                         shape=as.factor(analyst),
                         size=target_dilution_fraction))+
            geom_point(alpha=.7)+
            scale_size_continuous(range = c(2,6)) +
            geom_hline(yintercept=0)+
            xlab("Time Elapsed")+
            ylab("Difference from Expected Concentration")+
            guides(color=guide_legend(title = "Obs. Rep."))+
            guides(shape=guide_legend(title = "Analyst"))+
            theme_bw()+
            ggtitle("Sample Integrity Over Time")+
            theme(plot.title = element_text(hjust = 0.5,size=15)) +
            scale_size_continuous(breaks=unique(dat$target_dilution_fraction))
        }
        
        print(p)

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
    h4("Empirical CDF and Histogram", align='center'),
    fluidRow(column(6,plotOutput(ns("emp_cdf"))),column(6,plotOutput(ns("emp_hist")))),
    br(),
    h4("Pairwise Kolmogorov-Smirnov Test",align='center'),
    textOutput(ns('only_one_group')),
    fluidRow(column(width=6,offset=2,align='center',tableOutput(ns('pairwise_ks')))),
    helpText(paste("The KS test tests to see whether there appears to be significant",
                   "differences between the distributions of the two groups being tested.",
                   "Above, we are testing whether the absolute residuals about the median",
                   "have similar or different spreads.",
                   "A low p-value indicates evidence that the spreads of the two distributions",
                   "are not equivalent.")),
    br(),
    h4("Equivalence of Mean Percent Viable Cells",align='center'),
    fluidRow(column(width=8,offset=2,align='center',tableOutput(ns('equivalence_of_means')))),
    helpText(paste("Above are results from 2-sample t procedures for all counting method pairs.",
                   "The confidence interval is estimated for the difference 'Method 1' - 'Method 2'.",
                   "A low p-value indicates evidence that the mean percents of the two distributions",
                   "are different.")),
    #h4("Empirical CDF (by stock solution)", align='center'),
    #plotOutput(ns("emp_cdf_fct")),
    br()
  )
}

tpViabilityServer <- function(id,Metrics) {
  moduleServer(
    id,
    function(input,output,session) {
      
      output$no_via_col <- renderText({
        
        req(Metrics()$dat)

        data = Metrics()$dat

        if(is.null(data$percent_viable_cells)) {
          return("No viability information was provided.")

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
      
      output$equivalence_of_means <- renderTable({
        
        data = Metrics()$dat
        
        if(is.null(data$percent_viable_cells)) {
          return(NULL)
        }
        
        good_inds = !is.na(data$percent_viable_cells)
        
        if(length(unique(data$counting_method)) == 1) {
          return(NULL)
        }
        
        
        cms = unique(data$counting_method)
        
        all_combos = as.data.frame(t(combn(cms,2)))
        
        colnames(all_combos) = c('Method 1','Method 2')
        
        outmat = data.frame(ConfInt=rep('',nrow(all_combos)),pvalue=rep(0,nrow(all_combos)))
        
        outmat = cbind(all_combos,outmat)
        
        for(i in 1:nrow(outmat) ) {
          t_res = t.test(data$percent_viable_cells[data$counting_method == outmat[i,1]],
                         data$percent_viable_cells[data$counting_method == outmat[i,2]])
          lower = signif(t_res$conf.int[1],5)
          upper = signif(t_res$conf.int[2],5)
          outmat[i,3] = paste('(',lower,', ',upper,')',sep='')
          outmat[i,4] = t_res$p.value
        }
        
        
        return(outmat)
        
      },
      rownames = TRUE, 
      striped = TRUE,
      hover = TRUE,
      bordered = TRUE)
      
      output$hist_plot <- renderPlot({
        
        data = Metrics()$dat
        
        if(is.null(data$percent_viable_cells)) {
          return(NULL)
        }
        
        good_inds = !is.na(data$percent_viable_cell)
        
        ggplot(data[good_inds,],aes(x=percent_viable_cells,fill=counting_method)) + 
          geom_density(alpha=.3) + 
          geom_histogram(aes(y=stat(count)/sum(count)),position='dodge') +
          ylab("Proportion of Results")
        
      })
      
      output$emp_cdf <- renderPlot({
        
        data = Metrics()$dat
        
        if(is.null(data$percent_viable_cells)) {
          return(NULL)
        }
        
        good_inds = !is.na(data$percent_viable_cells)
        
        data = data[good_inds,]
        
        data <- data %>%
          group_by(counting_method) %>%
          summarise(via_abs_resid = abs(percent_viable_cells - median(percent_viable_cells)) )
        
        data$counting_method = factor(data$counting_method)
        
        ggplot(data, aes(x=via_abs_resid,colour=counting_method)) + 
          stat_ecdf() +
          xlab("Percent Viable (absolute residuals)") +
          ylab("Fraction of Data < x") +
          theme(legend.position = "none")
        
      })
      
      output$emp_hist <- renderPlot({
        
        data = Metrics()$dat
        
        if(is.null(data$percent_viable_cells)) {
          return(NULL)
        }
        
        good_inds = !is.na(data$percent_viable_cell)
        
        data = data[good_inds,]
        
        data <- data %>%
          group_by(counting_method) %>%
          summarise(via_abs_resid = abs(percent_viable_cells - median(percent_viable_cells)) )
        
        data$counting_method = factor(data$counting_method)
        
        ggplot(data, aes(x=via_abs_resid,fill=counting_method)) + 
          geom_histogram(aes(y=stat(count)/sum(count)),position='dodge') +
          xlab("Percent Viable (absolute residuals)") +
          ylab("Proportion of Data")
        
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
          summarise(via_abs_resid = abs(percent_viable_cells - median(percent_viable_cells)))
        
        cms = unique(data$counting_method)
        outmat = matrix(1,nrow=length(cms),ncol=length(cms))
        
        for(i in 1:(length(cms)-1) ) {
          for(j in (i+1):length(cms)) {
            ksres = ks.test(data$via_abs_resid[data$counting_method == cms[i]],
                            data$via_abs_resid[data$counting_method == cms[j]])
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
          xlab("percent_viable_cells (median-centered)") +
          ylab("Fraction of Data < x")
        
      })
      
      
    }
  )
}