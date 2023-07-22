# Dilution Integrity, Viability


tpDilutionUI <- function(id) {
  ns = NS(id)
  tagList(
    br(),
    span(textOutput(ns("insufficient_design")),style='color:Tomato'),
    br(),
    textOutput(ns('no_te_col')),
    plotOutput(ns('exp_diff')),
    uiOutput(ns('select_method')),
    br(),
    p('The above plot shows the scaled difference between the measured concentration',
      'and the expected concentration (based on the fitted proportional model,',
      'represented by the horizontal black line) over time.',
      'Each replicate observation appears as a different colored dot.',
      'The size of the dots correspond to each dilution fraction.',
      'The toggle button allows the user to choose one method to plot at a time,',
      'or all methods can be viewed simultaneously.',
      'Systematic deviation from the horizontal line over time may suggest instability in',
      'the dilution series experimental design.'),
    br(),
    h4("Viability Over Time Comparison",align='center'),
    textOutput(ns('no_via_col')),
    plotOutput(ns("via_over_time")),
    br(),
    fluidRow(
      column(width=6,offset=2,DT::dataTableOutput(ns('via_over_time_tests'),width='80%'),align='center')
    ),
    br(),
    h4("Pipette Error",align="center"),
    textOutput(ns('pip_err_no_mdf')),
    plotOutput(ns('pip_err')),
    br()

  )
}


tpDilutionServer <- function(id,Metrics) {
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
      
      output$no_te_col <- renderText({

        req(Metrics()$dat)

        data = Metrics()$dat

        if(is.null(data$time_elapsed)) {
          return("No time_elapsed or starting_soln_conc column provided.")

        } else if (sum(!is.na(data$time_elapsed) ) < 2){
          return("'time_elapsed' column either missing or contains too many NAs.")
          
        } else if (sum(!is.na(data$starting_soln_conc) ) < 2){
            return("'starting_soln_conc' column either contains too many NAs.")


        } else {
          return(NULL)
        }

      })
      
      output$select_method <- renderUI({
        
        req(Metrics()$dat)
        
        dat = Metrics()$dat
        
        if(sum(!is.na(dat$time_elapsed)) < 3) {
          return(NULL)
        }
        
        if(sum(!is.na(dat$starting_soln_conc)) < 3) {
          return(NULL)
        }
        
        tagList(
          selectInput(session$ns('which_method'),"Display Method:",choices=c('All',unique(dat$counting_method)))
        )
        
      })
      
      output$pip_err_no_mdf <- renderText({
        
        req(Metrics()$dat)
        
        if(!Metrics()$mdf_exists) {
          return(
            paste("Pipette Error plot not displayed because no measured",
                  "dilution fraction was submitted.")
          )
        } else {
          return(NULL)
        }
        
      })
      
      output$pip_err <- renderPlot({
        
        pipette_error_plot(Metrics())

      })
      
      output$exp_diff <- renderPlot({
        
        
        if (is.null(Metrics()$dat) || is.null(input$which_method)) {
          return(NULL)
        }
          
        p = experimental_integrity_plot(Metrics(),input$which_method)
      
        return(p)

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
        
        via_over_time_plot(Metrics())
        
      })
      
      output$via_over_time_tests <- DT::renderDataTable({
        
        data = Metrics()$dat
        
        if(is.null(data$percent_viable_cells)) {
          return(NULL)
          
        } else if(sum(!is.na(data$time_elapsed)) < 2) {
          return(NULL)
          
        } else if(sum(!is.na(data$percent_viable_cells)) < 2) {
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
    span(textOutput(ns("insufficient_design")),style='color:Tomato'),
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
    fluidRow(column(width=12,align='center',tableOutput(ns('pairwise_ks')))),
    helpText(paste("The KS test tests to see whether there appears to be significant",
                   "differences between the distributions of the two groups being tested.",
                   "Above, we are testing whether the absolute residuals about the median",
                   "have similar or different spreads.",
                   "P-values are displayed on the inside of the table for the KS test",
                   "between the method given in the corresponding row and column.",
                   "A p-value below the chosen significance level indicates evidence that the spreads of the two distributions",
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
      
      output$insufficient_design <- renderText({
        
        if(!is.null(Metrics()$exp_des_flag) && Metrics()$exp_des_flag) {
          return(descriptions$design_disclaimer)
          
        } else{
          return(NULL)
        }
        
      })
      
      output$no_via_col <- renderText({
        
        req(Metrics()$dat)

        data = Metrics()$dat

        if(is.null(data$percent_viable_cells)) {
          return("No percent_viable_cells column provided.")

        } else if (sum(!is.na(data$percent_viable_cells) ) < 2){
          return("'percent_viable_cells column' either contains NAs or too few observed values.")

        } else {
          return(NULL)
        }

      })
      
      output$only_one_group <- renderText({
        
        data = Metrics()$dat
        
        if(is.null(data$percent_viable_cells)) {
          return(NULL)
       
        } else if (sum(!is.na(data$percent_viable_cells) ) < 2) {
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
          
        } else if (sum(!is.na(data$percent_viable_cells) ) < 2) {
          return(NULL)
        }
        
        good_inds = !is.na(data$percent_viable_cells)
        data = data[good_inds,]
        
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
      bordered = TRUE,
      digits=2)
      
      output$hist_plot <- renderPlot({
        
        return(viability_hist_plot(Metrics()))
  
      })
      
      output$emp_cdf <- renderPlot({
      
        empirical_cdf_plot(Metrics())
        
      })
      
      output$emp_hist <- renderPlot({
        
        viability_resids_hist_plot(Metrics())
        
      })
      
      output$pairwise_ks <- renderTable({
        data = Metrics()$dat
        
        if(is.null(data$percent_viable_cells)) {
          return(NULL)
          
        } else if (sum(!is.na(data$percent_viable_cells) ) < 2) {
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
      bordered = TRUE,
      digits=3)
      
      
      output$emp_cdf_fct <- renderPlot({
        
        data = Metrics()$dat
        
        if(is.null(data$percent_viable_cells)) {
          return(NULL)
          
        } else if (sum(!is.na(data$percent_viable_cells) ) < 2) {
          return(NULL)
        }
        
        good_inds = !is.na(data$percent_viable_cell)
        data = data[good_inds,]
        
        data <- data %>%
          group_by(counting_method,stock_solution) %>%
          summarise(via_centered = percent_viable_cells - median(percent_viable_cells))
        
        data$counting_method = factor(data$counting_method)
        data$stock_solution = factor(data$stock_solution)
        
        ggplot(data, aes(x=via_centered,color=counting_method)) + 
          stat_ecdf() +
          xlab("percent_viable_cells (median-centered)") +
          ylab("Fraction of Data < x") +
          labs(color='Counting Method') +
          theme_bw() +
          the_theme
        
      })
      
      
    }
  )
}


