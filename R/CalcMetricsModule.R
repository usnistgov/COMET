# options module

metricsUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    textInput(ns("na_lab"), label = h4("How are missing values denoted in the file?"), value = "NA"),
    actionButton(ns("help_na_lab"), "Help"),
    hr(),
    
    selectInput(ns("var_func"), label = h4("Variance is ..."),
                choices = c('proportional to mean','constant','custom'), selected = "proportional to mean"),
    conditionalPanel(
      condition = "input.var_func == 'custom'",
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
                  min=50,max=5000,value=200,step=50), 
      ns = ns
    ),
    actionButton(ns("help_n_boot"), "Help"),
    hr(),
    
    
    sliderInput(ns("conf_lev"), label = h4("Confidence Level"), 
                min=.80,max=.99,step = .01,
                value = .95),
    actionButton(ns("help_conf_lev"), "Help"),
    hr(),
    

    checkboxGroupInput(ns("perf.metrics"), label = h4("Proportionality Indices"), 
                       choices = list(#"R-squared"=1,
                                      #"Sum Sq Error"=2,
                                      #"Sum Abs Error"=3,
                                      "Smoothed Scaled Sum Sq Error (recommended)"=8,
                                      "Scaled Sum Sq Error"=4,
                                      "Scaled Sum Abs Error"=5,
                                      "Smoothed R-squared"=6,
                                      "Smoothed Sum Sq Error"=7,
                                      "Smoothed Sum Abs Error" = 9,
                                      "Smoothed Scaled Sum Abs Error"=10),
                       selected = 8),
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
      
      Metrics<-eventReactive(input$goButton, {
        
        
        # note: if 'mn' and '1' are changed, need to update stat analysis tab
        function_body <- switch(input$var_func,
                                'proportional to mean' = 'mn',
                                'constant' = '1',
                                'custom' = input$cus_var_func)
        
        eval(parse(text=paste("var_func<-function(mn)",function_body))) 
        
        n_boot <- switch(input$n_boot,
                         '50 (test run)' = 50,
                         '1000 (final run)' = 1000,
                         'custom' = input$n_boot_cus)
        
        if(n_boot<20) {
          n_boot<- 0
        } 
        
        inFile <- input_file() # input$file1
        conf_lev<-as.numeric(input$conf_lev)
        
        if (is.null(inFile)) {
          return(NULL)
        }
        
        dat<- as.data.frame(readr::read_csv(inFile$datapath))
        
        expected_colnames = c('counting_method',
                              'random_sample_number',
                              'stock_extraction',
                              'target_dilution_fraction',
                              'replicate_sample',
                              'rep_obsv',
                              'analyst',
                              'time_elapsed',
                              'cell_conc')
        
        validate(
          need('counting_method' %in% colnames(dat),
               "No column named 'counting_method' detected in dataset."),
          
          need('random_sample_number' %in% colnames(dat),
               "No column named 'random_sample_number' detected in dataset."),
          
          need('stock_extraction' %in% colnames(dat),
               "No column named 'stock_extraction' detected in dataset."),
          
          need('target_dilution_fraction' %in% colnames(dat),
               "No column named 'target_dilution_fraction' detected in dataset."),
          
          need('replicate_sample' %in% colnames(dat),
               "No column named 'replicate_sample' detected in dataset."),
          
          need('rep_obsv' %in% colnames(dat),
               "No column named 'rep_obsv' detected in dataset."),
          
          need('analyst' %in% colnames(dat),
               "No column named 'rep_obsv' detected in dataset."),
          
          need('time_elapsed' %in% colnames(dat),
               "No column named 'time_elapsed' detected in dataset."),
          
          need('cell_conc' %in% colnames(dat),
               "No column named 'cell_conc' detected in dataset."),
          
          need(length(unique(dat$target_dilution_fraction)) < 15,
               "Too many unique target dilution fractions."),
          
          need(all(is.numeric(dat$random_sample_number)))
        )
        

          
        dat <- dat[dat$counting_method != '',]
        dat$cell_conc<-as.numeric(as.character(dat$cell_conc))
        dat$target_dilution_fraction<-as.numeric(as.character(dat$target_dilution_fraction))
        
        mdf = dat$measured_dilution_fraction
        
        if(all(is.na(mdf)) || is.null(mdf) ) {
          dat$measured_dilution_fraction<-as.numeric(as.character(dat$target_dilution_fraction))
          mdf_exists = FALSE
          
        } else {
          dat$measured_dilution_fraction<-as.numeric(as.character(dat$measured_dilution_fraction))
          mdf_exists = TRUE
        }
          
        
        dat<-dat%>%arrange(counting_method,
                           cell_type,
                           concentration_type,
                           stock_solution,
                           target_dilution_fraction,
                           measured_dilution_fraction,
                           stock_extraction)
        
        
        if(input$smooth_df == 'Default (recommended)') {
          smooth_df <- length(unique(dat$target_dilution_fraction)) - 1
          print(smooth_df)
        } else {
          smooth_df <- as.numeric(input$smooth_df_cus)
          validate(
            need(smooth_df <= length(unique(dat$target_dilution_fraction)) - 1,
                 "Polynomial model degrees of freedom too high;
                 model is over-constrained."),
            need(smooth_df >= 1,
                 "Polynomial model degrees of freedom should be at least 1.")
          )
        }
        
        
        #### How many comparison factors are there in the dataset?
        grouping_factors<-c("counting_method","cell_type","concentration_type")
        n_compare<-nrow(distinct(dat[,grouping_factors]))
        n_levels<-apply(dat[,grouping_factors],2,function(x)length(unique(x)))
        n_comparison_facs<-sum(n_levels>1)
        
        #### If there's one comparison factor, identify it
        if(n_comparison_facs==1) factor_to_compare<-grouping_factors[n_levels>1]
        
        #### If there's no comparison factor, set to NULL 
        if(n_comparison_facs==0) factor_to_compare<-'counting_method'
        
        #### If there's more than one comparison factor, shut off plots
        metrics<-calc.metrics(dat,var_func,smooth_df,plot.bool=n_comparison_facs<2,factor_to_compare)
        metrics$metrics$upper<-metrics$metrics$lower<-NULL
        
        withProgress(message = "Running Bootstrap Iterations", value=0, {
          if(n_boot>20){
            boot.metrics<-matrix(NA,nrow(metrics$metrics),n_boot)
            for(i in 1:n_boot){
              boot.ind <- nonpar.boot(dat,i)
              boot.dat <- dat[boot.ind[,1],]
              boot.dat$cell_conc <- dat$cell_conc[boot.ind[,2]]
              boot.dat$cell_conc[is.na(dat$cell_conc)] <- NA
              boot.dat$stock_extraction<-dat$stock_extraction
              boot.metrics[,i]<-unlist(calc.metrics(boot.dat,
                                                    var_func,
                                                    smooth_df,
                                                    plot.bool=FALSE,
                                                    factor_to_compare)$metrics$Value)
              
              incProgress(1/n_boot, detail = paste("Sample",i,"of",n_boot))
            }
            
            boot.metrics[is.infinite(boot.metrics)] = NA
            
            # transform skewed variables (everything except mean and cv)
            rows_to_transform = grepl('R.squared',metrics$metrics$Metric,ignore.case=TRUE)
            rows_to_transform_nonzero <- grepl('error',metrics$metrics$Metric,ignore.case = TRUE)
            boot.metrics[rows_to_transform,] = mylogit(boot.metrics[rows_to_transform,])
            boot.metrics[rows_to_transform_nonzero,] = log(boot.metrics[rows_to_transform_nonzero,])
            
            
            # percentile confidence intervals
            se = apply(boot.metrics,1,function(x) sqrt(var(x,na.rm=TRUE)))
            #upper<-apply(boot.metrics,1,quantile,(1+conf_lev)/2,na.rm=TRUE)
            #lower<-apply(boot.metrics,1,quantile,(1-conf_lev)/2,na.rm=TRUE)
            
            # compute reverse percentile intervals (so that interval estimates don't fall outside of interval)
            value = metrics$metrics$Value
            value[rows_to_transform] = mylogit(value[rows_to_transform])
            value[rows_to_transform_nonzero] = log(value[rows_to_transform_nonzero])
            t_val = qt((1+conf_lev)/2,n_boot-1)
            metrics$metrics$upper <- value + t_val*se
            metrics$metrics$lower <- value - t_val*se
            
            # back-transform
            metrics$metrics$upper[rows_to_transform] <- mylogistic(metrics$metrics$upper[rows_to_transform])
            metrics$metrics$lower[rows_to_transform] <- mylogistic(metrics$metrics$lower[rows_to_transform])
            metrics$metrics$upper[rows_to_transform_nonzero] <- exp(metrics$metrics$upper[rows_to_transform_nonzero])
            metrics$metrics$lower[rows_to_transform_nonzero] <- exp(metrics$metrics$lower[rows_to_transform_nonzero])
            
            # cap at 0
            metrics$metrics$lower <- pmax(metrics$metrics$lower,0)
            
            if(any(metrics$metrics$lower == 0) | any(metrics$metrics$upper == 0)) {
              #browser()
            }
          }
        })
        metrics$Title<-metrics$compare<-metrics$means_plot<-metrics$metrics.plot<-NULL
        
        if(n_comparison_facs==0) {
          #metrics$Title<-unique(apply(dat[,grouping_factors[-1]],1,paste,collapse=" "))
          metrics$Title <- 'Cell Concentration vs. Dilution Fraction'
        }
        
        
        if(n_comparison_facs %in% c(0,1)){
          #metrics$Title<-unique(apply(dat[,grouping_factors[-1][grouping_factors[-1]!=factor_to_compare]],1,paste,collapse=" "))
          metrics$Title <- 'Cell Concentration vs. Dilution Fraction'
          metrics$metrics$comp_factor<-metrics$metrics[,factor_to_compare]

          metrics_to_plot = c("R.squared",
                              "Sum.Squared.Error",
                              "Sum.Absolute.Error",
                              "Scaled.Sum.Squared.Error",
                              "Scaled.Sum.Absolute.Error",
                              "Smoothed.R-squared",
                              "Smoothed.Sum.Squared.Error",
                              "Smoothed.Scaled.Sum.Squared.Error",
                              "Smoothed.Sum.Absolute.Error",
                              "Smoothed.Scaled.Sum.Absolute.Error")
          
          metrics$metrics_to_plot = metrics_to_plot[as.numeric(input$perf.metrics)]
          metrics$metrics_to_plot = c("R.squared",metrics$metrics_to_plot)
          plot.metrics<- metrics$metrics[ metrics$metrics$Metric %in% metrics$metrics_to_plot,]
          
          metrics$metrics.plot<-ggplot(plot.metrics,aes(x=comp_factor,y=Value,color=comp_factor))+
            facet_wrap(~Metric,scales="free_y")+
            geom_point(size=2)+
            ylab("")+xlab("")+        
            theme_bw()+ 
            guides(color=FALSE)+
            theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=.5)) +
            theme(aspect.ratio=1)
          cv<- filter(metrics$metrics,substr(Metric,1,4)=="pool")
          cv$Metric<-as.numeric(gsub("pooled_cv_","",cv$Metric))
          metrics$cv_plot<-ggplot(cv,aes(y=Value,fill=comp_factor,x=Metric))+
            geom_col(position="dodge")+
            ylab("Pooled CV")+
            xlab("Dilution Fraction")+
            guides(fill=guide_legend(title = factor_to_compare))+
            theme_bw()
          
          means<-filter(metrics$metrics,substr(Metric,1,4)=="mean")
          means$Metric<-as.numeric(gsub("mean_conc_","",means$Metric))
          metrics$means_plot<-ggplot(means,aes(y=Value,fill=comp_factor,x=Metric))+
            geom_col(position="dodge")+
            ylab("Average Concentration")+
            xlab("Dilution Fraction")+
            guides(fill=guide_legend(title = factor_to_compare))+
            theme_bw()
          
          if(n_boot>20){
            metrics$metrics.plot<-metrics$metrics.plot+
              geom_pointrange(data=plot.metrics,
                              aes(x=comp_factor,y=Value,color=comp_factor,ymin=lower,ymax=upper))
            metrics$means_plot<- metrics$means_plot+
              geom_errorbar(data=means,
                            aes(x=Metric,ymax=upper,ymin=lower),
                            position="dodge")
            metrics$cv_plot<- metrics$cv_plot+
              geom_errorbar(data=cv,
                            aes(x=Metric,ymax=upper,ymin=lower),
                            position="dodge")
            
            # first column is value, next n_boot columns are samples
            boot.metrics<-cbind(metrics$metrics$Value,boot.metrics)
            n.met<-nrow(boot.metrics)/n_compare
            

            
            if(n_comparison_facs == 1){ 
              metrics$compare<-data.frame(Metric=NULL,level1=NULL,level2=NULL,Ratio=NULL,lower=NULL,upper=NULL)
              for(i in 1:(n_compare-1)){
                for(j in (i+1):n_compare){
                  t.comp<-boot.metrics[(0:(n.met-1))*n_compare+i,]/(boot.metrics[(0:(n.met-1))*n_compare+j,] + .0000001)
                  if(any(rowMeans(is.na(t.comp))>.1)) {

                    print("Bootstrap producing NA values")
                  } 
                  t.comp<-data.frame(metrics$metrics$Metric[(0:(n.met-1))*n_compare+i],
                                     level1=metrics$metrics$comp_factor[i],  ### First level in comparison
                                     level2=metrics$metrics$comp_factor[j],  ### divided by second level in comparison
                                     Ratio=t.comp[,1],
                                     t(apply(t.comp[,-1],1,quantile,c(.05,.95),na.rm=TRUE)))
                  names(t.comp)<-c("Metric","level1","level2","Ratio","lower","upper")
                  metrics$compare<-bind_rows(metrics$compare,t.comp)
                }
              }
            }
            
            
          }
        }
        
        
        metrics$dat<-dat
        metrics$smooth_df<-smooth_df
        metrics$n_boot<-n_boot
        metrics$conf_level <- conf_lev
        metrics$var_func <- var_func
        return(metrics)
      })
    }
  )
}

