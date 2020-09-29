# options module

metricsUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    textInput(ns("na_lab"), label = h3("How are missing values denoted in the file?"), value = "NA"),
    actionButton(ns("help_na_lab"), "Help"),
    #hr(),
    
    selectInput(ns("var_func"), label = h3("Variance is ..."),
                choices = c('proportional to mean','constant','custom'), selected = "proportional to mean"),
    conditionalPanel(
      condition = "input.var_func == 'custom'",
      textInput(ns('cus_var_func'),label='Variance is proportional to',value='mn^2'), 
      ns = ns
    ),
    actionButton(ns("help_var_func"), "Help"),
    #hr(),

    
    numericInput(ns("smooth_df"), label = h3("Order of smoothing polynomial"), value = 4),
    #hr(),
    
    numericInput(ns("n_boot"), label = h3("Number of Bootstrap Iterations"), value = 15),
    actionButton(ns("help_n_boot"), "Help"),
    
    selectInput(ns("conf_lev"), label = h3("Confidence Level"), 
                choices = list("95%" = .95, "90%" = .9, "80%" = .8), 
                selected = .95),
    actionButton(ns("help_conf_lev"), "Help"),
    
    #hr(),
    checkboxGroupInput(ns("perf.metrics"), label = h3("Performance Metrics"), 
                       choices = list("R-squared"=1,
                                      "Scaled Sum of Squares"=2,
                                      "Sum of Squares"=3,
                                      "Scaled Absolute Value"=4,
                                      "Absolute Value"=5,
                                      "Smoothed R-squared"=6,
                                      "Smoothed Scaled Sum of Squares"=7,
                                      "Smoothed Sum of Squares"=8,
                                      "Smoothed Scaled Absolute Value"=9,
                                      "Smoothed Absolute Value" = 10),
                       selected = 1),
    actionButton(ns("goButton"), "Run Analysis")
  )

}

metricsServer <- function(id,input_file) {
  moduleServer(
    id,
    
    function(input,output,session) {
      
      observeEvent(input$help_na_lab, {
        showModal(modalDialog(
          title = "Help",
          paste("Please type how missing values are denoted in the input file.",
                "(If there are no missing values, you may leave the input as is.)")
        ))
      })
      
      observeEvent(input$help_var_func, {
        showModal(modalDialog(
          title = "Help",
          paste("")
        ))
      })
      
      observeEvent(input$help_smooth_df, {
        showModal(modalDialog(
          title = "Help",
          "Help text"
        ))
      })
      
      observeEvent(input$help_n_boot, {
        showModal(modalDialog(
          title = "Help",
          "Help text"
        ))
      })
      
      observeEvent(input$help_conf_lev, {
        showModal(modalDialog(
          title = "Help",
          "Help text"
        ))
      })
      
      Metrics<-eventReactive(input$goButton, {
        
        if(input$var_func == 'proportional to mean') {
          function_body = 'mn'
        
        } else if(input$var_func == 'constant') {
          function_body = '1'
          
        } else {
          function_body = input$custom_var_func
          
        }
        
        eval(parse(text=paste("var_func<-function(mn)",function_body))) 
        
        smooth_df<-input$smooth_df
        n_boot<-input$n_boot
        if(n_boot<20)n_boot<-0
        inFile <- input_file # input$file1
        conf_lev<-as.numeric(input$conf_lev)
        if (is.null(inFile))
          return(NULL)
        if(!is.null(inFile)){
          dat<-read.csv(inFile$datapath,na.strings=input$na_lab)
          dat$cell_conc<-as.numeric(as.character(dat$cell_conc))
          dat$target_dilution_fraction<-as.numeric(as.character(dat$target_dilution_fraction))
          dat$measured_dilution_fraction<-as.numeric(as.character(dat$measured_dilution_fraction))
        }
        dat<-dat%>%arrange(counting_method,
                           cell_type,
                           concentration_type,
                           stock_solution,
                           target_dilution_fraction,
                           measured_dilution_fraction,
                           stock_extraction)
        
        #### How many comparison factors are there in the dataset?
        grouping_factors<-c("counting_method","cell_type","concentration_type")
        n_compare<-nrow(distinct(dat[,grouping_factors]))
        n_levels<-apply(dat[,grouping_factors],2,function(x)length(unique(x)))
        n_comparison_facs<-sum(n_levels>1)
        
        #### If there's one comparison factor, identify it
        if(n_comparison_facs==1) factor_to_compare<-grouping_factors[n_levels>1]
        
        #### If there's no comparison factor, set to NULL 
        if(n_comparison_facs==0) factor_to_compare<-NULL
        
        #### If there's more than one comparison factor, shut off plots
        metrics<-calc.metrics(dat,var_func,smooth_df,plot.bool=n_comparison_facs<2,factor_to_compare)
        metrics$metrics$upper<-metrics$metrics$lower<-NULL
        if(n_boot>20){
          boot.metrics<-matrix(NA,nrow(metrics$metrics),n_boot)
          for(i in 1:n_boot){
            boot.ind<-nonpar.boot(dat,i)
            boot.dat<-dat[boot.ind[,1],]
            boot.dat$cell_conc<-dat$cell_conc[boot.ind[,2]]
            boot.dat$cell_conc[is.na(dat$cell_conc)]<-NA
            boot.dat$stock_extraction<-dat$stock_extraction
            boot.metrics[,i]<-unlist(calc.metrics(boot.dat,
                                                  var_func,
                                                  smooth_df,
                                                  plot.bool=FALSE,
                                                  factor_to_compare)$metrics$Value)
          }
          metrics$metrics$upper<-apply(boot.metrics,1,quantile,(1+conf_lev)/2)
          metrics$metrics$lower<-apply(boot.metrics,1,quantile,(1-conf_lev)/2)
        }
        metrics$Title<-metrics$compare<-metrics$means_plot<-metrics$metrics.plot<-NULL
        if(n_comparison_facs==0) metrics$Title<-unique(apply(dat[,grouping_factors[-1]],1,paste,collapse=" "))
        if(n_comparison_facs==1){
          metrics$Title<-unique(apply(dat[,grouping_factors[-1][grouping_factors[-1]!=factor_to_compare]],1,paste,collapse=" "))
          metrics$metrics$comp_factor<-metrics$metrics[,factor_to_compare]
          plot.metrics<- metrics$metrics[ metrics$metrics$Metric%in%c(
            "R.squared","Scaled.Sum.of.Squares",
            "Sum.of.Squares","Scaled.Absolute.Value",
            "Absolute.Value","Smoothed.R.squared","Smoothed.Scaled.Sum.of.Squares",
            "Smoothed.Sum.of.Squares","Smoothed.Scaled.Abs..Value",
            "Smoothed.Absolute.Value"),]
          metrics$metrics.plot<-ggplot(plot.metrics,aes(x=comp_factor,y=Value,color=comp_factor))+
            facet_wrap(~Metric,scales="free_y")+
            geom_point(size=2)+
            ylab("")+xlab("")+        
            theme_bw()+ 
            guides(color=FALSE)+
            theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=.5))
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
            boot.metrics<-cbind(metrics$metrics$Value,boot.metrics)
            n.met<-nrow(boot.metrics)/n_compare
            metrics$compare<-data.frame(Metric=NULL,level1=NULL,level2=NULL,Ratio=NULL,lower=NULL,upper=NULL)
            for(i in 1:(n_compare-1)){
              for(j in (i+1):n_compare){
                t.comp<-boot.metrics[(0:(n.met-1))*n_compare+i,]/
                  boot.metrics[(0:(n.met-1))*n_compare+j,]
                if(any(rowMeans(is.na(t.comp))>.1)) stop("Bootstrap producing many NA values")
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

