calc.metrics<-function(dat,var_func,smooth_df,plot.bool=TRUE,factor_to_compare=NULL,
                       log_scale){
  
  ### Average across within-sample replicate counts  
  rep_dat <- dat %>% 
    group_by(counting_method,
             cell_type,
             concentration_type,
             stock_solution,
             target_dilution_fraction,
             measured_dilution_fraction,
             stock_extraction) %>%
    summarise(var_conc=var(cell_conc,na.rm=TRUE),   ### variance of replicate cell concentration measurements
              mean_conc=mean(cell_conc,na.rm=TRUE), ### mean cell concentration
              n_conc=sum(!is.na(cell_conc))) ### Number of replicate observations
    

  ### Compute a pooled coefficient of variation at each target dilution fraction
  CV<-rep_dat%>%group_by(counting_method,
                                target_dilution_fraction,
                                cell_type,
                                concentration_type)%>%
    summarise(mean_cv = mean(sqrt(var_conc)/(mean_conc + .00000001)))
    #summarise(mean_cv=sqrt(sum(((n_conc-1)*(var_conc + .000001))[n_conc>1])/(sum(n_conc[n_conc>1])-1))/(sum(n_conc*mean_conc)/sum(n_conc)))
  
  
  if(any(is.na(CV$mean_cv))){
    CV2<-dat%>%group_by(
      counting_method,
      target_dilution_fraction,
      cell_type,
      concentration_type)%>%
      summarise(mean_cv=sd(cell_conc,na.rm=TRUE)/mean(cell_conc,na.rm=TRUE))
    CV$mean_cv[is.na(CV$mean_cv)]<-CV2$mean_cv[is.na(CV$mean_cv)]
  }
  
  
  CV<- spread(CV,target_dilution_fraction,mean_cv)
  name_sub<-grep("0|1",names(CV))
  names(CV)[name_sub]<-paste0("mean_CV_",names(CV)[name_sub])
  
  ### Compute the mean concentration at each target dilution fraction
  means <- dat %>% group_by(
    counting_method,
    target_dilution_fraction,
    cell_type,
    concentration_type)%>%
    summarise(mean_conc=mean(cell_conc,na.rm=TRUE))%>%
    spread(target_dilution_fraction,mean_conc)
  name_sub<-grep("0|1",names(means))
  names(means)[name_sub]<-paste0("mean_conc_",names(means)[name_sub])
  
  ### Function computing performance metrics
  grouping_factors<-c("counting_method","cell_type","concentration_type")
  
  fits <- rep_dat %>%
    group_by(counting_method,
             cell_type,
             concentration_type)%>%
    do(prop_fit=prop_fitter(.,var_func,log_scale),smooth_fit=smooth_fitter(.,var_func,smooth_df,log_scale))
  
  mets<-fits%>%
    do(mets=metrics(.$prop_fit,.$smooth_fit))

  met.names<- unique(names(unlist(mets))) %>% 
    gsub("mets.","",.)%>%gsub(".1","",.)
  
  mets<-as.data.frame(matrix(unlist(mets),nrow=nrow(fits),byrow=TRUE))
  
  names(mets)<-met.names
  
  mets<-data.frame(
    select(fits,one_of(grouping_factors)),
    mets)%>%
    full_join(CV,by=grouping_factors)%>%
    full_join(means,by=grouping_factors)
  
  overview.plot = NULL
  overview.plot2 = NULL
  residual.plot = NULL
  df_for_poly = NULL
  
  if(plot.bool){
    
    plot_data<-function(prop_fit,smooth_fit){
      p.res<-prop_fit$residuals
      p.fit<-prop_fit$fitted.values
      y<-prop_fit$model$y
      x<-prop_fit$model$x
      s.res<-smooth_fit$fitted.values-p.fit
      
      data.frame(response_type=rep(c("Mean Conc.","Raw Resids","Smth Resids",
                                     "Scl, Smth Resids"),each=length(y)),
                 dilution_fraction=rep(x,4),
                 y=c(y,p.res,s.res,s.res/sqrt(abs(x) )))
    }
    
    data.for.plot<-fits%>%
      do(mets=plot_data(.$prop_fit,.$smooth_fit))
    
    data.for.plot<-rbind.fill(data.for.plot[[1]])
    line_parms<-fits%>%do(as.data.frame(coef(.$prop_fit)))
    
    line_parms<-data.frame(slope=c(unlist(line_parms),rep(0,3*nrow(mets))),
                           response_type=rep(c("Mean Conc.","Raw Resids","Smth Resids",
                                               "Scl, Smth Resids"),
                                             each=nrow(mets)))
    
    line_parms$response_type = factor(line_parms$response_type,
                                      levels=c("Mean Conc.","Raw Resids","Smth Resids",
                                               "Scl, Smth Resids"))
    
    if(!is.null(factor_to_compare)){
      data.for.plot$comp_level<-rep(mets[,factor_to_compare],4*table(rep_dat[,factor_to_compare]))
      line_parms$comp_level<-rep(mets[,factor_to_compare],4)
    } 

    
    # mean plot
    rep_dat$stock_solution = factor(rep_dat$stock_solution)
    line_parms$counting_method = line_parms$comp_level
    
    overview.plot<-ggplot(data=rep_dat,aes(y=mean_conc,x=measured_dilution_fraction,
                                           col=counting_method))+
      geom_point()+
      geom_abline(data=line_parms[line_parms$response_type == 'Mean Conc.',],aes(slope=slope,intercept=0))+
      facet_grid(.~counting_method,scales='free_y')+
      ylab("Mean Cell Concentration (cells/mL)")+
      xlab("Dilution Fraction")+
      theme_bw()+
      ggtitle("Mean Cell Concentration vs. Dilution Fraction")+
      theme(plot.title = element_text(hjust = 0.5,size=15))+
      labs(col='Counting Method')
    
    
    # residual plot
    residual_inds = grepl('resid',data.for.plot$response_type,ignore.case = TRUE)
    residual_inds_lp = grepl('resid',line_parms$response_type,ignore.case = TRUE)

    data.for.plot$response_type = factor(data.for.plot$response_type,
                                         levels= c("Mean Conc.","Raw Resids","Smth Resids",
                                         "Scl, Smth Resids"))
    
    residual.plot<-ggplot(data=data.for.plot[residual_inds,],aes(y=y,x=dilution_fraction))+
      geom_abline(data=line_parms[residual_inds_lp,],aes(slope=slope,intercept=0))+
      ylab("Cell Concentration")+
      xlab("Dilution Fraction")+
      theme_bw() + 
      ggtitle("Residual Plot for Model Fits")+
      theme(plot.title = element_text(hjust = 0.5, size=15)) 
    
    dat$comp_fac<-dat[,factor_to_compare]
    
    # time vs difference plot
    overview.plot2<-ggplot(dat,aes(x=time_elapsed,y=cell_conc-starting_soln_conc*target_dilution_fraction,
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
    
    if(!is.null(factor_to_compare)){
      # polynomial fit with prediction intervals
      xlowerlim = min(dat$target_dilution_fraction)
      xupperlim = max(dat$target_dilution_fraction)
      npreds = 100
      
      if(log_scale) {
        x = exp(seq(log(xlowerlim),log(xupperlim),length.out = npreds))
        
      } else {
        x = seq(xlowerlim,xupperlim,length.out = npreds)
      }
      
      remove(xlowerlim,xupperlim)
      methods = unique(data.for.plot$comp_level)
      
      df_for_poly = data.frame(x = rep(x,length(methods)), 
                               y = 0,
                               lwr = 0,
                               upr = 0,
                               comp_level = factor(rep(methods,each=npreds)))
      
      for(m in 1:length(methods)) {
        
        mod = fits$smooth_fit[fits$counting_method == methods[m]][[1]] # get the appropriate fitted polynomial model
        
        if(log_scale) {
          X = poly(log(x),smooth_df,raw=TRUE)
          colnames(X) = paste('X',1:ncol(X),sep='') # names need to agree with model coefficient names
          pred_and_fit = exp(predict(mod,X,interval='prediction',weights=var_func(x)))
          
        } else {
          X = poly(x,smooth_df,raw=TRUE)
          colnames(X) = paste('X',1:ncol(X),sep='') # names need to agree with model coefficient names
          pred_and_fit = predict(mod,X,interval='prediction',weights=1/var_func(x))
        }
        
        df_for_poly$y[df_for_poly$comp_level == methods[m]] <- pred_and_fit[,'fit']
        df_for_poly$lwr[df_for_poly$comp_level == methods[m]] <- pmax(pred_and_fit[,'lwr'],rep(1e-10,length(pred_and_fit[,'lwr'])))
        df_for_poly$upr[df_for_poly$comp_level == methods[m]] <- pred_and_fit[,'upr']

      }
      
      
      residual.plot <- residual.plot + 
        geom_point(aes(color=comp_level)) +
        facet_grid(rows = vars(response_type), cols=vars(comp_level)) +
        guides(color=FALSE)
        
      
      overview.plot2<-overview.plot2+
        facet_grid(comp_fac~.)
    }
    if(is.null(factor_to_compare)){
      overview.plot<-overview.plot+
        #facet_grid(response_type~.,scales="free_y")+
        geom_point()
      
      residual.plot <- residual.plot +
        facet_grid(response_type~.,scales="free_y")+
        geom_point() 
    }
      
  }
  
  list(metrics=mets%>%gather("Metric","Value",4:ncol(.)),
       overview.plot=overview.plot,
       df_for_poly=df_for_poly,
       overview.plot2=overview.plot2,
       residual.plot = residual.plot,
       prediction.ints = df_for_poly)
}