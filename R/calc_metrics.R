calc.metrics<-function(dat,var_func,smooth_df,plot.bool=TRUE,factor_to_compare=NULL){
  
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
              n_conc=sum(!is.na(cell_conc)))   ### Number of replicate observations

  
  ### Compute a pooled coefficient of variation at each target dilution fraction
  pooled_cv<-rep_dat%>%group_by(counting_method,
                                target_dilution_fraction,
                                cell_type,
                                concentration_type)%>%
    summarise(pool_cv=sqrt(sum(((n_conc-1)*(var_conc + .000001))[n_conc>1])/sum(n_conc[n_conc>1]-1))/
                (sum(n_conc*mean_conc)/sum(n_conc)))
  
  
  if(any(is.na(pooled_cv$pool_cv))){
    pooled_cv2<-dat%>%group_by(
      counting_method,
      target_dilution_fraction,
      cell_type,
      concentration_type)%>%
      summarise(pool_cv=sd(cell_conc,na.rm=TRUE)/mean(cell_conc,na.rm=TRUE))
    pooled_cv$pool_cv[is.na(pooled_cv$pool_cv)]<-pooled_cv2$pool_cv[is.na(pooled_cv$pool_cv)]
  }
  
  
  pooled_cv<- spread(pooled_cv,target_dilution_fraction,pool_cv)
  name_sub<-grep("0|1",names(pooled_cv))
  names(pooled_cv)[name_sub]<-paste0("pooled_cv_",names(pooled_cv)[name_sub])
  
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
  
  
  rep_dat2 <- dat %>% 
    group_by(counting_method,
             cell_type,
             concentration_type,
             stock_solution,
             target_dilution_fraction,
             measured_dilution_fraction,
             stock_extraction,
             rep_obsv) %>%
    summarise(var_conc=var(cell_conc,na.rm=TRUE),   ### variance of replicate cell concentration measurements
              mean_conc=mean(cell_conc,na.rm=TRUE), ### mean cell concentration
              n_conc=sum(!is.na(cell_conc))) 
  
  rep_dat2 <- rep_dat2[!(is.na(rep_dat2$mean_conc)),]
  
  fits <- rep_dat2 %>%
    group_by(counting_method,
             cell_type,
             concentration_type)%>%
    do(prop_fit=prop_fitter(.,var_func),smooth_fit=smooth_fitter(.,var_func,smooth_df))
  
  mets<-fits%>%
    do(mets=metrics(.$prop_fit,.$smooth_fit))

  met.names<- unique(names(unlist(mets))) %>% 
    gsub("mets.","",.)%>%gsub(".1","",.)
  
  mets<-as.data.frame(matrix(unlist(mets),nrow=nrow(fits),byrow=TRUE))
  
  names(mets)<-met.names
  
  mets<-data.frame(
    select(fits,one_of(grouping_factors)),
    mets)%>%
    full_join(pooled_cv,by=grouping_factors)%>%
    full_join(means,by=grouping_factors)
  
  overview.plot<-overview.plot2<-NULL
  
  if(plot.bool){
    plot_data<-function(prop_fit,smooth_fit){
      p.res<-prop_fit$residuals
      p.fit<-prop_fit$fitted.values
      y<-prop_fit$model$y
      x<-prop_fit$model$x
      s.res<-smooth_fit$fitted.values-p.fit
      data.frame(response_type=rep(c("Mean Conc.","Raw Residuals","Smoothed Residuals"),each=length(y)),
                 dilution_fraction=rep(x,3),
                 y=c(y,p.res,s.res))
    }
    data.for.plot<-fits%>%
      do(mets=plot_data(.$prop_fit,.$smooth_fit))
    data.for.plot<-rbind.fill(data.for.plot[[1]])
    line_parms<-fits%>%do(as.data.frame(coef(.$prop_fit)))
    line_parms<-data.frame(slope=c(unlist(line_parms),rep(0,2*nrow(mets))),
                           response_type=rep(c("Mean Conc.","Raw Residuals","Smoothed Residuals"),each=nrow(mets)))
    
    if(!is.null(factor_to_compare)){
      data.for.plot$comp_level<-rep(mets[,factor_to_compare],3*table(rep_dat2[,factor_to_compare]))
      line_parms$comp_level<-rep(mets[,factor_to_compare],3)
    } 
    
    overview.plot<-ggplot(data=data.for.plot,aes(y=y,x=dilution_fraction))+
      geom_abline(data=line_parms,aes(slope=slope,intercept=0))+
      ylab("")+
      xlab("Dilution Fraction")+
      theme_bw()
    
    dat$comp_fac<-dat[,factor_to_compare]
    
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
      theme_bw()
    if(!is.null(factor_to_compare)){
      overview.plot<-overview.plot+
        facet_grid(response_type~comp_level,scales="free_y")+
        geom_point(aes(color=comp_level))+
        guides(color=FALSE)+
        geom_smooth(data=subset(data.for.plot,response_type == 'Mean Conc.'),aes(x=dilution_fraction,y=y),se=FALSE,size=.1)
      
      overview.plot2<-overview.plot2+
        facet_grid(comp_fac~.)
    }
    if(is.null(factor_to_compare)){
      overview.plot<-overview.plot+
        facet_grid(response_type~.,scales="free_y")+
        geom_point()+
        geom_smooth(data=subset(data.for.plot,response_type == 'Mean Conc.'),aes(x=dilution_fraction,y=y),se=FALSE,size=.1)
    }
      
  }
  
  
  list(metrics=mets%>%gather("Metric","Value",4:ncol(.)),
       overview.plot=overview.plot,
       overview.plot2=overview.plot2)
}