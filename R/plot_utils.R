void_plot = function(text) {
  ggplot() + 
    annotate("text", x = 4, y = 25, size=5, label = text) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) +
    theme(axis.title.y=element_blank(),
          axis.text.y =element_blank(),
          axis.ticks.y=element_blank())
}

raw_data_plot = function(metrics) {
  
  dat = metrics$dat
  
  p = ggplot(dat, aes(x=measured_dilution_fraction, y=cell_conc, col=counting_method )) + 
    geom_point(alpha=.5,size=1.8) +
    facet_wrap(~counting_method) + 
    ggtitle("Raw Data") +
    xlab("Dilution Fraction") +
    ylab("Cell Concentration (cells/mL)") +
    theme_bw() +
    labs(col='Counting Method') + 
    scale_y_continuous(labels = scales::scientific) +
    get_theme()
  
  return(p)
  
}

means_plot = function(metrics) {
  
  rep_dat = metrics$rep_dat
  line_parms = metrics$line_parms
  
  rep_dat$stock_solution = factor(rep_dat$stock_solution)
  line_parms$counting_method = line_parms$comp_level
  
  p = ggplot(data=rep_dat,aes(y=mean_conc,x=measured_dilution_fraction,
                              col=counting_method))+
    geom_point(size=1.8)+
    geom_abline(data=line_parms[line_parms$response_type == 'Mean Conc.',],aes(slope=slope,intercept=0))+
    facet_grid(.~counting_method,scales='free_y')+
    ylab(paste0("Mean Cell Concentration",'\n',"(cells/mL)"))+
    xlab("Dilution Fraction")+
    theme_bw()+
    ggtitle("Mean Cell Concentration vs. Dilution Fraction")+
    labs(col='Counting Method') +
    scale_y_continuous(labels = scales::scientific) +
    get_theme()
  
  return(p)
}

means_barplot = function(metrics) {
  
  dat = metrics$dat
  
  se_df = compute_se_df(dat)
  conf_lev = metrics$conf_level
  
  means = se_df %>%
    group_by(counting_method,target_dilution_fraction) %>% 
    summarise(pooled_mn = sum(mn*N/sum(N)),
              std_err_mn = sd(mn)/sqrt(n()),
              upper = pooled_mn + std_err_mn*qt(conf_lev,n()-1),
              lower = pooled_mn - std_err_mn*qt(conf_lev,n()-1))
  
  means$target_dilution_fraction = factor(means$target_dilution_fraction)
  
  p = ggplot(means,aes(y=pooled_mn,fill=counting_method,x=target_dilution_fraction))+
    geom_col(position="dodge")+
    geom_errorbar(data=means,
                  aes(x=target_dilution_fraction,ymax=upper,ymin=lower),
                  position="dodge")+
    ylab("Average Concentration")+
    xlab("Dilution Fraction")+
    labs(fill='Counting Method') +
    theme_bw() +
    get_theme() +
    scale_y_continuous(labels = scales::scientific)
  
  return(p)
}

prop_const_plot = function(metrics) {
  
  outdf <- metrics$metrics %>%
    dplyr::select(counting_method,cell_type,Metric,Value,lower,upper) %>%
    dplyr::filter(Metric == 'Prop.Const.x') %>%
    dplyr::select(-Metric)
  
  for(var in c('Value','lower','upper')) {
    outdf[,var] <- sapply(outdf[,var],round_or_truncate,3)
  }
  
  colnames(outdf) = c('countingMethod','cellType','propConst','lowerCL','upperCL')
  
  p = ggplot(outdf, aes_string(x='countingMethod',y='propConst')) + 
    geom_point() +
    geom_errorbar(aes_string(ymin='lowerCL',ymax='upperCL'),width=.2) +
    xlab("Method") +
    ylab("Proportionality Constant") +
    theme_bw() +
    get_theme() +
    scale_y_continuous(labels = scales::scientific)
  
  return(p)
  
}

residual_plot = function(metrics) {
  
  rep_dat = metrics$rep_dat
  line_parms = metrics$line_parms
  data.for.plot = metrics$data.for.plot
  
  residual_inds = grepl('resid',data.for.plot$response_type,ignore.case = TRUE)
  residual_inds_lp = grepl('resid',line_parms$response_type,ignore.case = TRUE)
  
  data.for.plot$response_type = factor(data.for.plot$response_type,
                                       levels= c("Mean Conc.","Raw Resids","Smth Resids",
                                                 "Scl. Smth Resids"))
  
  p = residual.plot<-ggplot(data=data.for.plot[residual_inds,],aes(y=y,x=dilution_fraction))+
    geom_abline(data=line_parms[residual_inds_lp,],aes(slope=slope,intercept=0))+
    ylab("Cell Concentration")+
    xlab("Dilution Fraction")+
    theme_bw() + 
    ggtitle("Residual Plot for Model Fits") + 
    get_theme() + 
    scale_y_continuous(labels = scales::scientific) + 
    geom_point(aes(color=comp_level),size=1.8) +
    facet_grid(rows = vars(response_type), cols=vars(comp_level)) +
    guides(color=FALSE) + 
    get_theme()
  
  return(p)
}

cv_barplot = function(metrics) {
  
  dat = metrics$dat
  
  se_df = compute_se_df(dat)
  
  cv = se_df %>%
    group_by(counting_method,target_dilution_fraction) %>% 
    summarise(pcv = 100*sum(cv*N/sum(N)),
              std_err_cv = 100*sd(cv)/sqrt(n()),
              upper = pcv + std_err_cv,
              lower = pcv - std_err_cv)
  
  cv$target_dilution_fraction = factor(cv$target_dilution_fraction)
  
  p = ggplot(cv,aes(y=pcv,fill=counting_method,x=target_dilution_fraction ))+
    geom_col(position="dodge")+
    #geom_errorbar(data=cv,
    #              aes(x=target_dilution_fraction,ymax=upper,ymin=lower),
    #              position="dodge")+
    ylab("Mean %CV")+
    xlab("Dilution Fraction")+
    labs(fill='Counting Method') +
    theme_bw() +
    get_theme()
  
  return(p)
  
}

pi_plot = function(metrics) {
  
  # not all of these are available for selection in the UI
  all_metrics = c("R.squared",
                  "Sum.Squared.Error",
                  "Sum.Absolute.Error",
                  "Scaled.Sum.Squared.Error",
                  "Scaled.Sum.Absolute.Error",
                  "Smoothed.R-squared",
                  "Smoothed.Sum.Squared.Error",
                  "Smoothed.Scaled.Sum.Squared.Error",
                  "Smoothed.Sum.Absolute.Error",
                  "Smoothed.Scaled.Sum.Absolute.Error",
                  'Variance.Stabilized.Smoothed.Sum.Squared.Error',
                  'Variance.Stabilized.Smoothed.Sum.Absolute.Error')
  
  all_metrics_names_pretty = c("R squared",
                               "Sum Squared Error",
                               "Sum Absolute Error",
                               "Scaled Sum Squared Error",
                               "Scaled Sum Absolute Error",
                               "Smoothed R-squared",
                               "Smoothed Sum Squared Error",
                               "Smoothed Scaled Sum Squared Error",
                               "Smoothed Sum Absolute Error",
                               "Smoothed Scaled Sum Absolute Error",
                               'Variance Stabilized Smoothed Sum Squared Error',
                               'Variance Stabilized Smoothed Sum Absolute Error')
  
  perf_metrics = metrics$perf_metrics
  
  metrics_to_plot = all_metrics[as.numeric(perf_metrics)]
  metrics_to_plot_pretty = all_metrics_names_pretty[as.numeric(perf_metrics)]
  
  plot.metrics <- metrics$metrics[ metrics$metrics$Metric %in% metrics_to_plot,]
  
  p = ggplot(plot.metrics,aes(x=comp_factor,y=Value,color=comp_factor))+
    facet_wrap(~Metric,scales="free_y")+
    geom_point(size=2)+
    ylab("Proportionality Index")+xlab("")+        
    theme_bw()+ 
    guides(color=FALSE)+
    theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=.5)) +
    theme(aspect.ratio=.75) +
    get_theme() +
    geom_pointrange(data=plot.metrics,
                    aes(x=comp_factor,y=Value,color=comp_factor,ymin=lower,ymax=upper))
  
  return(p)
  
}

r2_plot = function(metrics) {
  
  plot.metrics.r2 <- metrics$metrics[metrics$metrics$Metric == 'R.squared',]
  
  p  = ggplot(plot.metrics.r2,aes(x=comp_factor,y=Value,color=comp_factor))+
    facet_wrap(~Metric,scales="free_y")+
    geom_point(size=2)+
    ylab("R Squared")+xlab("")+        
    theme_bw()+ 
    get_theme() +
    guides(color=FALSE)+
    theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=.5)) +
    theme(aspect.ratio=1) +
    geom_pointrange(data=plot.metrics.r2,
                    aes(x=comp_factor,y=Value,color=comp_factor,ymin=lower,ymax=upper))
  
  return(p)
  
}

empirical_cdf_plot = function(metrics) {
  
  data = metrics$dat
  
  if(is.null(data$percent_viable_cells)) {
    return(NULL)
    
  } else if (sum(!is.na(data$percent_viable_cells) ) < 2) {
    return(NULL)
  }
  
  good_inds = !is.na(data$percent_viable_cells)
  
  data = data[good_inds,]
  
  data <- data %>%
    group_by(counting_method) %>%
    summarise(via_abs_resid = abs(percent_viable_cells - median(percent_viable_cells)) )
  
  data$counting_method = factor(data$counting_method)
  
  ggplot(data, aes(x=via_abs_resid,color=counting_method)) + 
    stat_ecdf() +
    xlab("Percent Viable (absolute residuals)") +
    ylab("Fraction of Data < x") +
    labs(color='Counting Method') +
    theme_bw() +
    theme(legend.position = "none") +
    get_theme()
  
  
}

viability_resids_hist_plot = function(metrics) {
  
  data = metrics$dat
  
  if(is.null(data$percent_viable_cells)) {
    return(NULL)
    
  } else if (sum(!is.na(data$percent_viable_cells) ) < 2) {
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
    ylab("Proportion of Data") +
    labs(fill='Counting Method') +
    theme_bw() + 
    get_theme()
  
}

viability_hist_plot = function(metrics) {
  
  data = metrics$dat
  
  if(is.null(data$percent_viable_cells)) {
    print("No viablity data.")
    return(NULL)
    
  } else if (sum(!is.na(data$percent_viable_cells) ) < 2) {
    print("Not enough viability data.")
    return(NULL)
  }
  
  good_inds = !is.na(data$percent_viable_cell)
  
  p = ggplot(data[good_inds,],aes(x=percent_viable_cells,fill=counting_method)) + 
    geom_density(alpha=.3) + 
    geom_histogram(aes(y=stat(count)/sum(count)),position='dodge') +
    theme_bw() +
    labs(fill='Counting Method') +
    get_theme() +
    ylab("Proportion of Results")
  
  return(p)
  
}

via_over_time_plot = function(metrics) {
  
  data = metrics$dat
  
  if(is.null(data$percent_viable_cells)) {
    return(NULL)
  } else if(sum(!is.na(data$time_elapsed)) < 2) {
    return(NULL)
  } else if(sum(!is.na(data$percent_viable_cells)) < 2) {
    return(NULL)
  }
  
  good_inds = !is.na(data$percent_viable_cell)
  
  p = ggplot(data[good_inds,],aes(x=time_elapsed, y=percent_viable_cells, color=counting_method)) + 
    geom_point() + 
    geom_smooth(method='lm',se=FALSE) +
    theme_bw() +
    get_theme() +
    xlab("Time Elapsed") +
    ylab("% Viable") + 
    labs(color='Counting Method') 
  
  if(any(is.na(data$stock_solution))) {
    return(p)
    
  } else if(length(unique(data$stock_solution)) == 1) {
    return(p)
    
  } else {
    return(p + facet_wrap(~stock_solution))
  }
  
}

get_theme = function() {
  return(
    theme(axis.text.y = element_text(size = 13),
          axis.text.x = element_text(size = 13),
          plot.title = element_text(hjust = 0.5,size = 17),
          axis.title = element_text(size = 17),
          legend.text = element_text(size = 13),
          legend.title = element_text(size = 15),
          strip.text.x = element_text(size = 10),
          strip.text.y = element_text(size = 10))
  )
}

pipette_error_plot = function(metrics) {
  
  if(is.null(metrics$dat)) {
    return(NULL)
  }
  
  dat = metrics$dat
  
  if(sum(!is.na(dat$time_elapsed)) < 2) {
    return(NULL)
  }
  
  if(!metrics$mdf_exists) {
    return(NULL)
  }
  
  data = dat %>% 
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
    theme_bw() +
    get_theme()
}

experimental_integrity_plot = function(metrics,which_method='All') {
  
  dat = metrics$dat
  
  # if no time elapsed, return null
  if(sum(!is.na(dat$time_elapsed)) < 2) {
    return(NULL)
  }
  
  dat$preds = 0
  cms = unique(dat$counting_method)
  
  if(which_method != 'All') {
    cms = which_method 
    dat = dat[dat$counting_method == which_method,]
  }
  
  pcs = metrics$metrics
  pcs = pcs[pcs$Metric == 'Prop.Const.x',]
  
  for(ii in 1:length(cms)) {
    
    t_cm = cms[ii] # counting method
    t_pc = pcs$Value[pcs$counting_method == t_cm] #prop const
    t_inds = dat$counting_method == t_cm
    dat$preds[t_inds] = dat$target_dilution_fraction[t_inds]*t_pc # predicted value
    
  }
  
  if(sum(!is.na(dat$analyst)) > 2) {
    
    p = ggplot(dat,aes(x=time_elapsed,y=(cell_conc-preds)/preds,
                       color=as.factor(rep_obsv),
                       shape=as.factor(analyst),
                       size=target_dilution_fraction))
    
    p = p + guides(shape=guide_legend(title = "Analyst"))
    
  } else {
    
    p = ggplot(dat,aes(x=time_elapsed,y=(cell_conc-preds)/preds,
                       color=as.factor(rep_obsv),
                       size=target_dilution_fraction))
  }
  
  p = p + 
    geom_point(alpha=.7)+
    scale_size_continuous(range = c(2,6)) +
    geom_hline(yintercept=0)+
    xlab("Time Elapsed")+
    ylab("Scaled Difference from \nExpected Concentration") +
    guides(color=guide_legend(title = "Obs. Rep.")) +
    theme_bw()+
    ggtitle("Sample Integrity Over Time") +
    get_theme() +
    scale_size_continuous(breaks=unique(dat$target_dilution_fraction)) +
    facet_grid(counting_method~.)
  
  
  return(p)
}

discrimination_bands_plot = function(metrics) {
  
  if(is.null(metrics$df_for_poly)) {
    
    text = paste("\n  Plot cannot be computed. \n",
                 "(Perhaps prediction intervals are non-monotonic?)")
    
    return(void_plot(text))
    
  } 
  
  preds = metrics$df_for_poly
  good_inds = !is.nan(preds[,'lwr'])
  
  # plot prediction intervals for debugging:
  #ggplot(preds,aes(x=x,y=y,col=comp_level)) + geom_line() +
  #  geom_line(aes(y=upr)) + geom_line(aes(y=lwr))
  
  if(length(good_inds) == 0) {
    text = paste("\n  Prediction Intervals All NaN. \n",
                 "(Perhaps only 1 replicate sample per Target DF?)")
    return(void_plot(text))
  }
  
  preds = preds[good_inds,]
  cms <- unique(preds$comp_level)
  
  preds$top <- 0
  preds$bottom <- 0
  
  for(m in 1:length(cms)) {
    
    sub_preds = preds[preds$comp_level == cms[m],]
    
    upper <- approxfun(x=make_monotonic(sub_preds$lwr),y=sub_preds$x, ties=max)
    lower <- approxfun(x=sub_preds$upr,y=sub_preds$x, ties=max)
    
    preds$top[preds$comp_level == cms[m]] <- upper(sub_preds$y)
    preds$bottom[preds$comp_level == cms[m]] <- lower(sub_preds$y)
    
  }
  
  p = ggplot(preds) + 
    geom_line(aes(x=x,y=bottom,color=comp_level),size=.75) +
    geom_line(aes(x=x,y=top,color=comp_level),size=.75) +
    geom_abline(slope=1,intercept=0,linetype='dashed') +
    ylab('Dilution Fraction Range') +
    xlab('Input Sample Dilution Fraction') + 
    labs(color='Counting Method') +
    ggtitle("Discrimination Bands") + 
    theme_bw() +
    get_theme()
  
  return(p)
  
}
