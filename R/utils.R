
# pre-processing functions
process_colnames = function(dat) {
  
  expected_colnames = get_expected_colnames()
  
  optional_colnames = c('measured_dilution_fraction',
                        'raw_count',
                        'percent_viable_cells',
                        'time_elapsed',
                        'cell_type',
                        'concentration_type',
                        'stock_solution',
                        'starting_soln_conc',
                        'stock_extraction',
                        'replicate_sample',
                        'analyst')
  
  for(ii in 1:length(optional_colnames)) {
    if( is.null(dat[[ optional_colnames[ii] ]]) ) {
      
      dat[[ optional_colnames[ii] ]] = NA
      
    }
  }
  
  return(dat)
  
}

get_expected_colnames = function() {
  
  expected_colnames = c('counting_method',
                        'target_dilution_fraction',
                        'random_sample_number',
                        'replicate_sample',
                        'rep_obsv',
                        'cell_conc')
  
  return(expected_colnames)
  
}

remove_useless_rows = function(dat) {
  
  # missing counting methods or concentration or extra ghost rows
  
  dat <- dat[dat$counting_method != '',]
  dat <- dat[!is.na(dat$counting_method),]
  dat <- dat[!is.na(dat$cell_conc),]
  dat$cell_conc <- as.numeric(as.character(dat$cell_conc))
  dat$target_dilution_fraction <- as.numeric(as.character(dat$target_dilution_fraction))
  
  return(dat)
  
}

handle_measured_dilution_fraction = function(dat) {
  
  # handle measured/target DF
  mdf = dat$measured_dilution_fraction
  
  
  if(all(is.na(mdf)) || is.null(mdf) ) {
    dat$measured_dilution_fraction <- as.numeric(as.character(dat$target_dilution_fraction))
    mdf_exists = FALSE
    
  } else {
    dat$measured_dilution_fraction <- as.numeric(as.character(dat$measured_dilution_fraction))
    mdf_exists = TRUE
    missing_inds = which(is.na(dat$measured_dilution_fraction))
    dat$measured_dilution_fraction[missing_inds] = dat$target_dilution_fraction[missing_inds] 
  }
  
  return(list(
    dat=dat,
    mdf_exists=mdf_exists
  ))
  
  
}

check_inputs = function(dataset,
                        n_boot,
                        seed,
                        var_func,
                        smooth_df,
                        perf_metrics,
                        conf_lev) {
  
  
  # n_boot
  if(length(n_boot) != 1) {
    stop("Invalid n_bood argument. Must be integer and bewteen 50 and 2000 (inclusive).")
  }
  
  if(any(!is.numeric(n_boot), n_boot < 50, n_boot > 2000, n_boot %% 1 != 0)) {
    stop("Invalid argument for n_boot. Must be integer and bewteen 50 and 2000 (inclusive).")
  }
  
  # seed
  if(length(seed) != 1) {
    stop("Invalid seed argument")
  }
  
  if(any(!is.numeric(seed),seed < 1, seed %% 1 !=0)) {
    stop("Seed must be positive integer.")
  }
  
  # var_func
  if(length(var_func) < 1) {
    stop("No var_func selected.")
  }
  
  if(!(var_func %in% c(1,2,3))) {
    stop("Invalid choice for var_func argument.")
  } 
  
  # perf_metrics
  if(length(perf_metrics) < 1) {
    stop("No PI metrics selected.")
  }
  
  if(!all(perf_metrics %in% 7:12)) {
    stop("PI metrics must be integers from 7 to 12 (inclusive).")
  }
  
  
  # dataset
  dat = dataset
  
  if(nrow(dat) <= 1) {
    stop("Not enough rows detected in dataset.")
  }
  
  # need required columns
  needed_colnames = c('counting_method',
                      'random_sample_number',
                      'target_dilution_fraction',
                      'rep_obsv',
                      'cell_conc')
  
  for(ii in 1:length(needed_colnames)) {
    if(!(needed_colnames[ii] %in% colnames(dat))) {
      stop(paste("No column named",needed_colnames[ii],"detected in dataset."))
    }
  }
  
  # remove NAs just in case
  dat = dat[!is.na(dat$cell_conc),]
  
  # some columns must be numeric
  numeric_columns = c('cell_conc',
                      'random_sample_number',
                      'rep_obsv',
                      'target_dilution_fraction')
  
  for(ii in 1:length(numeric_columns)) {
    
    if(!is.numeric(dat[,numeric_columns[ii]])) {
      stop(paste("Non-numeric values detected in column",numeric_columns[ii]))
    }
    
  }
  
  
  if(length(unique(dat$target_dilution_fraction)) >= 15) {
    stop("Too many unique target dilution fractions.")
  }  
  
  if(length(unique(dat$concentration_type)) > 1) {
    stop("Application can only analyze 1 concentration type. Please split rows with different concentration types into seperate .csv files.")
  }
  
  if(length(unique(dat$cell_type)) > 1) {
    stop("Application can only analyze 1 cell type. Please split rows with different concentration types into seperate .csv files.")
  }
  
  # smooth_df
  if(any(smooth_df < 0, smooth_df %% 1 != 0, smooth_df >= length(unique(dat$target_dilution_fraction)))) {
    stop("Invalid choice for degree of flexible model. Must be an integer 0 up to the number of unique target dilutions fractions, minus 1.")
  }
  
}

check_stock_ext_and_rep_samp = function(dat) {
  
  # use rsn for stock_extraction if stock_extraction missing
  if(sum(!is.na(dat$stock_extraction)) < 3) {
    dat$stock_extraction = dat$random_sample_number
  }
  
  
  # create replicate sample if missing
  if(any(is.na(dat$replicate_sample))) {
    
    cms = unique(dat$counting_method)
    tdfs = unique(dat$target_dilution_fraction)
    
    for(ii in 1:length(cms)) {
      for(jj in 1:length(tdfs)) {
        
        inds = dat$counting_method == cms[ii] & dat$target_dilution_fraction == tdfs[jj]
        
        if(length(inds) < 1) {
          next
        }
        
        dat$replicate_sample[inds] = as.integer(factor(dat$random_sample_number[inds]))
        
      }
    }
  }
  
  return(dat)
  
}

check_experimental_design = function(dat) {
  
  
  # there needs to exist at least 4 DFs
  # with at least 3 replicate samples
  # with at least 3 replicate observations.
  
  # it is OK for subset to not meet the criteria,
  # we just need SOME subset of the data to meet the criteria
  
  test_dat = dat %>%
    group_by(counting_method,replicate_sample,target_dilution_fraction) %>% 
    summarise(rep_obs_check = (n() >= 3) )
  
  # only keep the sufficient rep samples
  test_dat = test_dat %>% 
    filter(rep_obs_check)
  
  test_dat = test_dat %>%
    group_by(counting_method,target_dilution_fraction) %>% 
    summarise(rep_samp_check = ( n() >= 3 ))
  
  test_dat = test_dat %>%
    filter(rep_samp_check)
  
  # need at least 4 dfs that have 'survived' the chain above
  test_dat = test_dat %>%
    group_by(counting_method) %>%
    summarise(df_check = (n() >= 4))
  
  if(!all(test_dat$df_check)) {
    return(TRUE)
  } 
  
  return(FALSE)
  
}


# computing metrics
prop_fitter = function(data,var_func){
  y=unlist(data$mean_conc)
  x<-unlist(data$measured_dilution_fraction)
  x[is.na(x)]<-unlist(data$target_dilution_fraction)[is.na(x)]
  fit.dat<-data.frame(y=y,x=x)
  
  mod = lm(y~x-1,w=unlist(data$n_conc)/var_func(x),data=fit.dat)
  
  return(mod)
  
}

compute_cvs = function(dat,rep_dat,cv_estimation=1) {
  
  # unweighted cv estimation -- follows iso
  if(cv_estimation == 1) {
    CV<-rep_dat %>% group_by(counting_method,
                             target_dilution_fraction,
                             cell_type,
                             concentration_type)%>%
      summarise(mean_cv = sum((n_conc*sqrt(var_conc)/(mean_conc + .00000001)))/sum(n_conc) )
    
    # weighted cv estimation -- marginally better in unweighted samples but doesn't follow iso
  } else if(cv_estimation == 2) {
    
    CV<-rep_dat%>%group_by(counting_method,
                           target_dilution_fraction,
                           cell_type,
                           concentration_type)%>%
      summarise(mean_cv=sqrt(sum(((n_conc/sum(n_conc))*(var_conc))))/(sum(n_conc*mean_conc)/sum(n_conc)))
    
  } 
  
  
  
  if(any(is.na(CV$mean_cv))){
    
    CV2<-dat%>%group_by(
      counting_method,
      target_dilution_fraction,
      cell_type,
      concentration_type)%>%
      summarise(mean_cv=sd(cell_conc,na.rm=TRUE)/mean(cell_conc,na.rm=TRUE))
    
    CV$mean_cv[is.na(CV$mean_cv)]<-CV2$mean_cv[is.na(CV$mean_cv)]
  }
  
  
  CV <- spread(CV,target_dilution_fraction,mean_cv)
  name_sub <- grep("0|1",names(CV))
  names(CV)[name_sub] <- paste0("mean_CV_",names(CV)[name_sub])
  
  return(CV)
  
}

compute_means = function(dat) { 
  ### Compute the mean concentration at each target dilution fraction
  means <- dat %>% group_by(
    counting_method,
    target_dilution_fraction,
    cell_type,
    concentration_type)%>%
    summarise(mean_conc=mean(cell_conc,na.rm=TRUE)) %>%
    spread(target_dilution_fraction,mean_conc)
  
  name_sub<-grep("0|1",names(means))
  names(means)[name_sub]<-paste0("mean_conc_",names(means)[name_sub])
  
  return(means)
}

compute_df_for_poly = function(dat,data.for.plot,fits,smooth_df,var_func) {
  
  # polynomial fit with prediction intervals
  xlowerlim = 0
  xupperlim = 1
  npreds = 100
  
  x = seq(xlowerlim,xupperlim,length.out = npreds)

  
  remove(xlowerlim,xupperlim)
  methods = unique(data.for.plot$comp_level)
  
  df_for_poly = data.frame(x = rep(x,length(methods)), 
                           y = 0,
                           lwr = 0,
                           upr = 0,
                           comp_level = factor(rep(methods,each=npreds)))
  
  for(m in 1:length(methods)) {
    
    mod = fits$smooth_fit[fits$counting_method == methods[m]][[1]] # get the appropriate fitted polynomial model
    

    X = poly(x,smooth_df,raw=TRUE)
    colnames(X) = paste('X',1:ncol(X),sep='') # names need to agree with model coefficient names
    pred_and_fit = predict(mod,X,interval='prediction',weights=1/var_func(x))

    
    df_for_poly$y[df_for_poly$comp_level == methods[m]] <- pmax(pred_and_fit[,'fit'],rep(0,length(pred_and_fit[,'fit'])))
    df_for_poly$lwr[df_for_poly$comp_level == methods[m]] <- pmax(pred_and_fit[,'lwr'],rep(0,length(pred_and_fit[,'lwr'])))
    df_for_poly$upr[df_for_poly$comp_level == methods[m]] <- pred_and_fit[,'upr']
    
  }
  
  return(df_for_poly)
  
}

compute_se_df = function(dat) {
  
  se_df = dat %>% 
    group_by(counting_method,target_dilution_fraction,replicate_sample) %>% 
    summarise(std_dev = sd(cell_conc),
              mn = mean(cell_conc),
              cv = std_dev/mn,
              N = n()) 
  
  return(se_df)
  
}

calc.metrics = function(dat,var_func,smooth_df,cv_estimation=1,
                        for_bootstrap=F) {
  
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
  

  CV = compute_cvs(dat,rep_dat)
  means = compute_means(dat)


  ### Function computing performance metrics
  grouping_factors <- c("counting_method","cell_type","concentration_type")
  
  fits <- rep_dat %>%
    group_by(counting_method,
             cell_type,
             concentration_type)%>%
    do(prop_fit=prop_fitter(.,var_func),smooth_fit=smooth_fitter(.,var_func,smooth_df))
  
  
  # metrics
  mets <- fits%>%
    do(mets=compute_metrics(.$prop_fit,.$smooth_fit))
  
  met.names <- unique(names(unlist(mets))) %>% 
    gsub("mets.","",.)%>%gsub(".1","",.)
  
  mets <- as.data.frame(matrix(unlist(mets),nrow=nrow(fits),byrow=TRUE))
  
  names(mets) <- met.names
  
  mets <- data.frame(
    select(fits,one_of(grouping_factors)),
    mets)%>%
    full_join(CV,by=grouping_factors)%>%
    full_join(means,by=grouping_factors)
  
  if(!for_bootstrap) {
    # line parms for plots
    line_parms <- fits %>% do(as.data.frame(coef(.$prop_fit)))
    
    line_parms <- data.frame(slope=c(unlist(line_parms),rep(0,3*nrow(mets))),
                             response_type=rep(c("Mean Conc.","Raw Resids","Smth Resids",
                                                 "Scl. Smth Resids"),
                                               each=nrow(mets)))
    
    line_parms$response_type = factor(line_parms$response_type,
                                      levels=c("Mean Conc.","Raw Resids","Smth Resids",
                                               "Scl. Smth Resids"))
    
    
    plot_data = function(prop_fit,smooth_fit) {
      p.res<-prop_fit$residuals
      p.fit<-prop_fit$fitted.values
      y<-prop_fit$model$y
      x<-prop_fit$model$x
      s.res<-smooth_fit$fitted.values-p.fit
      
      data.frame(response_type=rep(c("Mean Conc.","Raw Resids","Smth Resids",
                                     "Scl. Smth Resids"),each=length(y)),
                 dilution_fraction=rep(x,4),
                 y=c(y,p.res,s.res,s.res/sqrt(abs(x) )))
    }
    
    data.for.plot <- fits %>%
      do(mets=plot_data(.$prop_fit,.$smooth_fit))
    
    data.for.plot <- plyr::rbind.fill(data.for.plot[[1]])
    
    factor_to_compare = 'counting_method'
    data.for.plot$comp_level <- rep(mets[,factor_to_compare],4*table(rep_dat[,factor_to_compare]))
    line_parms$comp_level <- rep(mets[,factor_to_compare],4)
    dat$comp_fac <- dat[,'counting_method']
    
    df_for_poly = compute_df_for_poly(dat,data.for.plot,fits,smooth_df,var_func)
    
    return(
      list(metrics=mets%>%gather("Metric","Value",4:ncol(.)),
           line_parms=line_parms,
           data.for.plot=data.for.plot,
           df_for_poly=df_for_poly,
           rep_dat=rep_dat,
           means=means)
    )
  }
  
  return(list(metrics=mets%>%gather("Metric","Value",4:ncol(.))))
  
}

smooth_fitter = function(data,var_func,smooth_df){
  y = unlist(data$mean_conc)
  x = unlist(data$measured_dilution_fraction)
  x[is.na(x)] = unlist(data$target_dilution_fraction)[is.na(x)]

  nn = unlist(data$n_conc)
  
  if(is.null(smooth_df)) {
    smooth_df = length(unique(data$target_dilution_fraction))-1
  }
  
  fit.dat2 = data.frame(y,poly(x,smooth_df,raw=TRUE))
  smooth_fit = lm(y~.,w=1/var_func(x),data=fit.dat2)
    
  return(smooth_fit)
}

compute_metrics = function(prop_fit,smooth_fit){
  p.res<-prop_fit$residuals
  p.fit<-prop_fit$fitted.values
  s.fit<-smooth_fit$fitted.values
  s.res<-s.fit-p.fit
  w<-smooth_fit$model$'(weights)'
  f1<-lm(s.fit~p.fit-1,w=w)

  return(
    c('Prop.Const'=prop_fit$coefficients,
    'R.squared'=summary(prop_fit)$r.squared,
    'Scaled.Sum.Squared.Error'=sum((p.res/p.fit)^2),
    'Sum.Squared.Error'=sum((p.res)^2),
    'Scaled.Sum.Absolute.Error'=sum(abs(p.res/p.fit)),
    'Sum.Absolute.Error'=sum(abs(p.res)),
    'Smoothed.R.squared'=summary(f1)$r.squared,
    'Smoothed.Scaled.Sum.Squared.Error'=sum((s.res/p.fit)^2),
    'Smoothed.Sum.Squared.Error'=sum((s.res)^2)/p.fit[1]^2,
    'Smoothed.Scaled.Sum.Absolute.Error'=sum(abs(s.res/p.fit)),
    'Smoothed.Sum.Absolute.Error'=sum(abs(s.res))/p.fit[1],
    'Variance.Stabilized.Smoothed.Sum.Squared.Error'=sum((s.res/sqrt(abs(p.fit)))^2),
    'Variance.Stabilized.Smoothed.Sum.Absolute.Error'=sum(abs(s.res/sqrt(abs(p.fit))))
    )
  )
}

run_bootstrap_proc = function(dat,
                              n_boot,
                              var_func,
                              smooth_df,
                              log_scale,
                              metrics,
                              conf_lev,
                              from_shiny) {
  
  boot.metrics<-matrix(NA,nrow(metrics$metrics),n_boot)
  
  if(from_shiny) { 
    
    withProgress(message="Running Bootstrap Iterations",value=0, {
      for(i in 1:n_boot){
        boot.ind <- nonpar.boot.simple(dat,i)
        boot.dat <- dat[boot.ind[,1],]
        boot.dat$cell_conc <- dat$cell_conc[boot.ind[,2]]
        boot.dat$cell_conc[is.na(dat$cell_conc)] <- NA
        boot.dat$stock_extraction<-dat$stock_extraction
        boot.metrics[,i]<-unlist(calc.metrics(boot.dat,
                                              var_func,
                                              smooth_df,
                                              for_bootstrap=T)$metrics$Value)
        
        incProgress(amount = 1/n_boot, detail = paste("Bootstrap iteration",i))
        
      }
    })
  
  } else {
    
    for(i in 1:n_boot){
      boot.ind <- nonpar.boot.simple(dat,i)
      boot.dat <- dat[boot.ind[,1],]
      boot.dat$cell_conc <- dat$cell_conc[boot.ind[,2]]
      boot.dat$cell_conc[is.na(dat$cell_conc)] <- NA
      boot.dat$stock_extraction<-dat$stock_extraction
      boot.metrics[,i]<-unlist(calc.metrics(boot.dat,
                                            var_func,
                                            smooth_df,
                                            for_bootstrap=T)$metrics$Value)
    }
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
  
  boot.metrics[rows_to_transform,] <- mylogistic(boot.metrics[rows_to_transform,])
  boot.metrics[rows_to_transform_nonzero,] <- exp(boot.metrics[rows_to_transform_nonzero,])
  
  # cap at 0
  metrics$metrics$lower <- pmax(metrics$metrics$lower,0)
  
  return(
    list(boot.metrics = boot.metrics,
         metrics = metrics,
         boot.dat = boot.dat)
  )
  
}

compare_metrics <- function(metrics,boot.metrics,dat,conf_lev) {
  
  # first column is value, next n_boot columns are samples
  boot.metrics<-cbind(metrics$metrics$Value,boot.metrics)
  n_compare = length(unique(dat$counting_method))
  n.met<-nrow(boot.metrics)/n_compare
  alpha = 1 - conf_lev
  
  metrics$compare<-data.frame(Metric=NULL,level1=NULL,level2=NULL,Ratio=NULL,lower=NULL,upper=NULL)
  
  if(n_compare == 1) {
    metrics$multiple_methods = FALSE
    return(metrics)
  }
  
  metrics$multiple_methods = TRUE
  
  for(i in 1:(n_compare-1)){
    for(j in (i+1):n_compare){
      t.comp <- boot.metrics[(0:(n.met-1))*n_compare+i,]/(boot.metrics[(0:(n.met-1))*n_compare+j,] + .0000001)
      
      if(any(rowMeans(is.na(t.comp))>.1)) {
        print("Bootstrap producing NA values")
      } 
      
      t.comp<-data.frame(metrics$metrics$Metric[(0:(n.met-1))*n_compare+i],
                         level1=metrics$metrics$comp_factor[i],  ### First level in comparison
                         level2=metrics$metrics$comp_factor[j],  ### divided by second level in comparison
                         Ratio=t.comp[,1],
                         t(apply(t.comp[,-1],1,quantile,c(alpha/2,1-alpha/2),na.rm=TRUE)))
      names(t.comp)<-c("Metric","level1","level2","Ratio","lower","upper")
      metrics$compare<-bind_rows(metrics$compare,t.comp)
    }
  }
  
  return(metrics)
  
}

nonpar.boot.simple = function(dat,i) {
  ### Conduct a nonparametric bootstrap to characterize the confidence in these results
  grp.ind<-as.numeric(as.factor(paste(dat$counting_method,
                                      dat$cell_type,
                                      dat$concentration_type)))
  boot.ind<-NULL
  for(g in 1:max(grp.ind)){
    set.seed(i)
    for(t in unique(dat$target_dilution_fraction[grp.ind==g])){
      
      inds.pool = which(dat$target_dilution_fraction==t&grp.ind==g)
      samps.boot<-sample(inds.pool,length(inds.pool),replace=TRUE)
      boot.ind = c(boot.ind,samps.boot)
      
    }
  }
  na_obs<-which(is.na(dat$cell_conc[boot.ind]))
  
  boot.ind2<-boot.ind 
  
  if(length(na_obs)>0){
    for(j in na_obs){
      good<-which(dat$target_dilution_fraction==dat$target_dilution_fraction[j]&
                    grp.ind==grp.ind[j]&
                    !is.na(dat$cell_conc))
      if(length(good)<1) stop(paste("No non-missing cell concentration values in",
                                    paste("Counting Method",dat$counting_method[j],
                                          ", Cell Type",dat$cell_type[j],
                                          ", Concetration Type",dat$concentration_type[j],
                                          ", Target Dilution Fraction",dat$target_dilution_fraction[j])))
      else boot.ind2[j]<-sample(good,1)
    }
  }
  cbind(boot.ind,boot.ind2)
  
}

metrics_key_value = function(inds) {
  
  vec = c(rep(NA,6),
    "Smoothed.Sum.Squared.Error",
    "Smoothed.Scaled.Sum.Squared.Error",
    "Smoothed.Sum.Absolute.Error",
    "Smoothed.Scaled.Sum.Absolute.Error",
    "Variance.Stabilized.Smoothed.Sum.Squared.Error",
    "Variance.Stabilized.Smoothed.Sum.Absolute.Error"
    )
  
  return(vec[inds])
  
}

# utility functions 
mylogit = function(x) {
  log(x/(1-x))
}

mylogistic = function(x) {
  exp(x)/(1 + exp(x))
}

nonpar.boot = function(dat,i){
  ### Conduct a nonparametric bootstrap to characterize the confidence in these results
  grp.ind<-as.numeric(as.factor(paste(dat$counting_method,
                                      dat$cell_type,
                                      dat$concentration_type)))

  
  boot.ind<-NULL
  for(g in 1:max(grp.ind)){
    #set.seed(i)
    for(t in unique(dat$target_dilution_fraction[grp.ind==g])){
      
      samps.pool<-unique(dat$random_sample_number[dat$target_dilution_fraction==t & grp.ind==g])
      if(length(samps.pool) == 1) {
        samps.boot = samps.pool
      
      } else{ 
        samps.boot<-sample(samps.pool,length(samps.pool),replace=TRUE)
      }
      
      
      for(s in samps.boot){
        obs.pool<-which(dat$random_sample_number==s&grp.ind==g)
        t.samp<-sample(obs.pool,length(obs.pool),replace=TRUE)
        boot.ind<-c(boot.ind,t.samp)
      }
    }
  }
  
  na_obs<-which(is.na(dat$cell_conc[boot.ind]))
  
  boot.ind2<-boot.ind 
  
  if(length(na_obs)>0){
    for(j in na_obs){
      good<-which(dat$target_dilution_fraction==dat$target_dilution_fraction[j]&
                    grp.ind==grp.ind[j]&
                    !is.na(dat$cell_conc))
      if(length(good)<1) stop(paste("No non-missing cell concentration values in",
                                    paste("Counting Method",dat$counting_method[j],
                                          ", Cell Type",dat$cell_type[j],
                                          ", Concetration Type",dat$concentration_type[j],
                                          ", Target Dilution Fraction",dat$target_dilution_fraction[j])))
      else boot.ind2[j]<-sample(good,1)
    }
  }
  cbind(boot.ind,boot.ind2)
}

round_or_truncate = function(x,numsig) {
  
  if(x < 0) {
    return(signif(x,numsig)) 
    
  } else if(x > 10) {
    return(round(x,0))
    
  } else {
    return(round(x,numsig))
  }
  
  
}

get_cdf = function(data) {
  
  n_points = max(c(100,length(data)))
  
  x = seq(min(data),max(data),length.out = n_points)
  y = rep(0,length(x))
  
  for(i in 1:length(y)) {
    y[i] = mean(data < x[i])
  }
  
  return(data.frame(x=x,y=y))
  
}

make_monotonic = function(y) {
  # takes a function and flattens where necessary so that there are no 'hills'

  for(ii in 2:length(y)) {
    y[ii] = max( (y[(ii-1)] + 10**(-5)),y[ii])
  }
  
  return(y)
}

find_ranges = function(y,x,var_func,degree,new_x=1:100/100){
  
  # fit polynomial model, get prediction intervals on new data
  X<-poly(x,degree=degree,raw=TRUE)
  fit<-lm(y~.,weights=1/var_func(x),data=as.data.frame(X))

  newdata<-poly(new_x,degree=degree,raw=TRUE)
  pred1<-predict(fit,newdata=as.data.frame(newdata),interval="prediction",weights=1/var_func(new_x))
  upper<-approxfun(x=pred1[,2],y=new_x)
  lower<-approxfun(x=pred1[,3],y=new_x)
  
  top<-upper(pred1[,1])
  bottom<-lower(pred1[,1])
  
  return(data.frame(new_x=new_x,top=top,bottom=bottom))
}

reformat_data = function(data){ 
  number_datasets = (nrow(data)+1)/10
  
  datasets = list()
  method_names = rep("",number_datasets)
  
  for(ii in 1:number_datasets) {
    inds = ((ii-1)*10+1):((ii)*10-1)
    t_data = data[inds,]
    method_names[ii] = t_data[1,2]
    datasets[[ii]] = as.data.frame(lapply(t_data[4:9,],as.numeric))
    colnames(datasets[[ii]]) = c('target_dilution_fraction',as.character(1:6))
    datasets[[ii]] = datasets[[ii]] %>% pivot_longer(!target_dilution_fraction,
                                                     names_to = 'rep',
                                                     values_to = 'cell_conc')
    
    datasets[[ii]]$stock_solution = ii
    datasets[[ii]]$cell_type = 'cell_type'
    datasets[[ii]]$concentration_type = 'total_cell_conc'
    datasets[[ii]]$analyst = 1
    datasets[[ii]]$counting_method = unlist(method_names[ii])
    datasets[[ii]]$raw_count = NA
    datasets[[ii]]$time_elapsed = NA
    datasets[[ii]]$measured_dilution_fraction = NA
    datasets[[ii]]$random_sample_number = rep(1:12,each=3)
    datasets[[ii]]$stock_extraction = datasets[[ii]]$random_sample_number + (ii-1)*12
    datasets[[ii]]$replicate_sample = rep(rep(c(1,2),each=3),6)
    datasets[[ii]]$rep_obsv = rep(1:3,12)
    
    cc = datasets[[ii]]$cell_conc[datasets[[ii]]$target_dilution_fraction > .999]
    datasets[[ii]]$starting_soln_conc = mean(cc)
    
  }
  
  full_data = as.data.frame(bind_rows(datasets))
  
  full_data$rep = NULL
  
  return(full_data)
  
}

process_method = function(data,starting_index) {
  
  df_spots = 10
  num_rep_obsv = 5
  num_replicate_samples = 5
  
  t_total_rows = df_spots*num_replicate_samples*num_rep_obsv
  
  t_data = data.frame(counting_method = rep(NA,t_total_rows),
                      target_dilution_fraction = NA,
                      random_sample_number = NA,
                      rep_obsv = NA,
                      cell_conc = NA)
  
  counter = 1
  t_random_sample_number = 0
  row_inds = (starting_index + 2):(starting_index + 2 + df_spots - 1)
  
  for(ii in 1:length(row_inds)) {
    
    for(jj in 1:num_replicate_samples) {
      col_inds = 1:num_rep_obsv + (jj-1)*num_rep_obsv + 1
      t_random_sample_number = t_random_sample_number + 1
      
      for(kk in 1:length(col_inds)) {
        
        if(is.na( data[ row_inds[ii],col_inds[kk] ])) {
          counter = counter + 1
          next
        }
        
        t_data$cell_conc[counter] = as.numeric(data[ row_inds[ii] , col_inds[kk] ])
        t_data$target_dilution_fraction[counter] = as.numeric(data[ row_inds[ii],1])
        t_data$random_sample_number[counter] = as.numeric(t_random_sample_number)
        t_data$rep_obsv[counter] = as.numeric(kk)
        t_data$counting_method[counter] = as.character(data[starting_index,1])
        
        counter = counter + 1
        
      }
    }
  }
  
  t_data = t_data[!is.na(t_data$cell_conc),]
  t_data$random_sample_number = as.integer(factor(t_data$random_sample_number))
  
  
  return(as.data.frame(t_data))
  
}

simple_to_gui = function(data) {
  
  method_starts = grep('target dilution fraction',data$...1,ignore.case = T) - 1
  method_starts = method_starts[method_starts >= 4]
  
  lod = as.list(rep(0,length(method_starts)))
  names(lod) = as.character(1:length(lod))
  
  for(ii in 1:length(method_starts)) { 
    
    # if all missing values in the first 
    lod[[as.character(ii)]] = process_method(data,method_starts[ii])
    
  }
  
  the_names = names(lod)
  
  for(ii in 1:length(the_names)) {
    
    if(nrow(lod[[ the_names[ii] ]]) < 1) {
      lod[[ the_names[ii] ]] = NULL
    }
    
  }
  
  big_d = dplyr::bind_rows(lod)
  
  return(big_d)
  
}

get_metrics_names = function() {
  
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
  
  
  return(
    list(
      all_metrics=all_metrics,
      all_metrics_names_pretty=all_metrics_names_pretty)
    )
  
}

is_increasing = function(x) {
  
  if(any(is.na(x))) {
    stop("NAs detected in input.")
  }
  
  return(all( x[1:(length(x)-1)] < x[2:length(x)]))
  
}
