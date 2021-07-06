
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


### Function fitting a proportional model
prop_fitter<-function(data,var_func){
  y=unlist(data$mean_conc)
  x<-unlist(data$measured_dilution_fraction)
  x[is.na(x)]<-unlist(data$target_dilution_fraction)[is.na(x)]
  fit.dat<-data.frame(y=y,x=x)
  lm(y~x-1,w=unlist(data$n_conc)/var_func(x),data=fit.dat)
}

### Function fitting a flexible model
smooth_fitter<-function(data,var_func,smooth_df){
  y=unlist(data$mean_conc)
  x<-unlist(data$measured_dilution_fraction)
  x[is.na(x)]<-unlist(data$target_dilution_fraction)[is.na(x)]
  #  x<-x[!is.na(y)]
  nn<-unlist(data$n_conc)#[!is.na(y)]
  #    y<-y[!is.na(y)]
  if(is.null(smooth_df)) {
    smooth_df<-length(unique(data$target_dilution_fraction))-1
  }

  fit.dat2<-data.frame(y,poly(x,smooth_df,raw=TRUE))
  smooth_fit<-lm(y~.,w=1/var_func(x),data=fit.dat2)
  # old.means<-0
  # new.means<-predict(smooth_fit)
  # #browser()
  # count = 1
  # while(any(abs(old.means/new.means-1)>.0001) && count < 100){
  #   t.w<-nn/var_func(new.means)
  #   t.w[t.w < 0] <- 0
  #   
  #   if(count == 99 ) {
  #     #browser()
  #   }
  #     
  #     
  #   smooth_fit<-lm(y~.,w=t.w,data=fit.dat2)
  #   old.means<-new.means
  #   new.means<-predict(smooth_fit)
  #   count = count + 1
  #   
  # }
  smooth_fit
}


metrics<-function(prop_fit,smooth_fit){
  p.res<-prop_fit$residuals
  p.fit<-prop_fit$fitted.values
  s.fit<-smooth_fit$fitted.values
  s.res<-s.fit-p.fit
  w<-smooth_fit$model$'(weights)'
  f1<-lm(s.fit~p.fit-1,w=w)

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
    'Smoothed.Sum.Absolute.Error'=sum(abs(s.res))/p.fit[1])
}


nonpar.boot.simple<-function(dat,i) {
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


mylogit <- function(x) {
  log(x/(1-x))
}


mylogistic <- function(x) {
  exp(x)/(1 + exp(x))
}


nonpar.boot<-function(dat,i){
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


round_or_truncate <- function(x,numsig) {
  
  if(x < 0) {
    return(signif(x,numsig)) 
    
  } else if(x > 10) {
    return(round(x,0))
    
  } else {
    return(round(x,numsig))
  }
  
  
}


get_cdf <- function(data) {
  
  n_points = max(c(100,length(data)))
  
  x = seq(min(data),max(data),length.out = n_points)
  y = rep(0,length(x))
  
  for(i in 1:length(y)) {
    y[i] = mean(data < x[i])
  }
  
  return(data.frame(x=x,y=y))
  
}

make_monotonic <- function(y) {
  # takes a function and flattens where necessary so that there are no 'hills'
  
  flat_y = y

  for(ii in 2:length(y)) {
    if(any(y[-(1:ii)] < y[ii])) {
      flat_y[ii] = min(y[-(1:ii)])
    }
  }
  
  return(flat_y)
}


find_ranges<-function(y,x,var_func,degree,new_x=1:100/100){
  
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
  
  method_starts = grep('method',data$...1,ignore.case = T)
  method_starts = method_starts[method_starts > 3]
  
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
