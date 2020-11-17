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
  if(is.null(smooth_df))smooth_df<-length(unique(data$target_dilution_fraction))-1
  fit.dat2<-data.frame(y,poly(x,smooth_df,raw=TRUE))
  smooth_fit<-lm(y~.,w=nn/var_func(x),data=fit.dat2)
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
    set.seed(i)
    for(t in unique(dat$target_dilution_fraction[grp.ind==g])){
      
      samps.pool<-unique(dat$random_sample_number[dat$target_dilution_fraction==t&grp.ind==g])
      samps.boot<-sample(samps.pool,length(samps.pool),replace=TRUE)
      
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