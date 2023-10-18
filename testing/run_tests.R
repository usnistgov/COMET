library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
options(dplyr.summarise.inform = FALSE)

setwd("C:/Users/dtn1/OneDrive\ -\ NIST/Documents/ByScientist/SumonaSarkar/AppV1")

source('R/utils.R')
source('R/plot_utils.R')
source('R/run_comet.R')

filepaths = c('other/Full_Template_Example_1_COMET.csv',
              'other/Full_Template_Example_2_COMET.csv',
              'other/Full_Template_Example_3_COMET.csv')

for(ii in 1:length(filepaths)) {
  
  data = read.csv(filepaths[ii])
  
  metrics = run_comet(dataset=data,
                      n_boot = 50,
                      seed = 1234,
                      var_func = 1, # variance proportional to mean
                      smooth_df = 0, # default smoothing
                      perf_metrics = c(7,8,9,10,11,12),
                      conf_lev = .95)
  
  
  print(raw_data_plot(metrics)) 
  
  print(means_plot(metrics)) 
  
  print(means_barplot(metrics)) 
  
  print(prop_const_plot(metrics) )
  
  print(residual_plot(metrics) )
  
  print(cv_barplot(metrics) )
  
  print(pi_plot(metrics) )
  
  print(r2_plot(metrics) )
  
  print(empirical_cdf_plot(metrics) )
  
  print(viability_resids_hist_plot(metrics) )
  
  print(viability_hist_plot(metrics) )
  
  print(via_over_time_plot(metrics) )
  
  print(pipette_error_plot(metrics))
  
  print(experimental_integrity_plot(metrics) )
  
  db_res = discrimination_bands_plot(metrics) 
  print(db_res[[1]])
  print(db_res[[2]])
  print(db_res[[3]])
  
}




