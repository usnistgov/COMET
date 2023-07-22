library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
options(dplyr.summarise.inform = FALSE)

source('R/utils.R')
source('R/plot_utils.R')
source('R/run_comet.R')

filepath = './other/Full_Template_Example_3_COMET.csv'
data = read.csv(filepath)

comet_results = run_comet(dataset=data,
                          n_boot = 100,
                          seed = 123,
                          var_func = 1,
                          smooth_df = 0, # -> default smoothing
                          perf_metrics = c(7,8,9,10),
                          conf_lev = .95)

pi_plot(comet_results)


