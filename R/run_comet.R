run_comet = function(dataset,
                     n_boot = 50,
                     seed = 123,
                     var_func = 1,
                     smooth_df = 0, # 0 will go to default
                     perf_metrics = c(7,8,9,10),
                     conf_lev = .95,
                     from_shiny = FALSE) {
  
  check_inputs(dataset,
               n_boot,
               seed,
               var_func,
               smooth_df,
               perf_metrics,
               conf_lev)
  
  dat = dataset
  
  set.seed(seed)
  
  var_func_ind = var_func
  var_func = switch(var_func,
                    function(x) x,
                    function(x) rep(1,length(x)),
                    function(x) x^2)
  
  if(smooth_df == 0) {
    smooth_df = length(unique(dat$target_dilution_fraction)) - 1
  }
  

  
  dat = process_colnames(dat)
  dat = remove_useless_rows(dat)
  hmdf_res = handle_measured_dilution_fraction(dat)
  dat = hmdf_res$dat
  mdf_exists = hmdf_res$mdf_exists
  remove(hmdf_res)
  
  dat = check_stock_ext_and_rep_samp(dat)
  
  
  exp_des_flag = check_experimental_design(dat)
  
  # organize dat
  dat = dat %>% 
    arrange(counting_method,
            cell_type,
            concentration_type,
            stock_solution,
            target_dilution_fraction,
            measured_dilution_fraction,
            stock_extraction)
  
  # compute metrics
  metrics = calc.metrics(dat,var_func,smooth_df)
  
  #browser()
  
  # bootstrap the metrics
  metrics$metrics$upper = NULL
  metrics$metrics$lower = NULL
  
  bp_res = run_bootstrap_proc(dat,
                              n_boot,
                              var_func,
                              smooth_df,
                              log_scale,
                              metrics,
                              conf_lev,
                              from_shiny=from_shiny)
  
  boot.metrics = bp_res$boot.metrics
  metrics = bp_res$metrics
  boot.dat = bp_res$boot.dat
  remove(bp_res)
  
  metrics$Title <- 'Cell Concentration vs. Dilution Fraction'
  metrics$metrics$comp_factor <- metrics$metrics[,'counting_method']
  
  metrics = compare_metrics(metrics,boot.metrics,dat,conf_lev)
  
  metrics$dat = dat
  metrics$smooth_df = smooth_df
  metrics$n_boot = n_boot
  metrics$conf_level = conf_lev
  metrics$var_func = var_func
  metrics$var_func_ind = var_func_ind
  metrics$mdf_exists = mdf_exists
  metrics$exp_des_flag = exp_des_flag
  metrics$perf_metrics = perf_metrics
  
  return(metrics)
}

