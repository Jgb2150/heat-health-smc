################################################################################-
# Function to save model outputs for mortality model ----
# Comparing HeatRisk and temperature for local heat surveillance: A time-stratified 
#  case-crossover study of emergency department visits in San Mateo County, 
#  California, 2017–2025 
# Juliana GE Bartels
# Updated: 6/2/2026
################################################################################-
## 0: Define model parameters ----
################################################################################-

# Model parameters ----

## mod:           'mod' = model output
##
## cb.temp:       'mod$cb.temp' = cross basis stored in model output
##
## model_name:    model name saved as 'outcome_sensitivity_subset_var_subset_er_constraint_er_type_lr_type_temp_analysis_temp_var
##
## temp_analysis: 'full' = analysis full year dataframe (mean temp)
##
## temp_var:      'tmean' = mean daily temperature
##                'heat_risk' = HeatRisk score
##
## data_source:   'original' = original, private data
##                'synth' = synthetic data


################################################################################-
## 1: Define function ----
################################################################################-

save_model <- function(mod, 
                       cb.temp,
                       model_name, 
                       temp_analysis, 
                       temp_var,
                       data_source) {
  
  # Generate suffix for file calling and writing, based on data source
  data_source_name <- case_when(data_source == 'original' ~ '',
                           data_source == 'synth' ~ '_synth')
  
  # Save overall model file to output folder
  mod %>% saveRDS(here::here('outputs', 'models', sub("_.*", "", model_name), temp_var,
                                           paste0(model_name, data_source_name, '.RDS')))
  
  # Generate estimates of IRR and attributable number 
    
    # For mean temperature analysis
    if(temp_analysis == 'full' & temp_var == 'tmean') {
      # Calculate counterfactual temperatures for 100 temperatures between min and max of dataset, and quantiles
      exposure_contrasts <- tibble(
        counterfactual_temp = c(seq(min(mod$data[[temp_var]], na.rm = TRUE), max(mod$data[[temp_var]], na.rm = TRUE), length.out = 100),
                                median(mod$data[[temp_var]], na.rm = TRUE),
                                quantile(mod$data[[temp_var]], 0.05, type = 1), # 5th percentile
                                quantile(mod$data[[temp_var]], 0.1, type = 1), # 10th percentile
                                quantile(mod$data[[temp_var]], 0.9, type = 1), # 90th percentile
                                quantile(mod$data[[temp_var]], 0.95, type = 1)), # 95th percentile
        label = c(rep('er_values', 100),
                  'median',
                  'per_05',
                  'per_10',
                  'per_90',
                  'per_95')) %>%
        mutate(counterfactual_temp = round(counterfactual_temp, 5))
  
      # Generate estimates of IRR compared to the median temperature
      est_ref_median <- crosspred(basis = cb.temp,
                                  model = mod,
                                  cen = median(mod$data[[temp_var]], na.rm = TRUE),
                                  at = exposure_contrasts$counterfactual_temp,
                                  cumul = TRUE,
                                  bylag = 1)
      
      # Calculate attributable number of deaths or ED visits
      an_calc <- calc_attributable_number(temp_var, cb.temp, mod, temp_analysis, model_name)
    }
    
    # For HeatRisk analysis
    if(temp_analysis == 'full' & temp_var == 'heat_risk') {
      # Calculate counterfactual temperatures for heat risk levels between min and max of dataset
      exposure_contrasts <- tibble(
        counterfactual_temp = c(seq(min(mod$data[[temp_var]], na.rm = TRUE), max(mod$data[[temp_var]], na.rm = TRUE), by = 1)), 
        label = 'heat_risk'
      )
      
      # Generate estimates of IRR compared to 0 heat risk  ----
      est_ref_0 <- crosspred(basis = cb.temp,
                                  model = mod,
                                  cen = 0,
                                  at = exposure_contrasts$counterfactual_temp,
                                  cumul = TRUE,
                                  bylag = 1)
      
      # Calculate attributable number of deaths or ED visits
      an_calc <- calc_attributable_number(temp_var, cb.temp, mod, temp_analysis, model_name)
    }
    
  # Save estimates, lower and upper 95% CI for individual lags
  
    # For mean temperature 
    if(temp_var == 'tmean') {
      fit_table <- as.data.frame(est_ref_median$matRRfit) %>% 
        mutate(refT = 'median',
               counterfactual_temp = rep(as.numeric(rownames(.))))
      fit_table <- fit_table %>% left_join(exposure_contrasts, by = c('counterfactual_temp'))
      colnames(fit_table) <- paste0('fit_rr_', colnames(fit_table))
    
      # Lower confidence intervals for individual lags
      lci_table <- as.data.frame(est_ref_median$matRRlow) %>% 
        mutate(refT = 'median')
      colnames(lci_table) <- paste0('lci_rr_', colnames(lci_table))
    
      # Upper confidence intervals for individual lags
      uci_table <- as.data.frame(est_ref_median$matRRhigh) %>% 
        mutate(refT = 'median')
      colnames(uci_table) <- paste0('uci_rr_', colnames(uci_table))
    
      # Combine fit and se for individual lags
      est_table <- bind_cols(fit_table, lci_table, uci_table) %>%
        rename(counterfactual_temp = fit_rr_counterfactual_temp,
               label = fit_rr_label)
      
      # Save table for individual lags
      est_table %>%
        write_csv(here::here('outputs', 'estimates', sub("_.*", "", model_name),
                              'ind', 'tmean', paste0('est_ind_', model_name, data_source_name, '.csv')))
    }
    
    # For HeatRisk 
    if(temp_var == 'heat_risk') {
      fit_table <- as.data.frame(est_ref_0$matRRfit) %>% 
        mutate(refT = '0',
               counterfactual_temp = rep(as.numeric(rownames(.))))
      fit_table <- fit_table %>% left_join(exposure_contrasts, by = c('counterfactual_temp'))
      colnames(fit_table) <- paste0('fit_rr_', colnames(fit_table))
      
      # Lower confidence intervals for individual lags
      lci_table <- as.data.frame(est_ref_0$matRRlow) %>% 
        mutate(refT = '0')
      colnames(lci_table) <- paste0('lci_rr_', colnames(lci_table))
      
      # Upper confidence intervals for individual lags
      uci_table <- as.data.frame(est_ref_0$matRRhigh) %>% 
        mutate(refT = '0')
      colnames(uci_table) <- paste0('uci_rr_', colnames(uci_table))
      
      # Combine fit and se for individual lags
      est_table <- bind_cols(fit_table, lci_table, uci_table) %>%
        rename(counterfactual_temp = fit_rr_counterfactual_temp,
               label = fit_rr_label)
      
      # Save table for individual lags
      est_table %>%
        write_csv(here::here('outputs', 'estimates', sub("_.*", "", model_name),
                              'ind', 'heat_risk', paste0('est_ind_', model_name, data_source_name, '.csv')))
    }

  # Save estimates lower and upper 95% CI for cumulative lags
  
    # For mean temperature 
    if(temp_var == 'tmean') {
      fit_cum_table <- as.data.frame(est_ref_median$cumRRfit) %>% 
      mutate(refT = 'median',
             counterfactual_temp = rep(as.numeric(rownames(.))))
      fit_cum_table <- fit_cum_table %>% left_join(exposure_contrasts, by = c('counterfactual_temp'))
      colnames(fit_cum_table) <- paste0('fit_rr_', colnames(fit_cum_table))
      
      # Lower confidence intervals for cumulative lags
      lci_cum_table <- as.data.frame(est_ref_median$cumRRlow) %>% 
        mutate(refT = 'median')
      colnames(lci_cum_table) <- paste0('lci_rr_', colnames(lci_cum_table))
      
      # Upper confidence intervals for cumulative lags
      uci_cum_table <- as.data.frame(est_ref_median$cumRRhigh) %>% 
        mutate(refT = 'median')
      colnames(uci_cum_table) <- paste0('uci_rr_', colnames(uci_cum_table))
      
      # Combine fit and se for cumulative lags
      est_table_cum <- bind_cols(fit_cum_table, lci_cum_table, uci_cum_table) %>%
        rename(counterfactual_temp = fit_rr_counterfactual_temp,
               label = fit_rr_label) 
      
      # Save table for cumulative lags
      est_table_cum %>%
        write_csv(here::here('outputs', 'estimates', sub("_.*", "", model_name),
                             'cum', 'tmean', paste0('est_cum_', model_name, data_source_name, '.csv')))
    }
  
  # For HeatRisk 
    if(temp_var == 'heat_risk') {
      fit_cum_table <- as.data.frame(est_ref_0$cumRRfit) %>% 
        mutate(refT = '0',
               counterfactual_temp = rep(as.numeric(rownames(.))))
      fit_cum_table <- fit_cum_table %>% left_join(exposure_contrasts, by = c('counterfactual_temp'))
      colnames(fit_cum_table) <- paste0('fit_rr_', colnames(fit_cum_table))
      
      # Lower confidence intervals for cumulative lags
      lci_cum_table <- as.data.frame(est_ref_0$cumRRlow) %>% 
        mutate(refT = '0')
      colnames(lci_cum_table) <- paste0('lci_rr_', colnames(lci_cum_table))
      
      # Upper confidence intervals for cumulative lags
      uci_cum_table <- as.data.frame(est_ref_0$cumRRhigh) %>% 
        mutate(refT = '0')
      colnames(uci_cum_table) <- paste0('uci_rr_', colnames(uci_cum_table))
      
      # Combine fit and se for cumulative lags
      est_table_cum <- bind_cols(fit_cum_table, lci_cum_table, uci_cum_table) %>%
        rename(counterfactual_temp = fit_rr_counterfactual_temp,
               label = fit_rr_label) 
      
      # Save table for cumulative lags
      est_table_cum %>%
        write_csv(here::here('outputs', 'estimates', sub("_.*", "", model_name),
                              'cum', 'heat_risk', paste0('est_cum_', model_name, data_source_name, '.csv')))
    }
  
  # Save attributable number of deaths
  an_calc_estimates <- read_csv(here::here('outputs', 'estimates',
                                         paste0('an_calc_estimates', data_source_name, '.csv'))
                                )

  an_calc_estimates[1+nrow(an_calc_estimates),] <- an_calc
  
  an_calc_estimates %>% 
    group_by(model_name, outcome_var, temp_analysis, temp_var) %>% 
    arrange(desc(run_date)) %>% 
    slice(1) %>% 
    write_csv(here::here('outputs', 'estimates',
                         paste0('an_calc_estimates', data_source_name, '.csv'))
    )
  
}
