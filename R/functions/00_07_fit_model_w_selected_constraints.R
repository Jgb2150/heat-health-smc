################################################################################-
# Identify selected constraints with lowest QAIC and fit model ----
# Comparing HeatRisk and temperature for local heat surveillance: A time-stratified 
#  case-crossover study of emergency department visits in San Mateo County, 
#  California, 2017–2025
# Juliana GE Bartels
# Updated: 4/30/2026
################################################################################-
## 0: Define model parameters ----
################################################################################-

## sensitivity:   'main' = main model
##                'nocov' = without years 2020 and 2021
##
## temp_analysis: 'full' = analysis of full year of data
## 
## dta:           'dta_full'
##
## model_var_grid: age_model_constraints
##                 sex_model_constraints
##                 region_model_constraints
##                 cause_model_constraints
##                 hosp_model constraints
##
## data_source:   'original' = original, private data
##                'synth' = synthetic data
## 

################################################################################-
## 1: Define function ----
################################################################################-

fit_model_w_selected_constraints <- function(sensitivity, 
                                             temp_analysis, 
                                             dta, 
                                             model_var_grid,
                                             data_source
                                             ){
  
  
  # Generate suffix for file calling and writing, based on data source
  data_source_name <- case_when(data_source == 'original' ~ '',
                                data_source == 'synth' ~ '_synth')
  
  for(i in 1:nrow(model_var_grid)) {
    
    subset_var <- model_var_grid$subset_var[[i]]
    subset <- model_var_grid$subset[[i]]
    temp_var <- model_var_grid$temp_var[[i]]
    outcome <- model_var_grid$outcome[[i]]
    outcome_var <- model_var_grid$outcome_var[[i]]
    pop_denom <- model_var_grid$pop_denom[[i]]
    
    # Read all of the selected models and keep only the selected constraints for the model of interest
    selected_constraints <- read_csv(here::here('outputs', 'tables',
                                                paste0('selected_constraints_', 
                                                       outcome, data_source_name,
                                                       '.csv'))
                                        ) %>% 
      filter(sensitivity == .env$sensitivity & 
               subset_var == 'full' & 
               subset == 'full' & 
               temp_analysis == .env$temp_analysis,
               temp_var == .env$temp_var)
    
    # Fit selected model
    analyze_temp_dlnm(sensitivity = sensitivity,
                      subset_var = subset_var,
                      subset = subset,
                      outcome = outcome,
                      er_constraint = selected_constraints$er_constraint, 
                      er_type = selected_constraints$er_type, 
                      lr_type = selected_constraints$lr_type,
                      temp_analysis = 'full',
                      save_model = 'save_model',
                      dta = dta, 
                      temp_var = temp_var,
                      pop_denom = pop_denom,
                      outcome_var = outcome_var,
                      data_source = data_source
                      )
    
    # Print statement to verify completion of model
    print(paste0('Model fit completed. Subset variable: ', subset_var, '. Subset: ', subset, '. Temperature variable: ', temp_var, '. Outcome: ', outcome, 
                 '. Data source: ', data_source, '.' 
                 ))
    
  }
}
