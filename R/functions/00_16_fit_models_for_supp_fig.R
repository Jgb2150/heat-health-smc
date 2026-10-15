################################################################################-
# Identify selected constraints with lowest QAIC and fit model for ED visit model----
# Comparing HeatRisk and temperature for local heat surveillance: A time-stratified 
#  case-crossover study of emergency department visits in San Mateo County, 
#  California, 2017–2025
# Juliana GE Bartels
# Updated: 6/24/2026
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
## models_grid:   grid of model constraints
##
## data_source:   'original' = original, private data
##                'synth' = synthetic data
## 

################################################################################-
## 1: Define function ----
################################################################################-

fit_models_for_supp_fig <- function(models_grid,
                                    sensitivity, 
                                    subset_var, 
                                    subset, 
                                    outcome,
                                    temp_analysis, 
                                    dta, 
                                    pop_denom, 
                                    outcome_var,
                                    data_source
                                    ){
  
  
  # Generate suffix for file calling and writing, based on data source
  data_source_name <- case_when(data_source == 'original' ~ '',
                                data_source == 'synth' ~ '_synth')
  
  for(i in 1:nrow(models_grid)) {
    
    # Fit selected model
    analyze_temp_dlnm(sensitivity = sensitivity,
                      subset_var = subset_var,
                      subset = subset,
                      outcome = outcome,
                      er_constraint = models_grid$er_constraint[i], 
                      er_type = models_grid$er_type[i], 
                      lr_type = models_grid$lr_type[i],
                      temp_analysis = 'full',
                      save_model = 'save_model',
                      dta = dta, 
                      temp_var = models_grid$temp_var[i],
                      pop_denom = pop_denom,
                      outcome_var = outcome_var,
                      data_source = data_source
                      )
    
    # Print statement to verify completion of model
    print(paste0('Model fit completed. ER constraint: ', models_grid$er_constraint[i], '. ER type: ',  models_grid$er_type[i], '. Outcome: ', outcome, 
                 '. Data source: ', data_source, '.' 
                 ))
    
  }
}
