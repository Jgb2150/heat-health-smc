################################################################################-
# Function to load estimates for plot of all models for supplemental figure ----
# Comparing HeatRisk and temperature for local heat surveillance: A time-stratified 
#  case-crossover study of emergency department visits in San Mateo County, 
#  California, 2017–2025 
# Juliana GE Bartels
# Updated: 6/24/2026
################################################################################-
## 0: Define model parameters ----
################################################################################-

# Model parameters:

## models_grid:           grid of model constraints
##
## outcome:               'death' = death model
##                        'ed' = ED visit model
##              
## estimate_type:         'cum' = cumulative relative risk
##                        'ind' = individual lag relative risk
##
## data_source_name:      '' = when data_source is 'original'
##                        '_synth' = when data_source is 'synthetic'

################################################################################-
## 1: Define function ----
################################################################################-

load_estimates_models_supp_fig <- function(models_grid,
                           outcome, 
                           estimate_type,
                           data_source_name
                           ) {
  
  for(i in 1:nrow(models_grid)){
    
    estimate_name_i <- paste0('est_', outcome, '_main_full_full_', models_grid$er_constraint[i], '_', models_grid$er_type[i], '_', models_grid$lr_type[i])
    
    # mod_name_i <- paste0('mod_', outcome, '_main_full_full_', models_grid$er_constraint[i], '_', models_grid$er_type[i], '_', models_grid$lr_type[i])
    
      estimates <- read_csv(here::here('outputs', 'estimates', outcome, estimate_type, 'tmean',
                                       paste0('est_', estimate_type, '_', outcome, '_main_full_full_', 
                                              models_grid$er_constraint[i], '_', models_grid$er_type[i], '_', 
                                              models_grid$lr_type[i], '_full_tmean', data_source_name,
                                              '.csv')))
      
      # mod <- read_csv(here::here('outputs', 'models', outcome, 'tmean',
      #                                  paste0(outcome, '_main_full_full_', 
      #                                         models_grid$er_constraint[i], '_', models_grid$er_type[i], '_', 
      #                                         models_grid$lr_type[i], '_full_tmean', data_source_name,
      #                                         '.RDS')))
    
    # Save model and estimates in the environment
    assign(estimate_name_i, estimates, envir = .GlobalEnv)
    # assign(mod_name_i, mod, envir = .GlobalEnv)
  }
}

