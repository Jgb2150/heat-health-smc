################################################################################-
# Function to load estimates for plots ----
# Comparing HeatRisk and temperature for local heat surveillance: A time-stratified 
#  case-crossover study of emergency department visits in San Mateo County, 
#  California, 2017–2025
# Juliana GE Bartels
# Updated: 6/2/2026
################################################################################-
## 0: Define model parameters ----
################################################################################-

# Model parameters:

## selected_constraints:   selected_constraints_death = death model constraints
##                         selected_constraints_ed = ED model constraints
##
## outcome:               'death' = death model
##                        'ed' = ED visit model
##
## subset_var:            'age' = age groups (under 5, 5 to 17, 18 to 64, 65 plus)
##                        'region' = regions in SMC (coastside, mid, north, south)
##                        'sex' = sex of patients (female, male)
##                        'cause' = cause of death or visit (non-accidental, cardiovascular, respiratory, injury, stroke)
##              
## var_grid:              'age_grid' = variables for age plots
##                        'region_grid' = variables for region plots
##                        'sex_grid' = variables for sex plots
##                        'cause_grid' = variables for cause plots
##                        'hosp_grid' = variables for cause plots
##
## estimate_type:         'cum' = cumulative relative risk
##                        'ind' = individual lag relative risk
##
## data_source_name:      '' = when data_source is 'original'
##                        '_synth' = when data_source is 'synthetic'

################################################################################-
## 1: Define function ----
################################################################################-

load_estimates <- function(selected_constraints,
                           outcome, 
                           subset_var, 
                           var_grid,
                           estimate_type,
                           data_source_name
                           ) {
  
  for(i in 1:nrow(var_grid)){
    
    temp_var_i <- var_grid$temp_var[i]
    subset_i <- var_grid$subset[i]
    estimate_name_i <- var_grid$estimate_name[i]
    mod_name_i <- var_grid$mod_name[i]
    
    if(temp_var_i == 'tmean'){
      estimates <- read_csv(here::here('outputs', 'estimates', outcome, estimate_type, temp_var_i,
                                       paste0('est_', estimate_type, '_', outcome, '_', 
                                              selected_constraints[2,1], '_', subset_var, '_', subset_i, '_', 
                                              selected_constraints[2,4], '_', selected_constraints[2,5], '_', 
                                              selected_constraints[2,6], '_', selected_constraints[2,7], '_', 
                                              temp_var_i, data_source_name,
                                              '.csv')))
      
      mod <- readRDS(here::here('outputs', 'models', outcome, temp_var_i, 
                                paste0(outcome, '_', selected_constraints[2,1], '_', subset_var, '_', subset_i, '_', 
                                       selected_constraints[2,4], '_', selected_constraints[2,5], '_', 
                                       selected_constraints[2,6], '_', selected_constraints[2,7], '_', 
                                       temp_var_i, data_source_name,
                                       '.RDS')))
    }
    if(temp_var_i == 'heat_risk'){
      estimates <- read_csv(here::here('outputs', 'estimates', outcome, estimate_type, temp_var_i,
                                       paste0('est_', estimate_type, '_', outcome, '_',
                                              selected_constraints[1,1], '_', subset_var, '_', subset_i, '_', 
                                              selected_constraints[1,4], '_', selected_constraints[1,5], '_', 
                                              selected_constraints[1,6], '_', selected_constraints[1,7], '_', 
                                              temp_var_i, data_source_name,
                                              '.csv')))
      
      mod <- readRDS(here::here('outputs', 'models', outcome, temp_var_i, 
                                paste0(outcome, '_', selected_constraints[1,1], '_', subset_var, '_', subset_i, '_',
                                       selected_constraints[1,4], '_', selected_constraints[1,5], '_', 
                                       selected_constraints[1,6], '_', selected_constraints[1,7], '_', 
                                       temp_var_i, data_source_name,
                                       '.RDS')))
    }
    
    # Save model and estimates in the environment
    assign(estimate_name_i, estimates, envir = .GlobalEnv)
    assign(mod_name_i, mod, envir = .GlobalEnv)
  }
}

