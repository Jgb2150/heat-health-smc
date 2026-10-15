################################################################################-
# Function to perform grid search to identify constraints with lowest QAIC for model ----
# Comparing HeatRisk and temperature for local heat surveillance: A time-stratified 
#  case-crossover study of emergency department visits in San Mateo County, 
#  California, 2017–2025 
# Juliana GE Bartels
# Updated: 6/12/2026
################################################################################-
## 0: Define model parameters ----
################################################################################-

# Model parameters

## potent_model_constraints: 'potent_model_constraints' = dataframe of potential model constraints
## 
## sensitivity:              'main' = main model
##                           'nocov' = without years 2020 and 2021
##
## subset_var:               'full' = full dataset
## 
## subset:                   'full' = full dataset
##
## outcome:                  'ed' = ED visit
##                           'death' = death
##
## temp_analysis:            'full' = analysis full year dataframe (mean temp)
##
## dta:                      'dta_full'
##
## pop_denom:                'total_pop_all' = full dataset
##
## outcome_var:              ('ed_count_', subset_var, '_', subset)
##                           ('death_count_', subset_var, '_', subset)
## 
## data_source:             'original' = original, private data
##                          'synth' = synthetic data

################################################################################-
## 1: Define function ----
################################################################################-

perform_grid_search <- function(potent_model_constraints, 
                                sensitivity, 
                                subset_var, 
                                subset, 
                                outcome,
                                temp_analysis, 
                                dta, 
                                pop_denom, 
                                outcome_var,
                                data_source) {
  
  # Loop over rows of potent_model_constraints
  for(i in 1:nrow(potent_model_constraints)){
    
    # Fit model with candidate constraints
    analyze_temp_dlnm(sensitivity = sensitivity,
                      subset_var = subset_var,
                      subset = subset,
                      outcome = outcome,
                      er_constraint = potent_model_constraints$er_constraint[i], 
                      er_type = potent_model_constraints$er_type[i],
                      lr_type = potent_model_constraints$lr_type[i],
                      temp_analysis = temp_analysis,
                      save_model = 'save_qaic',
                      dta = dta, 
                      temp_var = potent_model_constraints$temp_var[i],
                      pop_denom = pop_denom,
                      outcome_var = outcome_var,
                      data_source = data_source
    )
    
    # Print statement to verify successful run of model
    print(paste0('Model run. ER constraint: ', potent_model_constraints$er_constraint[i], '. ER type: ', potent_model_constraints$er_type[i], '. LR type: ', potent_model_constraints$lr_type[i],
                 '. Temperature variable: ', potent_model_constraints$temp_var[i], '. Outcome: ', outcome, '. Data source: ', data_source, '.' 
    ))
    
  }
  
  # Generate suffix for file calling and writing, based on data source
  data_source_name <- case_when(data_source == 'original' ~ '',
                                data_source == 'synth' ~ '_synth')
  
  # Select model
  # Create QAIC table
  qaic_table <- read_csv(here::here('outputs', 'tables', paste0('model_QAIC_', outcome, data_source_name, '.csv')),
                                  col_types = 'ccccccccdT') # %>%
    # filter(sensitivity == !!sensitivity & 
    #          subset_var == !!subset_var &
    #          subset == !!subset)
  
  # Keep only the most recent versions of each model and save 
  qaic_table %>% 
    group_by(sensitivity, subset_var, subset, er_constraint,
              er_type, lr_type, temp_analysis, temp_var) %>% 
    arrange(desc(run_date)) %>% 
    slice(1) %>% 
    filter(!is.na(sensitivity)) %>%
    write_csv(here::here('outputs', 'tables', paste0('model_QAIC_', outcome, data_source_name, '.csv')))
    
  selected_mod <- qaic_table %>%
    filter(sensitivity == !!sensitivity & 
             subset_var == !!subset_var &
             subset == !!subset) %>%
    mutate(QAIC = as.numeric(QAIC)) %>%
    group_by(temp_analysis, temp_var) %>%
    filter(QAIC == min(QAIC)) %>% 
    bind_rows(read_csv(here::here('outputs', 'tables', paste0('selected_constraints_', outcome, data_source_name, '.csv')),
                         col_types = 'ccccccccdT'))
  
  # Keep only the QAIC of the most recent versions of each model and save 
  selected_mod %>%
    group_by(sensitivity, subset_var, subset, temp_analysis, temp_var) %>% 
    arrange(desc(run_date)) %>% 
    slice(0:1) %>% 
    filter(!is.na(sensitivity)) %>%
    write_csv(here::here('outputs', 'tables', 
                         paste0('selected_constraints_', outcome, data_source_name, '.csv')))
  
}
