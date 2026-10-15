################################################################################-
# Fit sensitivity model for death analysis ----
# Comparing HeatRisk and temperature for local heat surveillance: A time-stratified 
#  case-crossover study of emergency department visits in San Mateo County, 
#  California, 2017–2025
# Juliana GE Bartels
# Updated: 6/12/2026
################################################################################-

# This file runs sensitivity analyses for the model excluding 2020-2021 (nocov 
# model) and with a binary variable for federal holidays (holiday model). It 
# performs a search for the constraints with the best fit for daily death counts 
# and runs main models for temperature and HeatRisk and daily death counts for 
# both sensitivity analyses.

################################################################################-
## 0: Upload data ----
################################################################################-

# Load dataset of deaths and ED visits matched with temperature exposures
if(data_source == 'original') {
  dta_full <- read_csv(here::here(
    "data", "prepared",
    "death_ed_climate_df.csv"
  )) %>%
    mutate(stratum = as.factor(stratum))
}

if(data_source == 'synth') {
  dta_full <- read_csv(here::here(
    "data", "prepared",
    "death_ed_climate_df_synth.csv"
  )) %>%
    mutate(stratum = as.factor(stratum))
}

################################################################################-
## 1: Perform grid search to select constraints for no covid model ----
################################################################################-

# Create grid of potential model constraints for mean temperature
potent_model_constraints_tmean <- expand_grid(
  er_constraint = c('a', 'b', 'c', 'd', 'e'),
  er_type = c('ns'),
  lr_type = c('ns'),
  temp_var = c('tmean')
) %>%
  rbind(., 
        expand_grid(
          er_constraint = c('a'),
          er_type = c('quad', 'cub', 'quar'),
          lr_type = c('ns'),
          temp_var = c('tmean'))
  )

# Create grid of potential model constraints for HeatRisk
potent_model_constraints_hr <- expand_grid(
  er_constraint = c('a'),
  er_type = c('strata'),
  lr_type = c('ns'),
  temp_var = c('heat_risk')
)

potent_model_constraints <- rbind(potent_model_constraints_tmean, potent_model_constraints_hr)

# Perform grid search to identify optimal constraints - for sensitivity model without 2020 and 2021
perform_grid_search(
  potent_model_constraints = potent_model_constraints,
  outcome = 'death',
  sensitivity = 'nocov',
  subset_var = 'full',
  subset = 'full',
  temp_analysis = 'full',
  dta = dta_full,
  pop_denom = 'total_pop_all',
  outcome_var = 'death_count',
  data_source = data_source
)

################################################################################-
## 2: Run main model with selected constraints (lowest QAIC), no covid model ----
################################################################################-

# Create grid of main model variables for deaths
main_model_var_death <- expand_grid(
  outcome = ('death'),
  subset_var = c('full'),
  subset = c('full'),
  temp_var = c('tmean', 
               'heat_risk')
) %>% 
  mutate(pop_denom = paste0('total_pop_all'), 
         outcome_var = paste0('death_count')
  )

# Get estimates from main model with selected constraints
fit_model_w_selected_constraints(
  sensitivity = 'nocov',
  temp_analysis = 'full',
  dta = dta_full,
  model_var_grid = main_model_var_death,
  data_source = data_source
)

################################################################################-
## 3: Clean up ----
################################################################################-

rm(dta_full, 
   potent_model_constraints_tmean, potent_model_constraints_hr, potent_model_constraints,
   main_model_var_death)
