################################################################################-
# Fit models for supplement comparison of all exposure response functions ----
# Comparing HeatRisk and temperature for local heat surveillance: A time-stratified 
#  case-crossover study of emergency department visits in San Mateo County, 
#  California, 2017–2025 
# Juliana GE Bartels
# Updated: 6/24/2026
################################################################################-

# This file runs all the mean temperature models and plots them together to show 
# how they compare for a supplemental figure.

################################################################################-
## 0: Upload data ----
################################################################################-

# Load dataset of deaths and ED visits matched with temperature exposures
if(data_source == 'original') {
  dta_full <- read_csv(here::here(
    'data', 'prepared',
    'death_ed_climate_df.csv'
  )) %>%
    mutate(stratum = as.factor(stratum))
}

if(data_source == 'synth') {
  dta_full <- read_csv(here::here(
    'data', 'prepared',
    'death_ed_climate_df_synth.csv'
  )) %>%
    mutate(stratum = as.factor(stratum))
}

################################################################################-
## 1a: Fit all ED visits models ----
################################################################################-

# Create grid of model constraints for mean temperature
models_grid <- expand_grid(
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

# Fit ED visit models
fit_models_for_supp_fig(
  models_grid = models_grid,
  sensitivity = 'main',
  subset_var = 'full',
  subset = 'full',
  outcome = 'ed',
  temp_analysis = 'full',
  dta = dta_full,
  pop_denom = 'total_pop_all',
  outcome_var = 'ed_count',
  data_source = data_source
)

################################################################################-
## 1b: Fit all deaths models ----
################################################################################-

# Fit death models
fit_models_for_supp_fig(
  models_grid = models_grid,
  sensitivity = 'main',
  subset_var = 'full',
  subset = 'full',
  outcome = 'death',
  temp_analysis = 'full',
  dta = dta_full,
  pop_denom = 'total_pop_all',
  outcome_var = 'death_count',
  data_source = data_source
)

################################################################################-
## 2: Clean up ----
################################################################################-

rm(dta_full, models_grid)
