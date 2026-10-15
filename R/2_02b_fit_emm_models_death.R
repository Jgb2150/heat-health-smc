################################################################################-
# Fit EMM models for mortality analysis ----
# Comparing HeatRisk and temperature for local heat surveillance: A time-stratified 
#  case-crossover study of emergency department visits in San Mateo County, 
#  California, 2017–2025
# Juliana GE Bartels
# Updated: 6/2/2026
################################################################################-

# This file fits stratified models for daily death count analysis by age group,
# sex, region, and cause of death. 

################################################################################-
## 0: Upload data ----
################################################################################-

## Load dataset of deaths and ED visits matched with temperature exposures ----
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
## 1a: Run sex EMM models with selected constraints ----
################################################################################-

# Create grid of sex model variables
sex_model_constraints <- expand_grid(
  outcome = ('death'),
  subset_var = c('sex'),
  subset = c('m', 
             'f'),
  temp_var = c('tmean', 
               'heat_risk')
) %>% 
  mutate(pop_denom = paste0('total_pop_', subset), 
         outcome_var = paste0('death_count_', subset_var, '_', subset)
  )

# Get estimates from main model with selected constraints
fit_model_w_selected_constraints(
  sensitivity = 'main',
  temp_analysis = 'full',
  dta = dta_full,
  model_var_grid = sex_model_constraints,
  data_source = data_source
)

################################################################################-
## Only run analyses below if data_source == 'original'
################################################################################-

if(data_source == 'original') {
  
################################################################################-
## 1b: Run age group EMM models with selected constraints ----
################################################################################-

# Create grid of age model variables
age_model_constraints <- expand_grid(
  outcome = ('death'),
  subset_var = c('age'),
  subset = c('under5', 
             #'5to17', # numbers too small
             '18to64', 
             '65plus'
             ),
  temp_var = c('tmean', 
               'heat_risk')
) %>% 
  mutate(pop_denom = paste0('total_pop_', subset), 
         outcome_var = paste0('death_count_', subset_var, '_', subset)
  )

# Get estimates from main model with selected constraints
fit_model_w_selected_constraints(
  sensitivity = 'main',
  temp_analysis = 'full',
  dta = dta_full,
  model_var_grid = age_model_constraints,
  data_source = data_source
)

################################################################################-
## 1c: Run region EMM models with selected constraints ----
################################################################################-

# Create grid of region model variables
region_model_constraints <- expand_grid(
  outcome = ('death'),
  subset_var = c('region'),
  subset = c('coastside', 
             'mid',
             'north',
             'south'),
  temp_var = c('tmean', 
               'heat_risk')
) %>% 
  mutate(pop_denom = paste0('total_pop_', subset), 
         outcome_var = paste0('death_count_', subset_var, '_', subset)
  )

# Get estimates from main model with selected constraints
fit_model_w_selected_constraints(
  sensitivity = 'main',
  temp_analysis = 'full',
  dta = dta_full,
  model_var_grid = region_model_constraints,
  data_source = data_source
)

################################################################################-
## 1d: Run cause of mortality EMM models with selected constraints ----
################################################################################-

# Create grid of cause model variables
cause_model_constraints <- expand_grid(
  outcome = ('death'),
  subset_var = c('cause'),
  subset = c('cardiovascular',
             'respiratory',
             'stroke',
             'intentinjury',
             'unintentinjury',
             #'heat', # numbers too small
             'substance',
             #'mental', # numbers too small
             'renal',
             'genitourinary'),
  temp_var = c('tmean', 
               'heat_risk')
) %>% 
  mutate(pop_denom = paste0('total_pop_all'), 
         outcome_var = paste0('death_count_', subset_var, '_', subset)
  )

# Get estimates from main model with selected constraints
fit_model_w_selected_constraints(
  sensitivity = 'main',
  temp_analysis = 'full',
  dta = dta_full,
  model_var_grid = cause_model_constraints,
  data_source = data_source
)

}

################################################################################-
## 2: Clean up ----
################################################################################-

rm(dta_full,
   age_model_constraints, sex_model_constraints, region_model_constraints,
   cause_model_constraints)

