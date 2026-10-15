################################################################################-
# Fit EMM models for ED visit analysis ----
# Comparing HeatRisk and temperature for local heat surveillance: A time-stratified 
#  case-crossover study of emergency department visits in San Mateo County, 
#  California, 2017–2025
# Juliana GE Bartels
# Updated: 6/2/2026
################################################################################-

# This file fits stratified models for daily ED visit count analysis by age group,
# sex, region, cause of ED visit, cause of ED visit by age group, and hospital facility. 

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
  outcome = ('ed'),
  subset_var = c('sex'),
  subset = c('m', 
             'f'),
  temp_var = c('tmean', 
               'heat_risk')
) %>% 
  mutate(pop_denom = paste0('total_pop_', subset), 
         outcome_var = paste0('ed_count_', subset_var, '_', subset)
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
  outcome = ('ed'),
  subset_var = c('age'),
  subset = c('under5', 
             '5to17', 
             '18to64', 
             '65plus'),
  temp_var = c('tmean', 
               'heat_risk')
) %>% 
  mutate(pop_denom = paste0('total_pop_', subset), 
         outcome_var = paste0('ed_count_', subset_var, '_', subset)
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
  outcome = ('ed'),
  subset_var = c('region'),
  subset = c('coastside', 
             'mid',
             'north',
             'south'),
  temp_var = c('tmean', 
               'heat_risk')
) %>% 
  mutate(pop_denom = paste0('total_pop_', subset), 
         outcome_var = paste0('ed_count_', subset_var, '_', subset)
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
## 1d: Run cause EMM models with selected constraints ----
################################################################################-

# Create grid of cause model variables
cause_model_constraints <- expand_grid(
  outcome = ('ed'),
  subset_var = c('cause'),
  subset = c('cardiovascular',
             'respiratory',
             'stroke',
            #'intentinjury', # numbers too small
             'unintentinjury',
            #'heat', # numbers too small
             'substance',
             'mental',
             'renal',
             'genitourinary'),
  temp_var = c('tmean', 
               'heat_risk')
) %>% 
  mutate(pop_denom = paste0('total_pop_all'), 
         outcome_var = paste0('ed_count_', subset_var, '_', subset)
  )

# Get estimates from main model with selected constraints
fit_model_w_selected_constraints(
  sensitivity = 'main',
  temp_analysis = 'full',
  dta = dta_full,
  model_var_grid = cause_model_constraints,
  data_source = data_source
)

################################################################################-
## 1e: Run cause by age EMM models with selected constraints ----
################################################################################-

### For causes within under 5 ----
# Create grid of cause model variables
cause_model_constraints_under5 <- expand_grid(
  outcome = ('ed'),
  subset_var = c('causeunder5'),
  subset = c(#'cardiovascular', # numbers too small
             'respiratory',
             #'stroke', # numbers too small
             #'intentinjury', # numbers too small
             'unintentinjury',
             #'heat', # numbers too small
             #'substance', # numbers too small
             #'mental', # numbers too small
             #'renal', # numbers too small
             'genitourinary'),
  temp_var = c('tmean', 
               'heat_risk')
) %>% 
  mutate(pop_denom = paste0('total_pop_under5'), 
         outcome_var = paste0('ed_count_', subset_var, '_', subset)
  )

# Get estimates from main model with selected constraints
fit_model_w_selected_constraints(
  sensitivity = 'main',
  temp_analysis = 'full',
  dta = dta_full,
  model_var_grid = cause_model_constraints_under5,
  data_source = data_source
)

### For causes within 5 to 17 ----
# Create grid of cause model variables
cause_model_constraints_5to17 <- expand_grid(
  outcome = ('ed'),
  subset_var = c('cause5to17'),
  subset = c('cardiovascular',
             'respiratory',
             #'stroke',
             #'intentinjury',
             'unintentinjury',
             #'heat',
             'substance',
             'mental',
             #'renal',
             'genitourinary'),
  temp_var = c('tmean', 
               'heat_risk')
  ) %>% 
  mutate(pop_denom = paste0('total_pop_5to17'), 
         outcome_var = paste0('ed_count_', subset_var, '_', subset)
  )

# Get estimates from main model with selected constraints
fit_model_w_selected_constraints(
  sensitivity = 'main',
  temp_analysis = 'full',
  dta = dta_full,
  model_var_grid = cause_model_constraints_5to17,
  data_source = data_source
)

### For causes within 18 to 64 ----
# Create grid of cause model variables
cause_model_constraints_18to64 <- expand_grid(
  outcome = ('ed'),
  subset_var = c('cause18to64'),
  subset = c('cardiovascular',
             'respiratory',
             'stroke',
             #'intentinjury',
             'unintentinjury',
             #'heat',
             'substance',
             'mental',
             'renal',
             'genitourinary'),
  temp_var = c('tmean', 
               'heat_risk')
) %>% 
  mutate(pop_denom = paste0('total_pop_18to64'), 
         outcome_var = paste0('ed_count_', subset_var, '_', subset)
  )

# Get estimates from main model with selected constraints
fit_model_w_selected_constraints(
  sensitivity = 'main',
  temp_analysis = 'full',
  dta = dta_full,
  model_var_grid = cause_model_constraints_18to64,
  data_source = data_source
)

### For causes within 65 plus ----
# Create grid of cause model variables
cause_model_constraints_65plus <- expand_grid(
  outcome = ('ed'),
  subset_var = c('cause65plus'),
  subset = c('cardiovascular',
             'respiratory',
             'stroke',
             #'intentinjury',
             'unintentinjury',
             #'heat',
             'substance',
             'mental',
             'renal',
             'genitourinary'),
  temp_var = c('tmean', 
               'heat_risk')
) %>% 
  mutate(pop_denom = paste0('total_pop_65plus'), 
         outcome_var = paste0('ed_count_', subset_var, '_', subset)
  )

# Get estimates from main model with selected constraints
fit_model_w_selected_constraints(
  sensitivity = 'main',
  temp_analysis = 'full',
  dta = dta_full,
  model_var_grid = cause_model_constraints_65plus,
  data_source = data_source
)

################################################################################-
## 1f: Run hospital EMM models with selected constraints ----
################################################################################-

# Create grid of cause model variables
hosp_model_constraints <- expand_grid(
  outcome = ('ed'),
  subset_var = c('hosp'),
  subset = c('krwc',
             'kssf',
             'mp',
             'smmc',
             'seq', 
             'sdc',
             'stan'
            ),
  temp_var = c('tmean', 
               'heat_risk')
  ) %>% 
  mutate(pop_denom = paste0('total_pop_all'), 
         outcome_var = paste0('ed_count_', subset_var, '_', subset)
  )

# Get estimates from main model with selected constraints
fit_model_w_selected_constraints(
  sensitivity = 'main',
  temp_analysis = 'full',
  dta = dta_full,
  model_var_grid = hosp_model_constraints,
  data_source = data_source
)


}

################################################################################-
## 2: Clean up ----
################################################################################-

rm(dta_full,
   age_model_constraints, sex_model_constraints, region_model_constraints,
   cause_model_constraints)

