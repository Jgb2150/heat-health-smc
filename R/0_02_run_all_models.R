################################################################################-
# Run all models for analysis ----
# Comparing HeatRisk and temperature for local heat surveillance: A time-stratified 
#  case-crossover study of emergency department visits in San Mateo County, 
#  California, 2017–2025 
# Juliana GE Bartels
# Updated: 9/30/2026
################################################################################-

# This file runs all model scripts. 

################################################################################-
## 0: Set timer ---- 
################################################################################-

# This takes about 4 hours.
tic("Model runs complete.")

################################################################################-
## 1: Run all model scripts for analysis ---- 
################################################################################-

# Run all models

# ED models
source(here::here('R', '2_01a_fit_main_model_ed.R'))
source(here::here('R', '2_02a_fit_emm_models_ed.R'))
source(here::here('R', '2_03a_fit_sens_model_ed.R'))

# Death models
source(here::here('R', '2_01b_fit_main_model_death.R'))
source(here::here('R', '2_02b_fit_emm_models_death.R'))
source(here::here('R', '2_03b_fit_sens_model_death.R'))

# Supplemental model with all E-Rs
source(here::here('R', '2_04_run_model_supp_all_ers.R'))

################################################################################-
## 0: End timer ---- 
################################################################################-

toc()
