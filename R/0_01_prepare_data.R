################################################################################-
# Run all files to prepare data for analysis ----
# Comparing HeatRisk and temperature for local heat surveillance: A time-stratified 
#  case-crossover study of emergency department visits in San Mateo County, 
#  California, 2017–2025
# Juliana GE Bartels
# Updated: 4/30/2026
################################################################################-

# This file runs scripts to prepare data for analysis. 

################################################################################-
## 0: Set timer ---- 
################################################################################-

tic("Data preparation complete.")

################################################################################-
## 1: Run data preparation scripts ---- 
################################################################################-

# Prepare raw data by calling all scripts for data preparation

# Takes about 20 minutes
if(data_source == 'original') {
  source(here::here('R', '1_01_download_gridmet_data.R'))
  source(here::here('R', '1_02_prepare_gridmet_zips.R'))
  source(here::here('R', '1_03_prepare_vrbis_zips.R')) # requires manual password entry
  source(here::here('R', '1_04_prepare_essence_ed_visits.R')) # requires manual password entry
  source(here::here('R', '1_05_prepare_denominators.R')) # requires manual password entry
  source(here::here('R', '1_06_combine_death_ed_visits.R'))
  source(here::here('R', '1_08_assign_exposures.R'))
}

if(data_source == 'synth') {
  source(here::here('R', '1_01_download_gridmet_data.R'))
  source(here::here('R', '1_02_prepare_gridmet_zips.R'))
  source(here::here('R', '1_05_prepare_denominators.R')) # requires manual password entry
  source(here::here('R', '1_07_create_synthetic_outcome_data.R'))
  source(here::here('R', '1_08_assign_exposures.R'))
}

################################################################################-
## 0: End timer ---- 
################################################################################-

toc()
