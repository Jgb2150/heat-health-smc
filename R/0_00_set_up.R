################################################################################-
# Set up R packages, files, and functions for full analysis ----
# Comparing HeatRisk and temperature for local heat surveillance: A time-stratified 
#  case-crossover study of emergency department visits in San Mateo County, 
#  California, 2017–2025
# Juliana GE Bartels
# Updated: 4/21/2026
################################################################################-

# This file loads packages, sets up folder structure, and sources functions that will be used in the analyses. 

# Code structure adapted from:
#   Elser, H., S. Rowland, et al. Ambient temperature and risk of urinary tract 
#       infection in California: A time-stratified case-crossover study using 
#       electronic health records. Environment International 165, 107303 (2022).
#       https://github.com/s-rowland/DailyTemp-UTI

################################################################################-
## 1: Set data source for run ---- 
################################################################################-

# Set data source.
# The default is original . If you are running a reproducibility test or don't have 
# access to the original data, you can run the synthetic version.

data_source <- 'original'
# data_source <- 'synth'

################################################################################-
## 1: Load packages ---- 
################################################################################-

# Load pacman library to load packages using p_load()
library(pacman)

# Load packages to prepare the data and conduct analysis
p_load(
  tidyverse, lubridate, magrittr, tidyr,
  fst, tictoc,
  gnm, dlnm, splines, MuMIn,
  egg, cowplot,
  sf, terra, exactextractr, janitor, weathermetrics,
  dplyr, data.table, 
  smcepi, #devtools::install_github('San-Mateo-County-Health-Epidemiology/smcepi')
  knitr, labeling, purrr, stringr, 
  dbplyr, MASS,
  tidycensus, tigris, jsonlite,
  ggplot2, ggrepel,
  tools
)

# Change global to set show col types to FALSE
options(readr.show_col_types = FALSE)

################################################################################-
## 1: Create folder structure ----
################################################################################-

# Create directorized folder names 
project_folder <- paste0(print(here::here()), '/')

data_folder <- paste0(project_folder, 'data/')
  raw_folder <- paste0(data_folder, 'raw/')
    tmax_folder <- paste0(raw_folder, 'tmax/')
    tmin_folder <- paste0(raw_folder, 'tmin/')
    rmax_folder <- paste0(raw_folder, 'rmax/')
    rmin_folder <- paste0(raw_folder, 'rmin/')
  intermediate_folder <- paste0(data_folder, 'intermediate/')
  final_folder <- paste0(data_folder, 'prepared/')

R_folder <- paste0(project_folder, 'R/')

functions_folder <- paste0(R_folder, 'functions/')

outPath <- 'outputs'
outputs_folder <- paste0(project_folder, 'outputs', '/')

models_folder <- paste0(outputs_folder, 'models/')
  ed_mod_folder <- paste0(models_folder, 'ed/')
    t_mod_ed_folder <- paste0(ed_mod_folder, 'tmean/')
    hr_mod_ed_folder <- paste0(ed_mod_folder, 'heat_risk/')
  death_mod_folder <- paste0(models_folder, 'death/')
    t_mod_death_folder <- paste0(death_mod_folder, 'tmean/')
    hr_mod_death_folder <- paste0(death_mod_folder, 'heat_risk/')

estimates_folder <- paste0(outputs_folder, 'estimates/')
  ed_est_folder <- paste0(estimates_folder, 'ed/')
    cum_est_ed_folder <- paste0(ed_est_folder, 'cum/')
      t_cum_est_ed_folder <- paste0(cum_est_ed_folder, 'tmean/')
      hr_cum_est_ed_folder <- paste0(cum_est_ed_folder, 'heat_risk/')
    ind_est_ed_folder <- paste0(ed_est_folder, 'ind/')
      t_ind_est_ed_folder <- paste0(ind_est_ed_folder, 'tmean/')
      hr_ind_est_ed_folder <- paste0(ind_est_ed_folder, 'heat_risk/')
  death_est_folder <- paste0(estimates_folder, 'death/')
    cum_est_death_folder <- paste0(death_est_folder, 'cum/')
      t_cum_est_death_folder <- paste0(cum_est_death_folder, 'tmean/')
      hr_cum_est_death_folder <- paste0(cum_est_death_folder, 'heat_risk/')
    ind_est_death_folder <- paste0(death_est_folder, 'ind/')
      t_ind_est_death_folder <- paste0(ind_est_death_folder, 'tmean/')
      hr_ind_est_death_folder <- paste0(ind_est_death_folder, 'heat_risk/')

plots_folder <- paste0(outputs_folder, 'plots/')
  maps_folder <- paste0(plots_folder, 'maps/')
  ed_plots_folder <- paste0(plots_folder, 'ed/')
    cum_plots_ed_folder <- paste0(ed_plots_folder, 'cum/')
      t_cum_plots_ed_folder <- paste0(cum_plots_ed_folder, 'tmean/')
      hr_cum_plots_ed_folder <- paste0(cum_plots_ed_folder, 'heat_risk/')
    ind_plots_ed_folder <- paste0(ed_plots_folder, 'ind/')
      t_ind_plots_ed_folder <- paste0(ind_plots_ed_folder, 'tmean/')
      hr_ind_plots_ed_folder <- paste0(ind_plots_ed_folder, 'heat_risk/')
    an_plots_ed_folder <- paste0(ed_plots_folder, 'an/')
      t_an_plots_ed_folder <- paste0(an_plots_ed_folder, 'tmean/')
      hr_an_plots_ed_folder <- paste0(an_plots_ed_folder, 'heat_risk/')
  death_plots_folder <- paste0(plots_folder, 'death/')
    cum_plots_death_folder <- paste0(death_plots_folder, 'cum/')
      t_cum_plots_death_folder <- paste0(cum_plots_death_folder, 'tmean/')
      hr_cum_plots_death_folder <- paste0(cum_plots_death_folder, 'heat_risk/')
    ind_plots_death_folder <- paste0(death_plots_folder, 'ind/')
      t_ind_plots_death_folder <- paste0(ind_plots_death_folder, 'tmean/')
      hr_ind_plots_death_folder <- paste0(ind_plots_death_folder, 'heat_risk/')
    an_plots_death_folder <- paste0(death_plots_folder, 'an/')
      t_an_plots_death_folder <- paste0(an_plots_death_folder, 'tmean/')
      hr_an_plots_death_folder <- paste0(an_plots_death_folder, 'heat_risk/')

tables_folder <- paste0(outputs_folder, 'tables/')
manuscript_folder <- paste0(outputs_folder, 'manuscript/')

# Identify list of folder locations which have just been created above
folder_names <- grep('_folder', names(.GlobalEnv), value = TRUE)

# Create function to create list of folders
# Note: function will not create a folder if it already exists
create_folders <- function(name) {
  ifelse(!dir.exists(get(name)), dir.create(get(name), recursive = TRUE), FALSE)
}

# Create the folders named above
lapply(folder_names, create_folders)

# Clean up
rm(list = ls(pattern = 'folder'))

################################################################################-
## 2: Source functions ----
################################################################################-

# Get names of all function R
my_functions <- list.files(path = here::here('R', 'functions'))

# Create function to run sources
source_functions <- function(function_name) {
  source(here::here('R', 'functions', function_name))
}

# Source all function scripts
a <- map(my_functions, source_functions)

# Clean up
rm(a, my_functions, source_functions)

################################################################################-
## 3a: Create tables for grid search (data_source == 'original') ----
################################################################################-

# In the scripts to run the main models for death and ED visit outcomes, we run grid searches for best model fit

# Create table for mortality model QAIC
if (!file.exists(here::here(outPath, 'tables', 'model_QAIC_death.csv'))) {
  data.frame(
    sensitivity = NA,
    subset_var = NA,
    subset = NA,
    er_constraint = NA,
    er_type = NA,
    lr_type = NA,
    temp_analysis = NA,
    temp_var = NA,
    QAIC = NA,
    run_date = NA
  ) %>%
    write_csv(here::here(outPath, 'tables', 'model_QAIC_death.csv'))
}

# Create table for ED visit model QAIC
if (!file.exists(here::here(outPath, 'tables', 'model_QAIC_ed.csv'))) {
  data.frame(
    sensitivity = NA,
    subset_var = NA,
    subset = NA,
    er_constraint = NA,
    er_type = NA,
    lr_type = NA,
    temp_analysis = NA,
    temp_var = NA,
    QAIC = NA,
    run_date = NA
  ) %>%
    write_csv(here::here(outPath, 'tables', 'model_QAIC_ed.csv'))
}

# Create table of the final selected model constraints for the mortality model
if (!file.exists(here::here(outPath, 'tables', 'selected_constraints_death.csv'))) {
  data.frame(
    sensitivity = NA,
    subset_var = NA,
    subset = NA,
    er_constraint = NA,
    er_type = NA,
    lr_type = NA,   
    temp_analysis = NA,
    temp_var = NA,
    QAIC = NA,
    run_date = NA
  ) %>%
    write_csv(here::here(outPath, 'tables', 'selected_constraints_death.csv'))
}

# Create table of the final selected model constraints for the ED visit model
if (!file.exists(here::here(outPath, 'tables', 'selected_constraints_ed.csv'))) {
  data.frame(
    sensitivity = NA,
    subset_var = NA,
    subset = NA,
    er_constraint = NA,
    er_type = NA,
    lr_type = NA,
    temp_analysis = NA,
    temp_var = NA,
    QAIC = NA,
    run_date = NA
  ) %>%
    write_csv(here::here(outPath, 'tables', 'selected_constraints_ed.csv'))
}

################################################################################-
## 3a: Create tables for grid search (data_source == 'synth') ----
################################################################################-

# In the scripts to run the main models for death and ED visit outcomes, we run grid searches for best model fit

# Create table for mortality model QAIC
if (!file.exists(here::here(outPath, 'tables', 'model_QAIC_death_synth.csv'))) {
  data.frame(
    sensitivity = NA,
    subset_var = NA,
    subset = NA,
    er_constraint = NA,
    er_type = NA,
    lr_type = NA,
    temp_analysis = NA,
    temp_var = NA,
    QAIC = NA,
    run_date = NA
  ) %>%
    write_csv(here::here(outPath, 'tables', 'model_QAIC_death_synth.csv'))
}

# Create table for ED visit model QAIC
if (!file.exists(here::here(outPath, 'tables', 'model_QAIC_ed_synth.csv'))) {
  data.frame(
    sensitivity = NA,
    subset_var = NA,
    subset = NA,
    er_constraint = NA,
    er_type = NA,
    lr_type = NA,
    temp_analysis = NA,
    temp_var = NA,
    QAIC = NA,
    run_date = NA
  ) %>%
    write_csv(here::here(outPath, 'tables', 'model_QAIC_ed_synth.csv'))
}


# Create table of the final selected model constraints for the mortality model
if (!file.exists(here::here(outPath, 'tables', 'selected_constraints_death_synth.csv'))) {
  data.frame(
    sensitivity = NA,
    subset_var = NA,
    subset = NA,
    er_constraint = NA,
    er_type = NA,
    lr_type = NA,   
    temp_analysis = NA,
    temp_var = NA,
    QAIC = NA,
    run_date = NA
  ) %>%
    write_csv(here::here(outPath, 'tables', 'selected_constraints_death_synth.csv'))
}

# Create table of the final selected model constraints for the ED visit model
if (!file.exists(here::here(outPath, 'tables', 'selected_constraints_ed_synth.csv'))) {
  data.frame(
    sensitivity = NA,
    subset_var = NA,
    subset = NA,
    er_constraint = NA,
    er_type = NA,
    lr_type = NA,
    temp_analysis = NA,
    temp_var = NA,
    QAIC = NA,
    run_date = NA
  ) %>%
    write_csv(here::here(outPath, 'tables', 'selected_constraints_ed_synth.csv'))
}

################################################################################-
## 4a: Create table of attributable number estimates (data_source == 'original') ----
################################################################################-

# Attributable number and fraction are estimated in model runs and are input into this table template

if (!file.exists(here::here(outPath, 'estimates', 'an_calc_estimates.csv'))) {
  data.frame(
    model_name = NA,
    outcome_var = NA,
    outcome = NA,
    count = NA,
    subset_var = NA,
    subset = NA,
    temp_analysis = NA,
    temp_var = NA,
    
    cen = NA,
    mmt = NA,
    median = NA,
    perc_95 = NA,
    perc_05 = NA,
    num_cases = NA,
    
    an_all_est = NA,
    an_all_lci = NA,
    an_all_med = NA,
    an_all_uci = NA,
    af_all_est = NA,
    af_all_lci = NA,
    af_all_med = NA,
    af_all_uci = NA,
    an_heat_est = NA,
    an_heat_lci = NA,
    an_heat_med = NA,
    an_heat_uci = NA, 
    af_heat_est = NA,
    af_heat_lci = NA,
    af_heat_med = NA,
    af_heat_uci = NA,
    an_cold_est = NA,
    an_cold_lci = NA,
    an_cold_med = NA,
    an_cold_uci = NA,
    af_cold_est = NA,
    af_cold_lci = NA,
    af_cold_med = NA,
    af_cold_uci = NA,
    an_ext_heat_est = NA,
    an_ext_heat_lci = NA,
    an_ext_heat_med = NA,
    an_ext_heat_uci = NA, 
    af_ext_heat_est = NA,
    af_ext_heat_lci = NA,
    af_ext_heat_med = NA,
    af_ext_heat_uci = NA,
    an_ext_cold_est = NA,
    an_ext_cold_lci = NA,
    an_ext_cold_med = NA,
    an_ext_cold_uci = NA,
    af_ext_cold_est = NA,
    af_ext_cold_lci = NA,
    af_ext_cold_med = NA,
    af_ext_cold_uci = NA,
    run_date = NA
  ) %>%
    write_csv(here::here(outPath, 'estimates', 'an_calc_estimates.csv'))
}

################################################################################-
## 4a: Create table of attributable number estimates (data_source == 'synth') ----
################################################################################-

# Attributable number and fraction are estimated in model runs and are input into this table template

if (!file.exists(here::here(outPath, 'estimates', 'an_calc_estimates_synth.csv'))) {
  data.frame(
    model_name = NA,
    outcome_var = NA,
    outcome = NA,
    count = NA,
    subset_var = NA,
    subset = NA,
    temp_analysis = NA,
    temp_var = NA,
    
    cen = NA,
    mmt = NA,
    median = NA,
    perc_95 = NA,
    perc_05 = NA,
    num_cases = NA,
    
    an_all_est = NA,
    an_all_lci = NA,
    an_all_med = NA,
    an_all_uci = NA,
    af_all_est = NA,
    af_all_lci = NA,
    af_all_med = NA,
    af_all_uci = NA,
    an_heat_est = NA,
    an_heat_lci = NA,
    an_heat_med = NA,
    an_heat_uci = NA, 
    af_heat_est = NA,
    af_heat_lci = NA,
    af_heat_med = NA,
    af_heat_uci = NA,
    an_cold_est = NA,
    an_cold_lci = NA,
    an_cold_med = NA,
    an_cold_uci = NA,
    af_cold_est = NA,
    af_cold_lci = NA,
    af_cold_med = NA,
    af_cold_uci = NA,
    an_ext_heat_est = NA,
    an_ext_heat_lci = NA,
    an_ext_heat_med = NA,
    an_ext_heat_uci = NA, 
    af_ext_heat_est = NA,
    af_ext_heat_lci = NA,
    af_ext_heat_med = NA,
    af_ext_heat_uci = NA,
    an_ext_cold_est = NA,
    an_ext_cold_lci = NA,
    an_ext_cold_med = NA,
    an_ext_cold_uci = NA,
    af_ext_cold_est = NA,
    af_ext_cold_lci = NA,
    af_ext_cold_med = NA,
    af_ext_cold_uci = NA,
    run_date = NA
  ) %>%
    write_csv(here::here(outPath, 'estimates', 'an_calc_estimates_synth.csv'))
}

