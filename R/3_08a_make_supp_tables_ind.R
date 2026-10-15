################################################################################-
# Make supplementary tables of individual percent change estimates for mean temperature and HeatRisk ----
# Comparing HeatRisk and temperature for local heat surveillance: A time-stratified 
#  case-crossover study of emergency department visits in San Mateo County, 
#  California, 2017–2025
# Juliana GE Bartels
# Updated: 9/18/2026
################################################################################-

# This file makes tables of estimates for the supplement.

################################################################################-
## 0: Upload data ----
################################################################################-

# Load cumulative ED estimates for mean temperature (quadratic model)
estimates_tmean_ed <- list(full = read_csv(here::here('outputs', 'estimates', 'ed', 'ind', 'tmean', 'est_ind_ed_main_full_full_a_quad_ns_full_tmean.csv')))
estimates_tmean_ed_nocov <- list(full = read_csv(here::here('outputs', 'estimates', 'ed', 'ind', 'tmean', 'est_ind_ed_nocov_full_full_a_quad_ns_full_tmean.csv')))
estimates_tmean_ed_restricthosp <- list(full = read_csv(here::here('outputs', 'estimates', 'ed', 'ind', 'tmean', 'est_ind_ed_restricthosp_full_full_a_quad_ns_full_tmean.csv')))

# Load cumulative ED estimates for HeatRisk
estimates_hr_ed <- list(full = read_csv(here::here('outputs', 'estimates', 'ed', 'ind', 'heat_risk', 'est_ind_ed_main_full_full_a_strata_ns_full_heat_risk.csv')))
estimates_hr_ed_nocov <- list(full = read_csv(here::here('outputs', 'estimates', 'ed', 'ind', 'heat_risk', 'est_ind_ed_nocov_full_full_a_strata_ns_full_heat_risk.csv')))
estimates_hr_ed_restricthosp <- list(full = read_csv(here::here('outputs', 'estimates', 'ed', 'ind', 'heat_risk', 'est_ind_ed_restricthosp_full_full_a_strata_ns_full_heat_risk.csv')))

# Load cumulative death estimates for mean temperature (quadratic model)
estimates_tmean_death <- list(full = read_csv(here::here('outputs', 'estimates', 'death', 'ind', 'tmean', 'est_ind_death_main_full_full_a_quad_ns_full_tmean.csv')))
estimates_tmean_death_nocov <- list(full = read_csv(here::here('outputs', 'estimates', 'death', 'ind', 'tmean', 'est_ind_death_nocov_full_full_a_quad_ns_full_tmean.csv')))

# Load cumulative death estimates for HeatRisk
estimates_hr_death <- list(full = read_csv(here::here('outputs', 'estimates', 'death', 'ind', 'heat_risk', 'est_ind_death_main_full_full_a_strata_ns_full_heat_risk.csv')))
estimates_hr_death_nocov <- list(full = read_csv(here::here('outputs', 'estimates', 'death', 'ind', 'heat_risk', 'est_ind_death_nocov_full_full_a_strata_ns_full_heat_risk.csv')))

################################################################################-
## 1a: Make supplemental table of ED mean temperature ----
################################################################################-

format_supp_table_ind_tmean(estimates_tmean_ed) %>% 
  write_csv(., here::here('outputs', 'tables', 'supp_table_tmean_ed_ind.csv'))

format_supp_table_ind_tmean(estimates_tmean_ed_nocov) %>%
  write_csv(., here::here('outputs', 'tables', 'supp_table_tmean_ed_ind_nocov.csv'))

format_supp_table_ind_tmean(estimates_tmean_ed_restricthosp) %>%
  write_csv(., here::here('outputs', 'tables', 'supp_table_tmean_ed_ind_restricthosp.csv'))

################################################################################-
## 1b: Make supplemental table of ED HeatRisk ----
################################################################################-

format_supp_table_ind_hr(estimates_hr_ed) %>%
  write_csv(., here::here('outputs', 'tables', 'supp_table_heat_risk_ed_ind.csv'))

format_supp_table_ind_hr(estimates_hr_ed_nocov) %>%
  write_csv(., here::here('outputs', 'tables', 'supp_table_heat_risk_ed_ind_nocov.csv'))

format_supp_table_ind_hr(estimates_hr_ed_restricthosp) %>%
  write_csv(., here::here('outputs', 'tables', 'supp_table_heat_risk_ed_ind_restricthosp.csv'))

################################################################################-
## 2a: Make supplemental table of ED mean temperature ----
################################################################################-

format_supp_table_ind_tmean(estimates_tmean_death) %>%
  write_csv(., here::here('outputs', 'tables', 'supp_table_tmean_death_ind.csv'))

format_supp_table_ind_tmean(estimates_tmean_death_nocov) %>%
  write_csv(., here::here('outputs', 'tables', 'supp_table_tmean_death_ind_nocov.csv'))

################################################################################-
## 2b: Make supplemental table of death HeatRisk ----
################################################################################-

format_supp_table_ind_hr(estimates_hr_death) %>%
  write_csv(., here::here('outputs', 'tables', 'supp_table_heat_risk_death_ind.csv'))

format_supp_table_ind_hr(estimates_hr_death_nocov) %>%
  write_csv(., here::here('outputs', 'tables', 'supp_table_heat_risk_death_ind_nocov.csv'))

################################################################################-
## 5: Clean up ----
################################################################################-

rm(list = ls())

# reload functions
source(here::here('R', '0_00_set_up.R'))
