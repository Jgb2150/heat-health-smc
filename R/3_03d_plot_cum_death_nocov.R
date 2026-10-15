################################################################################-
# Plot: Cumulative of lags 0-6 for mean temperatures and HeatRisk, deaths, no covid ----
# Comparing HeatRisk and temperature for local heat surveillance: A time-stratified 
#  case-crossover study of emergency department visits in San Mateo County, 
#  California, 2017–2025 
# Juliana GE Bartels
# Updated: 6/13/2026
################################################################################-

# This file plots cumulative percent change in death rate for lag 0-6 days,
# no covid model. 

################################################################################-
## 0: Upload data ----
################################################################################-

# Generate suffix for file calling and writing, based on data source
data_source_name <- case_when(data_source == 'original' ~ '',
                              data_source == 'synth' ~ '_synth')

# Load estimates for no covid years (2020, 2021) sensitivity model - mean temperature
selected_constraints_death <- read_csv(here::here('outputs', 'tables', paste0('selected_constraints_death', data_source_name, '.csv')) ) %>%
  filter(sensitivity == 'nocov')

estimates_tmean <- read_csv(here::here('outputs', 'estimates', 'death', 'cum', 'tmean',
                                 paste0('est_cum_death_', 
                                        selected_constraints_death[2,1], '_', selected_constraints_death[2,2], '_', 
                                        selected_constraints_death[2,3], '_', selected_constraints_death[2,4], '_', 
                                        selected_constraints_death[2,5], '_', selected_constraints_death[2,6], '_', 
                                        selected_constraints_death[2,7], '_', 'tmean', data_source_name, 
                                        '.csv')))

## Load no covid years (2020, 2021) sensitivity model - mean temperature
mod_tmean <- readRDS(here::here('outputs', 'models', 'death', 'tmean', 
                       paste0('death_', selected_constraints_death[2,1], '_', selected_constraints_death[2,2], '_', 
                       selected_constraints_death[2,3], '_', selected_constraints_death[2,4], '_', 
                       selected_constraints_death[2,5], '_', selected_constraints_death[2,6], '_', 
                       selected_constraints_death[2,7],'_', 'tmean', data_source_name, 
                       '.RDS')))

# Load estimates for no covid years (2020, 2021) sensitivity model - HeatRisk
estimates_heat_risk <- read_csv(here::here('outputs', 'estimates', 'death', 'cum', 'heat_risk',
                                       paste0('est_cum_death_', 
                                              selected_constraints_death[1,1], '_', selected_constraints_death[1,2], '_', 
                                              selected_constraints_death[1,3], '_', selected_constraints_death[1,4], '_', 
                                              selected_constraints_death[1,5], '_', selected_constraints_death[1,6], '_', 
                                              selected_constraints_death[1,7], '_', 'heat_risk', data_source_name,
                                              '.csv')))

# Load no covid years (2020, 2021) sensitivity model - HeatRisk
mod_heat_risk <- readRDS(here::here('outputs', 'models', 'death', 'heat_risk', 
                                paste0('death_', selected_constraints_death[1,1], '_', selected_constraints_death[1,2], '_', 
                                       selected_constraints_death[1,3], '_', selected_constraints_death[1,4], '_', 
                                       selected_constraints_death[1,5], '_', selected_constraints_death[1,6], '_', 
                                       selected_constraints_death[1,7],'_', 'heat_risk', data_source_name,
                                       '.RDS')))

# Set number of lags for cumulative plots
lag_num <- 6

################################################################################-
## 1a: Plot cumulative relative risk for temperatures compared to median, mean temperature ----
################################################################################-

# Plot cumulative relative risk of death for all temperatures compared to the median observed temperature
plot_cum_tmean(lag_num = lag_num, 
               mod = mod_tmean, 
               outcome = 'death',
               moa = 'rr',
               temp_var = 'tmean', 
               estimates = estimates_tmean, 
               subset = 'residents ',
               with_hist = TRUE)

ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'tmean', paste0('cum_lag', lag_num, '_full_death_rr_nocov', data_source_name, '.png')), 
       height = 8, width = 12 )

################################################################################-
## 1b: Plot cumulative percent change in deaths for temperatures compared to median, mean temperature ----
################################################################################-

# Plot cumulative relative risk of death for all temperatures compared to the median observed temperature
plot_cum_tmean(lag_num = lag_num, 
               mod = mod_tmean, 
               outcome = 'death',
               moa = 'pct_chg',
               temp_var = 'tmean', 
               estimates = estimates_tmean, 
               subset = 'residents ',
               with_hist = TRUE)

ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'tmean', paste0('cum_lag', lag_num, '_full_death_pct_chg_nocov', data_source_name, '.png')), 
       height = 8, width = 12 )

################################################################################-
## 2a: Plot cumulative relative risk for HR levels compared to HR 0, HeatRisk ----
################################################################################-

# Plot cumulative relative risk of death for all HR levels compared to HR 0
plot_cum_hr(lag_num = lag_num, 
                   mod = mod_heat_risk, 
                   outcome = 'death',
                   moa = 'rr',
                   temp_var = 'heat_risk', 
                   estimates = estimates_heat_risk, 
                   subset = 'residents ',
                   with_hist = TRUE)

ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'heat_risk', paste0('cum_lag', lag_num, '_full_death_rr_nocov', data_source_name, '.png')), 
       height = 8, width = 12 )

################################################################################-
## 2b: Plot cumulative percent change for HR levels compared to HR 0, HeatRisk ----
################################################################################-

# Plot cumulative relative risk of death for all HR levels compared to HR 0
plot_cum_hr(lag_num = lag_num, 
                   mod = mod_heat_risk, 
                   outcome = 'death',
                   moa = 'pct_chg',
                   temp_var = 'heat_risk', 
                   estimates = estimates_heat_risk, 
                   subset = 'residents ',
                   with_hist = TRUE)

ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'heat_risk', paste0('cum_lag', lag_num, '_full_death_pct_chg_nocov', data_source_name, '.png')), 
       height = 8, width = 12 )

################################################################################-
## 3: Clean up ----
################################################################################-

rm(data_source_name, selected_constraints_death, estimates_tmean, estimates_heat_risk, mod_tmean, mod_heat_risk)
