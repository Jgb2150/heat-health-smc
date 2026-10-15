################################################################################-
# Plot: Cumulative of lags 0-6 for mean temperatures and HeatRisk, ED visits, restricthosp ----
# Comparing HeatRisk and temperature for local heat surveillance: A time-stratified 
#  case-crossover study of emergency department visits in San Mateo County, 
#  California, 2017–2025 
# Juliana GE Bartels
# Updated: 6/13/2026
################################################################################-

# This file plots cumulative percent change in ED visit rate for lag 0-6 days,
# rstricthosp model. 

################################################################################-
## 0: Upload data ----
################################################################################-

# Generate suffix for file calling and writing, based on data source
data_source_name <- case_when(data_source == 'original' ~ '',
                              data_source == 'synth' ~ '_synth')

# Load estimates for restricthosp sensitivity model - mean temperature
selected_constraints_ed <- read_csv(here::here('outputs', 'tables', paste0('selected_constraints_ed', data_source_name, '.csv')) ) %>%
  filter(sensitivity == 'restricthosp')

estimates_tmean <- read_csv(here::here('outputs', 'estimates', 'ed', 'cum', 'tmean',
                                 paste0('est_cum_ed_', 
                                        selected_constraints_ed[2,1], '_', selected_constraints_ed[2,2], '_', 
                                        selected_constraints_ed[2,3], '_', selected_constraints_ed[2,4], '_', 
                                        selected_constraints_ed[2,5], '_', selected_constraints_ed[2,6], '_', 
                                        selected_constraints_ed[2,7], '_', 'tmean', data_source_name, 
                                        '.csv')))

## Load restricthosp sensitivity model - mean temperature
mod_tmean <- readRDS(here::here('outputs', 'models', 'ed', 'tmean', 
                       paste0('ed_', selected_constraints_ed[2,1], '_', selected_constraints_ed[2,2], '_', 
                       selected_constraints_ed[2,3], '_', selected_constraints_ed[2,4], '_', 
                       selected_constraints_ed[2,5], '_', selected_constraints_ed[2,6], '_', 
                       selected_constraints_ed[2,7],'_', 'tmean', data_source_name, 
                       '.RDS')))

# Load estimates for restricthosp sensitivity model - HeatRisk
estimates_heat_risk <- read_csv(here::here('outputs', 'estimates', 'ed', 'cum', 'heat_risk',
                                       paste0('est_cum_ed_', 
                                              selected_constraints_ed[1,1], '_', selected_constraints_ed[1,2], '_', 
                                              selected_constraints_ed[1,3], '_', selected_constraints_ed[1,4], '_', 
                                              selected_constraints_ed[1,5], '_', selected_constraints_ed[1,6], '_', 
                                              selected_constraints_ed[1,7], '_', 'heat_risk', data_source_name,
                                              '.csv')))

## Load restricthosp sensitivity model - HeatRisk
mod_heat_risk <- readRDS(here::here('outputs', 'models', 'ed', 'heat_risk', 
                                paste0('ed_', selected_constraints_ed[1,1], '_', selected_constraints_ed[1,2], '_', 
                                       selected_constraints_ed[1,3], '_', selected_constraints_ed[1,4], '_', 
                                       selected_constraints_ed[1,5], '_', selected_constraints_ed[1,6], '_', 
                                       selected_constraints_ed[1,7],'_', 'heat_risk', data_source_name,
                                       '.RDS')))

# Set number of lags for cumulative plots
lag_num <- 6

################################################################################-
## 1a: Plot cumulative relative risk for temperatures compared to median, mean temperature ----
################################################################################-

# Plot cumulative relative risk of ED visit for all temperatures compared to the median observed temperature
plot_cum_tmean(lag_num = lag_num, 
               mod = mod_tmean, 
               outcome = 'ED visit',
               moa = 'rr',
               temp_var = 'tmean', 
               estimates = estimates_tmean, 
               subset = 'residents ',
               with_hist = TRUE)

ggsave(filename = here::here('outputs', 'plots', 'ed', 'cum', 'tmean', paste0('cum_lag', lag_num, '_full_ed_rr_restricthosp', data_source_name, '.png')), 
       height = 8, width = 12 )

################################################################################-
## 1b: Plot cumulative percent change in ED visits for temperatures compared to median, mean temperature ----
################################################################################-

# Plot cumulative relative risk of ED visit for all temperatures compared to the median observed temperature
plot_cum_tmean(lag_num = lag_num, 
               mod = mod_tmean, 
               outcome = 'ed',
               moa = 'pct_chg',
               temp_var = 'tmean', 
               estimates = estimates_tmean, 
               subset = 'residents ',
               with_hist = TRUE)

ggsave(filename = here::here('outputs', 'plots', 'ed', 'cum', 'tmean', paste0('cum_lag', lag_num, '_full_ed_pct_chg_restricthosp', data_source_name, '.png')), 
       height = 8, width = 12 )

################################################################################-
## 2a: Plot cumulative relative risk for HR levels compared to HR 0, HeatRisk ----
################################################################################-

# Plot cumulative relative risk of ED visit for all HR levels compared to HR 0
plot_cum_hr(lag_num = lag_num, 
                   mod = mod_heat_risk, 
                   outcome = 'ED visit',
                   moa = 'rr',
                   temp_var = 'heat_risk', 
                   estimates = estimates_heat_risk, 
                   subset = 'residents ',
                   with_hist = TRUE)

ggsave(filename = here::here('outputs', 'plots', 'ed', 'cum', 'heat_risk', paste0('cum_lag', lag_num, '_full_ed_rr_restricthosp', data_source_name, '.png')), 
       height = 8, width = 12 )

################################################################################-
## 2b: Plot cumulative percent change for HR levels compared to HR 0, HeatRisk ----
################################################################################-

# Plot cumulative relative risk of ED visit for all HR levels compared to HR 0
plot_cum_hr(lag_num = lag_num, 
                   mod = mod_heat_risk, 
                   outcome = 'ED visit',
                   moa = 'pct_chg',
                   temp_var = 'heat_risk', 
                   estimates = estimates_heat_risk, 
                   subset = 'residents ',
                   with_hist = TRUE)

ggsave(filename = here::here('outputs', 'plots', 'ed', 'cum', 'heat_risk', paste0('cum_lag', lag_num, '_full_ed_pct_chg_restricthosp', data_source_name, '.png')), 
       height = 8, width = 12 )

################################################################################-
## 3: Clean up ----
################################################################################-

rm(data_source_name, selected_constraints_ed, estimates_tmean, estimates_heat_risk, mod_tmean, mod_heat_risk)

