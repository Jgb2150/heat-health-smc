################################################################################-
# Plot: Individual lags 0-6 for mean temperature and HeatRisk, ED visits, by sex ----
# Comparing HeatRisk and temperature for local heat surveillance: A time-stratified 
#  case-crossover study of emergency department visits in San Mateo County, 
#  California, 2017–2025 
# Juliana GE Bartels
# Updated: 8/26/2026
################################################################################-

# This file plots the lagged percent change in ED visit rates for 0-6 days following 
# exposure to temperature and HeatRisk, by sex.  

################################################################################-
## 0: Upload data ----
################################################################################-

# Generate suffix for file calling and writing, based on data source
data_source_name <- case_when(data_source == 'original' ~ '',
                              data_source == 'synth' ~ '_synth')

# Load estimates and models for sex EMMs
selected_constraints_ed <- read_csv(here::here('outputs', 'tables', paste0('selected_constraints_ed', data_source_name, '.csv')) ) %>%
  filter(sensitivity == 'main')

sex_grid <- expand_grid(
  subset = c('f', 'm'),
  temp_var = c('tmean', 'heat_risk')) %>%
  mutate(estimate_name = paste0('estimates_', subset, '_', temp_var),
         mod_name = paste0('mod_', subset, '_', temp_var))

load_estimates(selected_constraints_ed, 'ed', 'sex', sex_grid, 'ind', data_source_name)

################################################################################-
## 1a: Plot relative risk over 6 days lag (female) ----
################################################################################-

### Mean temperature ----
# Plot 6 days of lag relative risk of ed for the 95th percentile of temperature compared to the median observed temperature
plot_f_tmean_per95 <- plot_ind_tmean(lag_num = 6,
                                     mod = mod_f_tmean, 
                                     outcome = 'ed',
                                     moa = 'rr',
                                     percentile = 'per_95',
                                     temp_var = 'tmean', 
                                     estimates = estimates_f_tmean, 
                                     subset = 'female residents '
)

ggsave(filename = here::here('outputs', 'plots', 'ed', 'ind', 'tmean', paste0('ind_lag6_f_ed_per95', data_source_name, '.png')), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of ed for the 95th percentile of temperature compared to the median observed temperature
plot_f_tmean_per95_pct_chg <- plot_ind_tmean(lag_num = 6,
                                             mod = mod_f_tmean, 
                                             outcome = 'ed',
                                             moa = 'pct_chg',
                                             percentile = 'per_95',
                                             temp_var = 'tmean', 
                                             estimates = estimates_f_tmean, 
                                             subset = 'female residents '
)

ggsave(filename = here::here('outputs', 'plots', 'ed', 'ind', 'tmean', paste0('ind_lag6_f_ed_per95_pct_chg', data_source_name, '.png')), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of ed for the 5th percentile of temperature compared to the median observed temperature
plot_f_tmean_per05 <- plot_ind_tmean(lag_num = 6,
                                     mod = mod_f_tmean, 
                                     outcome = 'ed',
                                     moa = 'rr',
                                     percentile = 'per_05',
                                     temp_var = 'tmean', 
                                     estimates = estimates_f_tmean, 
                                     subset = 'female residents '
)

ggsave(filename = here::here('outputs', 'plots', 'ed', 'ind', 'tmean', paste0('ind_lag6_f_ed_per05', data_source_name, '.png')), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of ed for the 5th percentile of temperature compared to the median observed temperature
plot_f_tmean_per05_pct_chg <- plot_ind_tmean(lag_num = 6,
                                             mod = mod_f_tmean, 
                                             outcome = 'ed',
                                             moa = 'pct_chg',
                                             percentile = 'per_05',
                                             temp_var = 'tmean', 
                                             estimates = estimates_f_tmean, 
                                             subset = 'female residents '
)

ggsave(filename = here::here('outputs', 'plots', 'ed', 'ind', 'tmean', paste0('ind_lag6_f_ed_per05_pct_chg', data_source_name, '.png')), 
       height = 8, width = 12 )

### HeatRisk ----
# Plot 6 days of lag relative risk of ed for HR3 compared to HR0
plot_f_hr3 <- plot_ind_hr(lag_num = 6,
                          mod = mod_f_heat_risk, 
                          outcome = 'ed',
                          moa = 'rr',
                          hr_level = '3',
                          estimates = estimates_f_heat_risk, 
                          subset = 'female residents '
)

ggsave(filename = here::here('outputs', 'plots', 'ed', 'ind', 'heat_risk', paste0('ind_lag6_f_ed_hr3', data_source_name, '.png')), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of ed for HR3 compared to HR0
plot_f_hr3_pct_chg <- plot_ind_hr(lag_num = 6,
                                  mod = mod_f_heat_risk, 
                                  outcome = 'ed',
                                  moa = 'pct_chg',
                                  hr_level = '3',
                                  estimates = estimates_f_heat_risk, 
                                  subset = 'female residents '
)

ggsave(filename = here::here('outputs', 'plots', 'ed', 'ind', 'heat_risk', paste0('ind_lag6_f_ed_hr3_pct_chg', data_source_name, '.png')), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of ed for HR 2 compared to 0
plot_f_hr2 <- plot_ind_hr(lag_num = 6,
                          mod = mod_f_heat_risk, 
                          outcome = 'ed',
                          moa = 'rr',
                          hr_level = '2',
                          estimates = estimates_f_heat_risk, 
                          subset = 'female residents '
)

ggsave(filename = here::here('outputs', 'plots', 'ed', 'ind', 'heat_risk', paste0('ind_lag6_f_ed_hr2', data_source_name, '.png')), 
       height = 8, width = 12 )


# Plot 6 days of lag relative risk of ed for HR 2 compared to 0
plot_f_hr2_pct_chg <- plot_ind_hr(lag_num = 6,
                                  mod = mod_f_heat_risk, 
                                  outcome = 'ed',
                                  moa = 'pct_chg',
                                  hr_level = '2',
                                  estimates = estimates_f_heat_risk, 
                                  subset = 'female residents '
)

ggsave(filename = here::here('outputs', 'plots', 'ed', 'ind', 'heat_risk', paste0('ind_lag6_f_ed_hr2_pct_chg', data_source_name, '.png')), 
       height = 8, width = 12 )

################################################################################-
## 1b: Plot relative risk over 6 days lag (male) ----
################################################################################-

### Mean temperature ----
# Plot 6 days of lag relative risk of ed for the 95th percentile of temperature compared to the median observed temperature
plot_m_tmean_per95 <- plot_ind_tmean(lag_num = 6,
                                     mod = mod_m_tmean, 
                                     outcome = 'ed',
                                     moa = 'rr',
                                     percentile = 'per_95',
                                     temp_var = 'tmean', 
                                     estimates = estimates_m_tmean, 
                                     subset = 'male residents '
)

ggsave(filename = here::here('outputs', 'plots', 'ed', 'ind', 'tmean', paste0('ind_lag6_m_ed_per95', data_source_name, '.png')), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of ed for the 95th percentile of temperature compared to the median observed temperature
plot_m_tmean_per95_pct_chg <- plot_ind_tmean(lag_num = 6,
                                             mod = mod_m_tmean, 
                                             outcome = 'ed',
                                             moa = 'pct_chg',
                                             percentile = 'per_95',
                                             temp_var = 'tmean', 
                                             estimates = estimates_m_tmean, 
                                             subset = 'male residents '
)

ggsave(filename = here::here('outputs', 'plots', 'ed', 'ind', 'tmean', paste0('ind_lag6_m_ed_per95_pct_chg', data_source_name, '.png')), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of ed for the 5th percentile of temperature compared to the median observed temperature
plot_m_tmean_per05 <- plot_ind_tmean(lag_num = 6,
                                     mod = mod_m_tmean, 
                                     outcome = 'ed',
                                     moa = 'rr',
                                     percentile = 'per_05',
                                     temp_var = 'tmean', 
                                     estimates = estimates_m_tmean, 
                                     subset = 'male residents '
)

ggsave(filename = here::here('outputs', 'plots', 'ed', 'ind', 'tmean', paste0('ind_lag6_m_ed_per05', data_source_name, '.png')), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of ed for the 5th percentile of temperature compared to the median observed temperature
plot_m_tmean_per05_pct_chg <- plot_ind_tmean(lag_num = 6,
                                             mod = mod_m_tmean, 
                                             outcome = 'ed',
                                             moa = 'pct_chg',
                                             percentile = 'per_05',
                                             temp_var = 'tmean', 
                                             estimates = estimates_m_tmean, 
                                             subset = 'residents age in the m-county region '
)

ggsave(filename = here::here('outputs', 'plots', 'ed', 'ind', 'tmean', paste0('ind_lag6_m_ed_per05_pct_chg', data_source_name, '.png')), 
       height = 8, width = 12 )

### HeatRisk ----
# Plot 6 days of lag relative risk of ed for HR3 compared to HR0
plot_m_hr3 <- plot_ind_hr(lag_num = 6,
                          mod = mod_m_heat_risk, 
                          outcome = 'ed',
                          moa = 'rr',
                          hr_level = '3',
                          estimates = estimates_m_heat_risk, 
                          subset = 'male residents '
)

ggsave(filename = here::here('outputs', 'plots', 'ed', 'ind', 'heat_risk', paste0('ind_lag6_m_ed_hr3', data_source_name, '.png')), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of ed for HR3 compared to HR0
plot_m_hr3_pct_chg <- plot_ind_hr(lag_num = 6,
                                  mod = mod_m_heat_risk, 
                                  outcome = 'ed',
                                  moa = 'pct_chg',
                                  hr_level = '3',
                                  estimates = estimates_m_heat_risk, 
                                  subset = 'male residents '
)

ggsave(filename = here::here('outputs', 'plots', 'ed', 'ind', 'heat_risk', paste0('ind_lag6_m_ed_hr3_pct_chg', data_source_name, '.png')), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of ed for HR 2 compared to 0
plot_m_hr2 <- plot_ind_hr(lag_num = 6,
                          mod = mod_m_heat_risk, 
                          outcome = 'ed',
                          moa = 'rr',
                          hr_level = '2',
                          estimates = estimates_m_heat_risk, 
                          subset = 'male residents '
)

ggsave(filename = here::here('outputs', 'plots', 'ed', 'ind', 'heat_risk', paste0('ind_lag6_m_ed_hr2', data_source_name, '.png')), 
       height = 8, width = 12 )


# Plot 6 days of lag relative risk of ed for HR 2 compared to 0
plot_m_hr2_pct_chg <- plot_ind_hr(lag_num = 6,
                                  mod = mod_m_heat_risk, 
                                  outcome = 'ed',
                                  moa = 'pct_chg',
                                  hr_level = '2',
                                  estimates = estimates_m_heat_risk, 
                                  subset = 'male residents '
)

ggsave(filename = here::here('outputs', 'plots', 'ed', 'ind', 'heat_risk', paste0('ind_lag6_m_ed_hr2_pct_chg', data_source_name, '.png')), 
       height = 8, width = 12 )

################################################################################-
## 2: Clean up ----
################################################################################-

rm(data_source_name,
   selected_constraints_ed, sex_grid,
   estimates_f_tmean, estimates_f_heat_risk,
   estimates_m_tmean, estimates_m_heat_risk,
   mod_f_tmean, mod_f_heat_risk,
   mod_m_tmean, mod_m_heat_risk,
   plot_f_tmean_per95,
   plot_m_tmean_per95,
   plot_f_tmean_per95_pct_chg,
   plot_m_tmean_per95_pct_chg,
   plot_f_tmean_per05,
   plot_m_tmean_per05,
   plot_f_tmean_per05_pct_chg,
   plot_m_tmean_per05_pct_chg,
   plot_f_hr3,
   plot_m_hr3,
   plot_f_hr3_pct_chg,
   plot_m_hr3_pct_chg,
   plot_f_hr2,
   plot_m_hr2,
   plot_f_hr2_pct_chg,
   plot_m_hr2_pct_chg
)

