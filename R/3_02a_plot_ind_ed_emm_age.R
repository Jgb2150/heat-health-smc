################################################################################-
# Plot: Individual lags 0-6 for mean temperature and HeatRisk, ED visits, by age group ----
# Comparing HeatRisk and temperature for local heat surveillance: A time-stratified 
#  case-crossover study of emergency department visits in San Mateo County, 
#  California, 2017–2025 
# Juliana GE Bartels
# Updated: 8/26/2026
################################################################################-

# This file plots the lagged percent change in ED visit rates for 0-20 days following 
# exposure to temperature and HeatRisk, by age group.  

################################################################################-
## 0: Upload data ----
################################################################################-

# Generate suffix for file calling and writing, based on data source
data_source_name <- case_when(data_source == 'original' ~ '',
                              data_source == 'synth' ~ '_synth')

# Load estimates for main model 
selected_constraints_ed <- read_csv(here::here('outputs', 'tables', 'selected_constraints_ed.csv') ) %>%
  filter(sensitivity == 'main')

age_grid <- expand_grid(
  subset = c('under5', '5to17', '18to64', '65plus'),
  temp_var = c('tmean', 'heat_risk')) %>%
  mutate(estimate_name = paste0('estimates_', subset, '_', temp_var),
         mod_name = paste0('mod_', subset, '_', temp_var))

load_estimates(selected_constraints_ed, 'ed', 'age', age_grid, 'ind', data_source_name)

################################################################################-
## 1a: Plot relative risk over 6 days lag (under 5) ----
################################################################################-

### Mean temperature ----
# Plot 6 days of lag relative risk of ed for the 95th percentile of temperature compared to the median observed temperature
plot_under5_tmean_per95 <- plot_ind_tmean(lag_num = 6,
                                          mod = mod_under5_tmean, 
                                          outcome = 'ed',
                                          moa = 'rr',
                                          percentile = 'per_95',
                                          temp_var = 'tmean', 
                                          estimates = estimates_under5_tmean, 
                                          subset = 'residents under age 5 '
)

ggsave(filename = here::here('outputs', 'plots', 'ed', 'ind', 'tmean', 'ind_lag6_under5_ed_per95.png'), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of ed for the 95th percentile of temperature compared to the median observed temperature
plot_under5_tmean_per95_pct_chg <- plot_ind_tmean(lag_num = 6,
                                                  mod = mod_under5_tmean, 
                                                  outcome = 'ed',
                                                  moa = 'pct_chg',
                                                  percentile = 'per_95',
                                                  temp_var = 'tmean', 
                                                  estimates = estimates_under5_tmean, 
                                                  subset = 'residents under age 5 '
)

ggsave(filename = here::here('outputs', 'plots', 'ed', 'ind', 'tmean', 'ind_lag6_under5_ed_per95_pct_chg.png'), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of ed for the 5th percentile of temperature compared to the median observed temperature
plot_under5_tmean_per05 <- plot_ind_tmean(lag_num = 6,
                                          mod = mod_under5_tmean, 
                                          outcome = 'ed',
                                          moa = 'rr',
                                          percentile = 'per_05',
                                          temp_var = 'tmean', 
                                          estimates = estimates_under5_tmean, 
                                          subset = 'residents under age 5 '
)

ggsave(filename = here::here('outputs', 'plots', 'ed', 'ind', 'tmean', 'ind_lag6_under5_ed_per05.png'), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of ed for the 5th percentile of temperature compared to the median observed temperature
plot_under5_tmean_per05_pct_chg <- plot_ind_tmean(lag_num = 6,
                                                  mod = mod_under5_tmean, 
                                                  outcome = 'ed',
                                                  moa = 'pct_chg',
                                                  percentile = 'per_05',
                                                  temp_var = 'tmean', 
                                                  estimates = estimates_under5_tmean, 
                                                  subset = 'residents under age 5 '
)

ggsave(filename = here::here('outputs', 'plots', 'ed', 'ind', 'tmean', 'ind_lag6_under5_ed_per05_pct_chg.png'), 
       height = 8, width = 12 )

### HeatRisk ----
# Plot 6 days of lag relative risk of ed for HR3 compared to HR0
plot_under5_hr3 <- plot_ind_hr(lag_num = 6,
                               mod = mod_under5_heat_risk, 
                               outcome = 'ed',
                               moa = 'rr',
                               hr_level = '3',
                               estimates = estimates_under5_heat_risk, 
                               subset = 'residents under age 5 '
)

ggsave(filename = here::here('outputs', 'plots', 'ed', 'ind', 'heat_risk', 'ind_lag6_under5_ed_hr3.png'), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of ed for HR3 compared to HR0
plot_under5_hr3_pct_chg <- plot_ind_hr(lag_num = 6,
                                       mod = mod_under5_heat_risk, 
                                       outcome = 'ed',
                                       moa = 'pct_chg',
                                       hr_level = '3',
                                       estimates = estimates_under5_heat_risk, 
                                       subset = 'residents under age 5 '
)

ggsave(filename = here::here('outputs', 'plots', 'ed', 'ind', 'heat_risk', 'ind_lag6_under5_ed_hr3_pct_chg.png'), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of ed for HR 2 compared to 0
plot_under5_hr2 <- plot_ind_hr(lag_num = 6,
                               mod = mod_under5_heat_risk, 
                               outcome = 'ed',
                               moa = 'rr',
                               hr_level = '2',
                               estimates = estimates_under5_heat_risk, 
                               subset = 'residents under age 5 '
)

ggsave(filename = here::here('outputs', 'plots', 'ed', 'ind', 'heat_risk', 'ind_lag6_under5_ed_hr2.png'), 
       height = 8, width = 12 )


# Plot 6 days of lag relative risk of ed for HR 2 compared to 0
plot_under5_hr2_pct_chg <- plot_ind_hr(lag_num = 6,
                                       mod = mod_under5_heat_risk, 
                                       outcome = 'ed',
                                       moa = 'pct_chg',
                                       hr_level = '2',
                                       estimates = estimates_under5_heat_risk, 
                                       subset = 'residents under age 5 '
)

ggsave(filename = here::here('outputs', 'plots', 'ed', 'ind', 'heat_risk', 'ind_lag6_under5_ed_hr2_pct_chg.png'), 
       height = 8, width = 12 )

################################################################################-
## 1b: Plot relative risk over 6 days lag (5 to 17) ----
################################################################################-

### Mean temperature ----
# Plot 6 days of lag relative risk of ed for the 95th percentile of temperature compared to the median observed temperature
plot_5to17_tmean_per95 <- plot_ind_tmean(lag_num = 6,
                                          mod = mod_5to17_tmean, 
                                          outcome = 'ed',
                                          moa = 'rr',
                                          percentile = 'per_95',
                                          temp_var = 'tmean', 
                                          estimates = estimates_5to17_tmean, 
                                          subset = 'residents under age 5 '
)

ggsave(filename = here::here('outputs', 'plots', 'ed', 'ind', 'tmean', 'ind_lag6_5to17_ed_per95.png'), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of ed for the 95th percentile of temperature compared to the median observed temperature
plot_5to17_tmean_per95_pct_chg <- plot_ind_tmean(lag_num = 6,
                                                  mod = mod_5to17_tmean, 
                                                  outcome = 'ed',
                                                  moa = 'pct_chg',
                                                  percentile = 'per_95',
                                                  temp_var = 'tmean', 
                                                  estimates = estimates_5to17_tmean, 
                                                  subset = 'residents under age 5 '
)

ggsave(filename = here::here('outputs', 'plots', 'ed', 'ind', 'tmean', 'ind_lag6_5to17_ed_per95_pct_chg.png'), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of ed for the 5th percentile of temperature compared to the median observed temperature
plot_5to17_tmean_per05 <- plot_ind_tmean(lag_num = 6,
                                          mod = mod_5to17_tmean, 
                                          outcome = 'ed',
                                          moa = 'rr',
                                          percentile = 'per_05',
                                          temp_var = 'tmean', 
                                          estimates = estimates_5to17_tmean, 
                                          subset = 'residents under age 5 '
)

ggsave(filename = here::here('outputs', 'plots', 'ed', 'ind', 'tmean', 'ind_lag6_5to17_ed_per05.png'), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of ed for the 5th percentile of temperature compared to the median observed temperature
plot_5to17_tmean_per05_pct_chg <- plot_ind_tmean(lag_num = 6,
                                                  mod = mod_5to17_tmean, 
                                                  outcome = 'ed',
                                                  moa = 'pct_chg',
                                                  percentile = 'per_05',
                                                  temp_var = 'tmean', 
                                                  estimates = estimates_5to17_tmean, 
                                                  subset = 'residents under age 5 '
)

ggsave(filename = here::here('outputs', 'plots', 'ed', 'ind', 'tmean', 'ind_lag6_5to17_ed_per05_pct_chg.png'), 
       height = 8, width = 12 )

### HeatRisk ----
# Plot 6 days of lag relative risk of ed for HR3 compared to HR0
plot_5to17_hr3 <- plot_ind_hr(lag_num = 6,
                               mod = mod_5to17_heat_risk, 
                               outcome = 'ed',
                               moa = 'rr',
                               hr_level = '3',
                               estimates = estimates_5to17_heat_risk, 
                               subset = 'residents under age 5 '
)

ggsave(filename = here::here('outputs', 'plots', 'ed', 'ind', 'heat_risk', 'ind_lag6_5to17_ed_hr3.png'), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of ed for HR3 compared to HR0
plot_5to17_hr3_pct_chg <- plot_ind_hr(lag_num = 6,
                                       mod = mod_5to17_heat_risk, 
                                       outcome = 'ed',
                                       moa = 'pct_chg',
                                       hr_level = '3',
                                       estimates = estimates_5to17_heat_risk, 
                                       subset = 'residents under age 5 '
)

ggsave(filename = here::here('outputs', 'plots', 'ed', 'ind', 'heat_risk', 'ind_lag6_5to17_ed_hr3_pct_chg.png'), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of ed for HR 2 compared to 0
plot_5to17_hr2 <- plot_ind_hr(lag_num = 6,
                               mod = mod_5to17_heat_risk, 
                               outcome = 'ed',
                               moa = 'rr',
                               hr_level = '2',
                               estimates = estimates_5to17_heat_risk, 
                               subset = 'residents under age 5 '
)

ggsave(filename = here::here('outputs', 'plots', 'ed', 'ind', 'heat_risk', 'ind_lag6_5to17_ed_hr2.png'), 
       height = 8, width = 12 )


# Plot 6 days of lag relative risk of ed for HR 2 compared to 0
plot_5to17_hr2_pct_chg <- plot_ind_hr(lag_num = 6,
                                       mod = mod_5to17_heat_risk, 
                                       outcome = 'ed',
                                       moa = 'pct_chg',
                                       hr_level = '2',
                                       estimates = estimates_5to17_heat_risk, 
                                       subset = 'residents under age 5 '
)

ggsave(filename = here::here('outputs', 'plots', 'ed', 'ind', 'heat_risk', 'ind_lag6_5to17_ed_hr2_pct_chg.png'), 
       height = 8, width = 12 )

################################################################################-
## 1c: Plot relative risk over 6 days lag (18 to 64) ----
################################################################################-

### Mean temperature ----
# Plot 6 days of lag relative risk of ed for the 95th percentile of temperature compared to the median observed temperature
plot_18to64_tmean_per95 <- plot_ind_tmean(lag_num = 6,
                                          mod = mod_18to64_tmean, 
                                          outcome = 'ed',
                                          moa = 'rr',
                                          percentile = 'per_95',
                                          temp_var = 'tmean', 
                                          estimates = estimates_18to64_tmean, 
                                          subset = 'residents age 18 to 64 '
)

ggsave(filename = here::here('outputs', 'plots', 'ed', 'ind', 'tmean', 'ind_lag6_18to64_ed_per95.png'), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of ed for the 95th percentile of temperature compared to the median observed temperature
plot_18to64_tmean_per95_pct_chg <- plot_ind_tmean(lag_num = 6,
                                                  mod = mod_18to64_tmean, 
                                                  outcome = 'ed',
                                                  moa = 'pct_chg',
                                                  percentile = 'per_95',
                                                  temp_var = 'tmean', 
                                                  estimates = estimates_18to64_tmean, 
                                                  subset = 'residents age 18 to 64 '
)

ggsave(filename = here::here('outputs', 'plots', 'ed', 'ind', 'tmean', 'ind_lag6_18to64_ed_per95_pct_chg.png'), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of ed for the 5th percentile of temperature compared to the median observed temperature
plot_18to64_tmean_per05 <- plot_ind_tmean(lag_num = 6,
                                          mod = mod_18to64_tmean, 
                                          outcome = 'ed',
                                          moa = 'rr',
                                          percentile = 'per_05',
                                          temp_var = 'tmean', 
                                          estimates = estimates_18to64_tmean, 
                                          subset = 'residents age 18 to 64 '
)

ggsave(filename = here::here('outputs', 'plots', 'ed', 'ind', 'tmean', 'ind_lag6_18to64_ed_per05.png'), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of ed for the 5th percentile of temperature compared to the median observed temperature
plot_18to64_tmean_per05_pct_chg <- plot_ind_tmean(lag_num = 6,
                                                  mod = mod_18to64_tmean, 
                                                  outcome = 'ed',
                                                  moa = 'pct_chg',
                                                  percentile = 'per_05',
                                                  temp_var = 'tmean', 
                                                  estimates = estimates_18to64_tmean, 
                                                  subset = 'residents age 18 to 64 '
)

ggsave(filename = here::here('outputs', 'plots', 'ed', 'ind', 'tmean', 'ind_lag6_18to64_ed_per05_pct_chg.png'), 
       height = 8, width = 12 )

### HeatRisk ----
# Plot 6 days of lag relative risk of ed for HR3 compared to HR0
plot_18to64_hr3 <- plot_ind_hr(lag_num = 6,
                               mod = mod_18to64_heat_risk, 
                               outcome = 'ed',
                               moa = 'rr',
                               hr_level = '3',
                               estimates = estimates_18to64_heat_risk, 
                               subset = 'residents age 18 to 64 '
)

ggsave(filename = here::here('outputs', 'plots', 'ed', 'ind', 'heat_risk', 'ind_lag6_18to64_ed_hr3.png'), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of ed for HR3 compared to HR0
plot_18to64_hr3_pct_chg <- plot_ind_hr(lag_num = 6,
                                       mod = mod_18to64_heat_risk, 
                                       outcome = 'ed',
                                       moa = 'pct_chg',
                                       hr_level = '3',
                                       estimates = estimates_18to64_heat_risk, 
                                       subset = 'residents age 18 to 64 '
)

ggsave(filename = here::here('outputs', 'plots', 'ed', 'ind', 'heat_risk', 'ind_lag6_18to64_ed_hr3_pct_chg.png'), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of ed for HR 2 compared to 0
plot_18to64_hr2 <- plot_ind_hr(lag_num = 6,
                               mod = mod_18to64_heat_risk, 
                               outcome = 'ed',
                               moa = 'rr',
                               hr_level = '2',
                               estimates = estimates_18to64_heat_risk, 
                               subset = 'residents age 18 to 64 '
)

ggsave(filename = here::here('outputs', 'plots', 'ed', 'ind', 'heat_risk', 'ind_lag6_18to64_ed_hr2.png'), 
       height = 8, width = 12 )


# Plot 6 days of lag relative risk of ed for HR 2 compared to 0
plot_18to64_hr2_pct_chg <- plot_ind_hr(lag_num = 6,
                                       mod = mod_18to64_heat_risk, 
                                       outcome = 'ed',
                                       moa = 'pct_chg',
                                       hr_level = '2',
                                       estimates = estimates_18to64_heat_risk, 
                                       subset = 'residents age 18 to 64 '
)

ggsave(filename = here::here('outputs', 'plots', 'ed', 'ind', 'heat_risk', 'ind_lag6_18to64_ed_hr2_pct_chg.png'), 
       height = 8, width = 12 )

################################################################################-
## 1d: Plot relative risk over 6 days lag (over 65) ----
################################################################################-

### Mean temperature ----
# Plot 6 days of lag relative risk of ed for the 95th percentile of temperature compared to the median observed temperature
plot_65plus_tmean_per95 <- plot_ind_tmean(lag_num = 6,
                                          mod = mod_65plus_tmean, 
                                          outcome = 'ed',
                                          moa = 'rr',
                                          percentile = 'per_95',
                                          temp_var = 'tmean', 
                                          estimates = estimates_65plus_tmean, 
                                          subset = 'residents over 65 '
)

ggsave(filename = here::here('outputs', 'plots', 'ed', 'ind', 'tmean', 'ind_lag6_65plus_ed_per95.png'), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of ed for the 95th percentile of temperature compared to the median observed temperature
plot_65plus_tmean_per95_pct_chg <- plot_ind_tmean(lag_num = 6,
                                                  mod = mod_65plus_tmean, 
                                                  outcome = 'ed',
                                                  moa = 'pct_chg',
                                                  percentile = 'per_95',
                                                  temp_var = 'tmean', 
                                                  estimates = estimates_65plus_tmean, 
                                                  subset = 'residents over 65 '
)

ggsave(filename = here::here('outputs', 'plots', 'ed', 'ind', 'tmean', 'ind_lag6_65plus_ed_per95_pct_chg.png'), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of ed for the 5th percentile of temperature compared to the median observed temperature
plot_65plus_tmean_per05 <- plot_ind_tmean(lag_num = 6,
                                          mod = mod_65plus_tmean, 
                                          outcome = 'ed',
                                          moa = 'rr',
                                          percentile = 'per_05',
                                          temp_var = 'tmean', 
                                          estimates = estimates_65plus_tmean, 
                                          subset = 'residents over 65 '
)

ggsave(filename = here::here('outputs', 'plots', 'ed', 'ind', 'tmean', 'ind_lag6_65plus_ed_per05.png'), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of ed for the 5th percentile of temperature compared to the median observed temperature
plot_65plus_tmean_per05_pct_chg <- plot_ind_tmean(lag_num = 6,
                                                  mod = mod_65plus_tmean, 
                                                  outcome = 'ed',
                                                  moa = 'pct_chg',
                                                  percentile = 'per_05',
                                                  temp_var = 'tmean', 
                                                  estimates = estimates_65plus_tmean, 
                                                  subset = 'residents over 65 '
)

ggsave(filename = here::here('outputs', 'plots', 'ed', 'ind', 'tmean', 'ind_lag6_65plus_ed_per05_pct_chg.png'), 
       height = 8, width = 12 )

### HeatRisk ----
# Plot 6 days of lag relative risk of ed for HR3 compared to HR0
plot_65plus_hr3 <- plot_ind_hr(lag_num = 6,
                               mod = mod_65plus_heat_risk, 
                               outcome = 'ed',
                               moa = 'rr',
                               hr_level = '3',
                               estimates = estimates_65plus_heat_risk, 
                               subset = 'residents over 65 '
)

ggsave(filename = here::here('outputs', 'plots', 'ed', 'ind', 'heat_risk', 'ind_lag6_65plus_ed_hr3.png'), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of ed for HR3 compared to HR0
plot_65plus_hr3_pct_chg <- plot_ind_hr(lag_num = 6,
                                       mod = mod_65plus_heat_risk, 
                                       outcome = 'ed',
                                       moa = 'pct_chg',
                                       hr_level = '3',
                                       estimates = estimates_65plus_heat_risk, 
                                       subset = 'residents over 65 '
)

ggsave(filename = here::here('outputs', 'plots', 'ed', 'ind', 'heat_risk', 'ind_lag6_65plus_ed_hr3_pct_chg.png'), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of ed for HR 2 compared to 0
plot_65plus_hr2 <- plot_ind_hr(lag_num = 6,
                               mod = mod_65plus_heat_risk, 
                               outcome = 'ed',
                               moa = 'rr',
                               hr_level = '2',
                               estimates = estimates_65plus_heat_risk, 
                               subset = 'residents over 65 '
)

ggsave(filename = here::here('outputs', 'plots', 'ed', 'ind', 'heat_risk', 'ind_lag6_65plus_ed_hr2.png'), 
       height = 8, width = 12 )


# Plot 6 days of lag relative risk of ed for HR 2 compared to 0
plot_65plus_hr2_pct_chg <- plot_ind_hr(lag_num = 6,
                                       mod = mod_65plus_heat_risk, 
                                       outcome = 'ed',
                                       moa = 'pct_chg',
                                       hr_level = '2',
                                       estimates = estimates_65plus_heat_risk, 
                                       subset = 'residents over 65 '
)

ggsave(filename = here::here('outputs', 'plots', 'ed', 'ind', 'heat_risk', 'ind_lag6_65plus_ed_hr2_pct_chg.png'), 
       height = 8, width = 12 )

################################################################################-
## 2: Clean up ----
################################################################################-

rm(data_source_name,
   selected_constraints_ed, age_grid,
   estimates_under5_tmean, estimates_under5_heat_risk,
   estimates_5to17_tmean, estimates_5to17_heat_risk,
   estimates_18to64_tmean, estimates_18to64_heat_risk,
   estimates_65plus_tmean, estimates_65plus_heat_risk,   
   mod_under5_tmean, mod_under5_heat_risk,
   mod_5to17_tmean, mod_5to17_heat_risk,
   mod_18to64_tmean, mod_18to64_heat_risk,
   mod_65plus_tmean, mod_65plus_heat_risk,
   plot_under5_tmean_per95,
   plot_5to17_tmean_per95,
   plot_18to64_tmean_per95, 
   plot_65plus_tmean_per95,
   plot_under5_tmean_per95_pct_chg,
   plot_5to17_tmean_per95_pct_chg,
   plot_18to64_tmean_per95_pct_chg, 
   plot_65plus_tmean_per95_pct_chg,
   plot_under5_tmean_per05,
   plot_5to17_tmean_per05,
   plot_18to64_tmean_per05,
   plot_65plus_tmean_per05,
   plot_under5_tmean_per05_pct_chg,
   plot_5to17_tmean_per05_pct_chg,
   plot_18to64_tmean_per05_pct_chg,
   plot_65plus_tmean_per05_pct_chg,
   plot_under5_hr3,
   plot_5to17_hr3,
   plot_18to64_hr3, 
   plot_65plus_hr3,
   plot_under5_hr3_pct_chg,
   plot_5to17_hr3_pct_chg,
   plot_18to64_hr3_pct_chg, 
   plot_65plus_hr3_pct_chg,
   plot_under5_hr2,
   plot_5to17_hr2,
   plot_18to64_hr2, 
   plot_65plus_hr2,
   plot_under5_hr2_pct_chg,
   plot_5to17_hr2_pct_chg,
   plot_18to64_hr2_pct_chg, 
   plot_65plus_hr2_pct_chg
   )
