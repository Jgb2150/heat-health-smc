################################################################################-
# Plot: Individual lags 0-6 for mean temperature and HeatRisk, deaths, by age group ----
# Comparing HeatRisk and temperature for local heat surveillance: A time-stratified 
#  case-crossover study of emergency department visits in San Mateo County, 
#  California, 2017–2025 
# Juliana GE Bartels
# Updated: 8/26/2026
################################################################################-

# This file plots the lagged percent change in death rates for 0-20 days following 
# exposure to temperature and HeatRisk, by age group.  

################################################################################-
## 0: Upload data ----
################################################################################-

# Generate suffix for file calling and writing, based on data source
data_source_name <- case_when(data_source == 'original' ~ '',
                              data_source == 'synth' ~ '_synth')

# Load estimates amd models for age EMMs
selected_constraints_death <- read_csv(here::here('outputs', 'tables', 'selected_constraints_death.csv') ) %>%
  filter(sensitivity == 'main')

age_grid <- expand_grid(
  subset = c('under5', '18to64', '65plus'),
  temp_var = c('tmean', 'heat_risk')) %>%
  mutate(estimate_name = paste0('estimates_', subset, '_', temp_var),
         mod_name = paste0('mod_', subset, '_', temp_var))

load_estimates(selected_constraints_death, 'death', 'age', age_grid, 'ind', data_source_name)

################################################################################-
## 1a: Plot relative risk over 6 days lag (under 5) ----
################################################################################-

### Mean temperature ----
# Plot 6 days of lag relative risk of death for the 95th percentile of temperature compared to the median observed temperature
plot_under5_tmean_per95 <- plot_ind_tmean(lag_num = 6,
                                    mod = mod_under5_tmean, 
                                    outcome = 'death',
                                    moa = 'rr',
                                    percentile = 'per_95',
                                    temp_var = 'tmean', 
                                    estimates = estimates_under5_tmean, 
                                    subset = 'residents under age 5 '
                                    )

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'tmean', 'ind_lag6_under5_death_per95.png'), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of death for the 95th percentile of temperature compared to the median observed temperature
plot_under5_tmean_per95_pct_chg <- plot_ind_tmean(lag_num = 6,
                                          mod = mod_under5_tmean, 
                                          outcome = 'death',
                                          moa = 'pct_chg',
                                          percentile = 'per_95',
                                          temp_var = 'tmean', 
                                          estimates = estimates_under5_tmean, 
                                          subset = 'residents under age 5 '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'tmean', 'ind_lag6_under5_death_per95_pct_chg.png'), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of death for the 5th percentile of temperature compared to the median observed temperature
plot_under5_tmean_per05 <- plot_ind_tmean(lag_num = 6,
                                          mod = mod_under5_tmean, 
                                          outcome = 'death',
                                          moa = 'rr',
                                          percentile = 'per_05',
                                          temp_var = 'tmean', 
                                          estimates = estimates_under5_tmean, 
                                          subset = 'residents under age 5 '
                                          )

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'tmean', 'ind_lag6_under5_death_per05.png'), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of death for the 5th percentile of temperature compared to the median observed temperature
plot_under5_tmean_per05_pct_chg <- plot_ind_tmean(lag_num = 6,
                                          mod = mod_under5_tmean, 
                                          outcome = 'death',
                                          moa = 'pct_chg',
                                          percentile = 'per_05',
                                          temp_var = 'tmean', 
                                          estimates = estimates_under5_tmean, 
                                          subset = 'residents under age 5 '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'tmean', 'ind_lag6_under5_death_per05_pct_chg.png'), 
       height = 8, width = 12 )

### HeatRisk ----
# Plot 6 days of lag relative risk of death for HR3 compared to HR0
plot_under5_hr3 <- plot_ind_hr(lag_num = 6,
                               mod = mod_under5_heat_risk, 
                               outcome = 'death',
                               moa = 'rr',
                               hr_level = '3',
                               estimates = estimates_under5_heat_risk, 
                               subset = 'residents under age 5 '
                              )

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'heat_risk', 'ind_lag6_under5_death_hr3.png'), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of death for HR3 compared to HR0
plot_under5_hr3_pct_chg <- plot_ind_hr(lag_num = 6,
                               mod = mod_under5_heat_risk, 
                               outcome = 'death',
                               moa = 'pct_chg',
                               hr_level = '3',
                               estimates = estimates_under5_heat_risk, 
                               subset = 'residents under age 5 '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'heat_risk', 'ind_lag6_under5_death_hr3_pct_chg.png'), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of death for HR 2 compared to 0
plot_under5_hr2 <- plot_ind_hr(lag_num = 6,
                               mod = mod_under5_heat_risk, 
                               outcome = 'death',
                               moa = 'rr',
                               hr_level = '2',
                               estimates = estimates_under5_heat_risk, 
                               subset = 'residents under age 5 '
                              )

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'heat_risk', 'ind_lag6_under5_death_hr2.png'), 
       height = 8, width = 12 )


# Plot 6 days of lag relative risk of death for HR 2 compared to 0
plot_under5_hr2_pct_chg <- plot_ind_hr(lag_num = 6,
                               mod = mod_under5_heat_risk, 
                               outcome = 'death',
                               moa = 'pct_chg',
                               hr_level = '2',
                               estimates = estimates_under5_heat_risk, 
                               subset = 'residents under age 5 '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'heat_risk', 'ind_lag6_under5_death_hr2_pct_chg.png'), 
       height = 8, width = 12 )

################################################################################-
## 1b: Plot relative risk over 6 days lag (5 to 17) ----
################################################################################-

# Model did not converge 

################################################################################-
## 1c: Plot relative risk over 6 days lag (18 to 64) ----
################################################################################-

### Mean temperature ----
# Plot 6 days of lag relative risk of death for the 95th percentile of temperature compared to the median observed temperature
plot_18to64_tmean_per95 <- plot_ind_tmean(lag_num = 6,
                                          mod = mod_18to64_tmean, 
                                          outcome = 'death',
                                          moa = 'rr',
                                          percentile = 'per_95',
                                          temp_var = 'tmean', 
                                          estimates = estimates_18to64_tmean, 
                                          subset = 'residents age 18 to 64 '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'tmean', 'ind_lag6_18to64_death_per95.png'), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of death for the 95th percentile of temperature compared to the median observed temperature
plot_18to64_tmean_per95_pct_chg <- plot_ind_tmean(lag_num = 6,
                                                  mod = mod_18to64_tmean, 
                                                  outcome = 'death',
                                                  moa = 'pct_chg',
                                                  percentile = 'per_95',
                                                  temp_var = 'tmean', 
                                                  estimates = estimates_18to64_tmean, 
                                                  subset = 'residents age 18 to 64 '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'tmean', 'ind_lag6_18to64_death_per95_pct_chg.png'), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of death for the 5th percentile of temperature compared to the median observed temperature
plot_18to64_tmean_per05 <- plot_ind_tmean(lag_num = 6,
                                          mod = mod_18to64_tmean, 
                                          outcome = 'death',
                                          moa = 'rr',
                                          percentile = 'per_05',
                                          temp_var = 'tmean', 
                                          estimates = estimates_18to64_tmean, 
                                          subset = 'residents age 18 to 64 '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'tmean', 'ind_lag6_18to64_death_per05.png'), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of death for the 5th percentile of temperature compared to the median observed temperature
plot_18to64_tmean_per05_pct_chg <- plot_ind_tmean(lag_num = 6,
                                                  mod = mod_18to64_tmean, 
                                                  outcome = 'death',
                                                  moa = 'pct_chg',
                                                  percentile = 'per_05',
                                                  temp_var = 'tmean', 
                                                  estimates = estimates_18to64_tmean, 
                                                  subset = 'residents age 18 to 64 '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'tmean', 'ind_lag6_18to64_death_per05_pct_chg.png'), 
       height = 8, width = 12 )

### HeatRisk ----
# Plot 6 days of lag relative risk of death for HR3 compared to HR0
plot_18to64_hr3 <- plot_ind_hr(lag_num = 6,
                               mod = mod_18to64_heat_risk, 
                               outcome = 'death',
                               moa = 'rr',
                               hr_level = '3',
                               estimates = estimates_18to64_heat_risk, 
                               subset = 'residents age 18 to 64 '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'heat_risk', 'ind_lag6_18to64_death_hr3.png'), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of death for HR3 compared to HR0
plot_18to64_hr3_pct_chg <- plot_ind_hr(lag_num = 6,
                                       mod = mod_18to64_heat_risk, 
                                       outcome = 'death',
                                       moa = 'pct_chg',
                                       hr_level = '3',
                                       estimates = estimates_18to64_heat_risk, 
                                       subset = 'residents age 18 to 64 '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'heat_risk', 'ind_lag6_18to64_death_hr3_pct_chg.png'), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of death for HR 2 compared to 0
plot_18to64_hr2 <- plot_ind_hr(lag_num = 6,
                               mod = mod_18to64_heat_risk, 
                               outcome = 'death',
                               moa = 'rr',
                               hr_level = '2',
                               estimates = estimates_18to64_heat_risk, 
                               subset = 'residents age 18 to 64 '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'heat_risk', 'ind_lag6_18to64_death_hr2.png'), 
       height = 8, width = 12 )


# Plot 6 days of lag relative risk of death for HR 2 compared to 0
plot_18to64_hr2_pct_chg <- plot_ind_hr(lag_num = 6,
                                       mod = mod_18to64_heat_risk, 
                                       outcome = 'death',
                                       moa = 'pct_chg',
                                       hr_level = '2',
                                       estimates = estimates_18to64_heat_risk, 
                                       subset = 'residents age 18 to 64 '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'heat_risk', 'ind_lag6_18to64_death_hr2_pct_chg.png'), 
       height = 8, width = 12 )

################################################################################-
## 1d: Plot relative risk over 6 days lag (over 65) ----
################################################################################-

### Mean temperature ----
# Plot 6 days of lag relative risk of death for the 95th percentile of temperature compared to the median observed temperature
plot_65plus_tmean_per95 <- plot_ind_tmean(lag_num = 6,
                                          mod = mod_65plus_tmean, 
                                          outcome = 'death',
                                          moa = 'rr',
                                          percentile = 'per_95',
                                          temp_var = 'tmean', 
                                          estimates = estimates_65plus_tmean, 
                                          subset = 'residents over 65 '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'tmean', 'ind_lag6_65plus_death_per95.png'), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of death for the 95th percentile of temperature compared to the median observed temperature
plot_65plus_tmean_per95_pct_chg <- plot_ind_tmean(lag_num = 6,
                                                  mod = mod_65plus_tmean, 
                                                  outcome = 'death',
                                                  moa = 'pct_chg',
                                                  percentile = 'per_95',
                                                  temp_var = 'tmean', 
                                                  estimates = estimates_65plus_tmean, 
                                                  subset = 'residents over 65 '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'tmean', 'ind_lag6_65plus_death_per95_pct_chg.png'), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of death for the 5th percentile of temperature compared to the median observed temperature
plot_65plus_tmean_per05 <- plot_ind_tmean(lag_num = 6,
                                          mod = mod_65plus_tmean, 
                                          outcome = 'death',
                                          moa = 'rr',
                                          percentile = 'per_05',
                                          temp_var = 'tmean', 
                                          estimates = estimates_65plus_tmean, 
                                          subset = 'residents over 65 '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'tmean', 'ind_lag6_65plus_death_per05.png'), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of death for the 5th percentile of temperature compared to the median observed temperature
plot_65plus_tmean_per05_pct_chg <- plot_ind_tmean(lag_num = 6,
                                                  mod = mod_65plus_tmean, 
                                                  outcome = 'death',
                                                  moa = 'pct_chg',
                                                  percentile = 'per_05',
                                                  temp_var = 'tmean', 
                                                  estimates = estimates_65plus_tmean, 
                                                  subset = 'residents over 65 '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'tmean', 'ind_lag6_65plus_death_per05_pct_chg.png'), 
       height = 8, width = 12 )

### HeatRisk ----
# Plot 6 days of lag relative risk of death for HR3 compared to HR0
plot_65plus_hr3 <- plot_ind_hr(lag_num = 6,
                               mod = mod_65plus_heat_risk, 
                               outcome = 'death',
                               moa = 'rr',
                               hr_level = '3',
                               estimates = estimates_65plus_heat_risk, 
                               subset = 'residents over 65 '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'heat_risk', 'ind_lag6_65plus_death_hr3.png'), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of death for HR3 compared to HR0
plot_65plus_hr3_pct_chg <- plot_ind_hr(lag_num = 6,
                                       mod = mod_65plus_heat_risk, 
                                       outcome = 'death',
                                       moa = 'pct_chg',
                                       hr_level = '3',
                                       estimates = estimates_65plus_heat_risk, 
                                       subset = 'residents over 65 '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'heat_risk', 'ind_lag6_65plus_death_hr3_pct_chg.png'), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of death for HR 2 compared to 0
plot_65plus_hr2 <- plot_ind_hr(lag_num = 6,
                               mod = mod_65plus_heat_risk, 
                               outcome = 'death',
                               moa = 'rr',
                               hr_level = '2',
                               estimates = estimates_65plus_heat_risk, 
                               subset = 'residents over 65 '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'heat_risk', 'ind_lag6_65plus_death_hr2.png'), 
       height = 8, width = 12 )


# Plot 6 days of lag relative risk of death for HR 2 compared to 0
plot_65plus_hr2_pct_chg <- plot_ind_hr(lag_num = 6,
                                       mod = mod_65plus_heat_risk, 
                                       outcome = 'death',
                                       moa = 'pct_chg',
                                       hr_level = '2',
                                       estimates = estimates_65plus_heat_risk, 
                                       subset = 'residents over 65 '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'heat_risk', 'ind_lag6_65plus_death_hr2_pct_chg.png'), 
       height = 8, width = 12 )

################################################################################-
## 2: Clean up ----
################################################################################-

rm(data_source_name,
   selected_constraints_death, age_grid,
   estimates_under5_tmean, estimates_under5_heat_risk,
   estimates_18to64_tmean, estimates_18to64_heat_risk,
   estimates_65plus_tmean, estimates_65plus_heat_risk,   
   mod_under5_tmean, mod_under5_heat_risk,
   mod_18to64_tmean, mod_18to64_heat_risk,
   mod_65plus_tmean, mod_65plus_heat_risk,
   plot_under5_tmean_per95,
   plot_18to64_tmean_per95, plot_65plus_tmean_per95,
   plot_under5_tmean_per05,
   plot_18to64_tmean_per05, plot_65plus_tmean_per05,
   plot_under5_hr3,
   plot_18to64_hr3, plot_65plus_hr3,
   plot_under5_hr2,
   plot_18to64_hr2, plot_65plus_hr2
   )
