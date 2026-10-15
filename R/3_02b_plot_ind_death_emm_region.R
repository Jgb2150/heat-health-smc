################################################################################-
# Plot: Individual lags 0-6 for mean temperature and HeatRisk, deaths, by region ----
# Comparing HeatRisk and temperature for local heat surveillance: A time-stratified 
#  case-crossover study of emergency department visits in San Mateo County, 
#  California, 2017–2025
# Juliana GE Bartels
# Updated: 8/26/2026
################################################################################-

# This file plots the lagged percent change in death rates for 0-6 days following 
# exposure to temperature and HeatRisk, by region. 

################################################################################-
## 0: Upload data ----
################################################################################-

# Generate suffix for file calling and writing, based on data source
data_source_name <- case_when(data_source == 'original' ~ '',
                              data_source == 'synth' ~ '_synth')

# Load estimates amd models for region EMMs
selected_constraints_death <- read_csv(here::here('outputs', 'tables', 'selected_constraints_death.csv') ) %>%
  filter(sensitivity == 'main')

region_grid <- expand_grid(
  subset = c('north', 'mid', 'south', 'coastside'),
  temp_var = c('tmean', 'heat_risk')) %>%
  mutate(estimate_name = paste0('estimates_', subset, '_', temp_var),
         mod_name = paste0('mod_', subset, '_', temp_var))

load_estimates(selected_constraints_death, 'death', 'region', region_grid, 'ind', data_source_name)

################################################################################-
## 1a: Plot relative risk over 6 days lag (North) ----
################################################################################-

### Mean temperature ----
# Plot 6 days of lag relative risk of death for the 95th percentile of temperature compared to the median observed temperature
plot_north_tmean_per95 <- plot_ind_tmean(lag_num = 6,
                                          mod = mod_north_tmean, 
                                          outcome = 'death',
                                          moa = 'rr',
                                          percentile = 'per_95',
                                          temp_var = 'tmean', 
                                          estimates = estimates_north_tmean, 
                                          subset = 'residents in the North county region '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'tmean', 'ind_lag6_north_death_per95.png'), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of death for the 95th percentile of temperature compared to the median observed temperature
plot_north_tmean_per95_pct_chg <- plot_ind_tmean(lag_num = 6,
                                                  mod = mod_north_tmean, 
                                                  outcome = 'death',
                                                  moa = 'pct_chg',
                                                  percentile = 'per_95',
                                                  temp_var = 'tmean', 
                                                  estimates = estimates_north_tmean, 
                                                  subset = 'residents in the North county region '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'tmean', 'ind_lag6_north_death_per95_pct_chg.png'), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of death for the 5th percentile of temperature compared to the median observed temperature
plot_north_tmean_per05 <- plot_ind_tmean(lag_num = 6,
                                          mod = mod_north_tmean, 
                                          outcome = 'death',
                                          moa = 'rr',
                                          percentile = 'per_05',
                                          temp_var = 'tmean', 
                                          estimates = estimates_north_tmean, 
                                          subset = 'residents in the North county region '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'tmean', 'ind_lag6_north_death_per05.png'), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of death for the 5th percentile of temperature compared to the median observed temperature
plot_north_tmean_per05_pct_chg <- plot_ind_tmean(lag_num = 6,
                                                  mod = mod_north_tmean, 
                                                  outcome = 'death',
                                                  moa = 'pct_chg',
                                                  percentile = 'per_05',
                                                  temp_var = 'tmean', 
                                                  estimates = estimates_north_tmean, 
                                                  subset = 'residents in the North county region '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'tmean', 'ind_lag6_north_death_per05_pct_chg.png'), 
       height = 8, width = 12 )

### HeatRisk ----
# Plot 6 days of lag relative risk of death for HR3 compared to HR0
plot_north_hr3 <- plot_ind_hr(lag_num = 6,
                               mod = mod_north_heat_risk, 
                               outcome = 'death',
                               moa = 'rr',
                               hr_level = '3',
                               estimates = estimates_north_heat_risk, 
                               subset = 'residents in the North county region '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'heat_risk', 'ind_lag6_north_death_hr3.png'), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of death for HR3 compared to HR0
plot_north_hr3_pct_chg <- plot_ind_hr(lag_num = 6,
                                       mod = mod_north_heat_risk, 
                                       outcome = 'death',
                                       moa = 'pct_chg',
                                       hr_level = '3',
                                       estimates = estimates_north_heat_risk, 
                                       subset = 'residents in the North county region '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'heat_risk', 'ind_lag6_north_death_hr3_pct_chg.png'), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of death for HR 2 compared to 0
plot_north_hr2 <- plot_ind_hr(lag_num = 6,
                               mod = mod_north_heat_risk, 
                               outcome = 'death',
                               moa = 'rr',
                               hr_level = '2',
                               estimates = estimates_north_heat_risk, 
                               subset = 'residents in the North county region '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'heat_risk', 'ind_lag6_north_death_hr2.png'), 
       height = 8, width = 12 )


# Plot 6 days of lag relative risk of death for HR 2 compared to 0
plot_north_hr2_pct_chg <- plot_ind_hr(lag_num = 6,
                                       mod = mod_north_heat_risk, 
                                       outcome = 'death',
                                       moa = 'pct_chg',
                                       hr_level = '2',
                                       estimates = estimates_north_heat_risk, 
                                       subset = 'residents in the North county region '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'heat_risk', 'ind_lag6_north_death_hr2_pct_chg.png'), 
       height = 8, width = 12 )

################################################################################-
## 1b: Plot relative risk over 6 days lag (in the Mid-county region) ----
################################################################################-

### Mean temperature ----
# Plot 6 days of lag relative risk of death for the 95th percentile of temperature compared to the median observed temperature
plot_mid_tmean_per95 <- plot_ind_tmean(lag_num = 6,
                                          mod = mod_mid_tmean, 
                                          outcome = 'death',
                                          moa = 'rr',
                                          percentile = 'per_95',
                                          temp_var = 'tmean', 
                                          estimates = estimates_mid_tmean, 
                                          subset = 'residents in the Mid-county region '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'tmean', 'ind_lag6_mid_death_per95.png'), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of death for the 95th percentile of temperature compared to the median observed temperature
plot_mid_tmean_per95_pct_chg <- plot_ind_tmean(lag_num = 6,
                                                  mod = mod_mid_tmean, 
                                                  outcome = 'death',
                                                  moa = 'pct_chg',
                                                  percentile = 'per_95',
                                                  temp_var = 'tmean', 
                                                  estimates = estimates_mid_tmean, 
                                                  subset = 'residents in the Mid-county region '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'tmean', 'ind_lag6_mid_death_per95_pct_chg.png'), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of death for the 5th percentile of temperature compared to the median observed temperature
plot_mid_tmean_per05 <- plot_ind_tmean(lag_num = 6,
                                          mod = mod_mid_tmean, 
                                          outcome = 'death',
                                          moa = 'rr',
                                          percentile = 'per_05',
                                          temp_var = 'tmean', 
                                          estimates = estimates_mid_tmean, 
                                          subset = 'residents in the Mid-county region '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'tmean', 'ind_lag6_mid_death_per05.png'), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of death for the 5th percentile of temperature compared to the median observed temperature
plot_mid_tmean_per05_pct_chg <- plot_ind_tmean(lag_num = 6,
                                                  mod = mod_mid_tmean, 
                                                  outcome = 'death',
                                                  moa = 'pct_chg',
                                                  percentile = 'per_05',
                                                  temp_var = 'tmean', 
                                                  estimates = estimates_mid_tmean, 
                                                  subset = 'residents age in the Mid-county region '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'tmean', 'ind_lag6_mid_death_per05_pct_chg.png'), 
       height = 8, width = 12 )

### HeatRisk ----
# Plot 6 days of lag relative risk of death for HR3 compared to HR0
plot_mid_hr3 <- plot_ind_hr(lag_num = 6,
                               mod = mod_mid_heat_risk, 
                               outcome = 'death',
                               moa = 'rr',
                               hr_level = '3',
                               estimates = estimates_mid_heat_risk, 
                               subset = 'residents in the Mid-county region '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'heat_risk', 'ind_lag6_mid_death_hr3.png'), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of death for HR3 compared to HR0
plot_mid_hr3_pct_chg <- plot_ind_hr(lag_num = 6,
                                       mod = mod_mid_heat_risk, 
                                       outcome = 'death',
                                       moa = 'pct_chg',
                                       hr_level = '3',
                                       estimates = estimates_mid_heat_risk, 
                                       subset = 'residents in the Mid-county region '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'heat_risk', 'ind_lag6_mid_death_hr3_pct_chg.png'), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of death for HR 2 compared to 0
plot_mid_hr2 <- plot_ind_hr(lag_num = 6,
                               mod = mod_mid_heat_risk, 
                               outcome = 'death',
                               moa = 'rr',
                               hr_level = '2',
                               estimates = estimates_mid_heat_risk, 
                               subset = 'residents in the Mid-county region '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'heat_risk', 'ind_lag6_mid_death_hr2.png'), 
       height = 8, width = 12 )


# Plot 6 days of lag relative risk of death for HR 2 compared to 0
plot_mid_hr2_pct_chg <- plot_ind_hr(lag_num = 6,
                                       mod = mod_mid_heat_risk, 
                                       outcome = 'death',
                                       moa = 'pct_chg',
                                       hr_level = '2',
                                       estimates = estimates_mid_heat_risk, 
                                       subset = 'residents in the Mid-county region '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'heat_risk', 'ind_lag6_mid_death_hr2_pct_chg.png'), 
       height = 8, width = 12 )

################################################################################-
## 1c: Plot relative risk over 6 days lag (in the South county region) ----
################################################################################-

### Mean temperature ----
# Plot 6 days of lag relative risk of death for the 95th percentile of temperature compared to the median observed temperature
plot_south_tmean_per95 <- plot_ind_tmean(lag_num = 6,
                                          mod = mod_south_tmean, 
                                          outcome = 'death',
                                          moa = 'rr',
                                          percentile = 'per_95',
                                          temp_var = 'tmean', 
                                          estimates = estimates_south_tmean, 
                                          subset = 'residents in the South county region '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'tmean', 'ind_lag6_south_death_per95.png'), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of death for the 95th percentile of temperature compared to the median observed temperature
plot_south_tmean_per95_pct_chg <- plot_ind_tmean(lag_num = 6,
                                                  mod = mod_south_tmean, 
                                                  outcome = 'death',
                                                  moa = 'pct_chg',
                                                  percentile = 'per_95',
                                                  temp_var = 'tmean', 
                                                  estimates = estimates_south_tmean, 
                                                  subset = 'residents in the South county region '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'tmean', 'ind_lag6_south_death_per95_pct_chg.png'), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of death for the 5th percentile of temperature compared to the median observed temperature
plot_south_tmean_per05 <- plot_ind_tmean(lag_num = 6,
                                          mod = mod_south_tmean, 
                                          outcome = 'death',
                                          moa = 'rr',
                                          percentile = 'per_05',
                                          temp_var = 'tmean', 
                                          estimates = estimates_south_tmean, 
                                          subset = 'residents in the South county region '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'tmean', 'ind_lag6_south_death_per05.png'), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of death for the 5th percentile of temperature compared to the median observed temperature
plot_south_tmean_per05_pct_chg <- plot_ind_tmean(lag_num = 6,
                                                  mod = mod_south_tmean, 
                                                  outcome = 'death',
                                                  moa = 'pct_chg',
                                                  percentile = 'per_05',
                                                  temp_var = 'tmean', 
                                                  estimates = estimates_south_tmean, 
                                                  subset = 'residents in the South county region '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'tmean', 'ind_lag6_south_death_per05_pct_chg.png'), 
       height = 8, width = 12 )

### HeatRisk ----
# Plot 6 days of lag relative risk of death for HR3 compared to HR0
plot_south_hr3 <- plot_ind_hr(lag_num = 6,
                               mod = mod_south_heat_risk, 
                               outcome = 'death',
                               moa = 'rr',
                               hr_level = '3',
                               estimates = estimates_south_heat_risk, 
                               subset = 'residents in the South county region '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'heat_risk', 'ind_lag6_south_death_hr3.png'), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of death for HR3 compared to HR0
plot_south_hr3_pct_chg <- plot_ind_hr(lag_num = 6,
                                       mod = mod_south_heat_risk, 
                                       outcome = 'death',
                                       moa = 'pct_chg',
                                       hr_level = '3',
                                       estimates = estimates_south_heat_risk, 
                                       subset = 'residents in the South county region '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'heat_risk', 'ind_lag6_south_death_hr3_pct_chg.png'), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of death for HR 2 compared to 0
plot_south_hr2 <- plot_ind_hr(lag_num = 6,
                               mod = mod_south_heat_risk, 
                               outcome = 'death',
                               moa = 'rr',
                               hr_level = '2',
                               estimates = estimates_south_heat_risk, 
                               subset = 'residents in the South county region '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'heat_risk', 'ind_lag6_south_death_hr2.png'), 
       height = 8, width = 12 )


# Plot 6 days of lag relative risk of death for HR 2 compared to 0
plot_south_hr2_pct_chg <- plot_ind_hr(lag_num = 6,
                                       mod = mod_south_heat_risk, 
                                       outcome = 'death',
                                       moa = 'pct_chg',
                                       hr_level = '2',
                                       estimates = estimates_south_heat_risk, 
                                       subset = 'residents in the South county region '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'heat_risk', 'ind_lag6_south_death_hr2_pct_chg.png'), 
       height = 8, width = 12 )

################################################################################-
## 1d: Plot relative risk over 6 days lag (in the Coastside region) ----
################################################################################-

### Mean temperature ----
# Plot 6 days of lag relative risk of death for the 95th percentile of temperature compared to the median observed temperature
plot_coastside_tmean_per95 <- plot_ind_tmean(lag_num = 6,
                                          mod = mod_coastside_tmean, 
                                          outcome = 'death',
                                          moa = 'rr',
                                          percentile = 'per_95',
                                          temp_var = 'tmean', 
                                          estimates = estimates_coastside_tmean, 
                                          subset = 'residents in the Coastside region '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'tmean', 'ind_lag6_coastside_death_per95.png'), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of death for the 95th percentile of temperature compared to the median observed temperature
plot_coastside_tmean_per95_pct_chg <- plot_ind_tmean(lag_num = 6,
                                                  mod = mod_coastside_tmean, 
                                                  outcome = 'death',
                                                  moa = 'pct_chg',
                                                  percentile = 'per_95',
                                                  temp_var = 'tmean', 
                                                  estimates = estimates_coastside_tmean, 
                                                  subset = 'residents in the Coastside region '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'tmean', 'ind_lag6_coastside_death_per95_pct_chg.png'), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of death for the 5th percentile of temperature compared to the median observed temperature
plot_coastside_tmean_per05 <- plot_ind_tmean(lag_num = 6,
                                          mod = mod_coastside_tmean, 
                                          outcome = 'death',
                                          moa = 'rr',
                                          percentile = 'per_05',
                                          temp_var = 'tmean', 
                                          estimates = estimates_coastside_tmean, 
                                          subset = 'residents in the Coastside region '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'tmean', 'ind_lag6_coastside_death_per05.png'), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of death for the 5th percentile of temperature compared to the median observed temperature
plot_coastside_tmean_per05_pct_chg <- plot_ind_tmean(lag_num = 6,
                                                  mod = mod_coastside_tmean, 
                                                  outcome = 'death',
                                                  moa = 'pct_chg',
                                                  percentile = 'per_05',
                                                  temp_var = 'tmean', 
                                                  estimates = estimates_coastside_tmean, 
                                                  subset = 'residents in the Coastside region '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'tmean', 'ind_lag6_coastside_death_per05_pct_chg.png'), 
       height = 8, width = 12 )

### HeatRisk ----
# Plot 6 days of lag relative risk of death for HR3 compared to HR0
plot_coastside_hr3 <- plot_ind_hr(lag_num = 6,
                               mod = mod_coastside_heat_risk, 
                               outcome = 'death',
                               moa = 'rr',
                               hr_level = '3',
                               estimates = estimates_coastside_heat_risk, 
                               subset = 'residents in the Coastside region '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'heat_risk', 'ind_lag6_coastside_death_hr3.png'), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of death for HR3 compared to HR0
plot_coastside_hr3_pct_chg <- plot_ind_hr(lag_num = 6,
                                       mod = mod_coastside_heat_risk, 
                                       outcome = 'death',
                                       moa = 'pct_chg',
                                       hr_level = '3',
                                       estimates = estimates_coastside_heat_risk, 
                                       subset = 'residents in the Coastside region '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'heat_risk', 'ind_lag6_coastside_death_hr3_pct_chg.png'), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of death for HR 2 compared to 0
plot_coastside_hr2 <- plot_ind_hr(lag_num = 6,
                               mod = mod_coastside_heat_risk, 
                               outcome = 'death',
                               moa = 'rr',
                               hr_level = '2',
                               estimates = estimates_coastside_heat_risk, 
                               subset = 'residents in the Coastside region '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'heat_risk', 'ind_lag6_coastside_death_hr2.png'), 
       height = 8, width = 12 )


# Plot 6 days of lag relative risk of death for HR 2 compared to 0
plot_coastside_hr2_pct_chg <- plot_ind_hr(lag_num = 6,
                                       mod = mod_coastside_heat_risk, 
                                       outcome = 'death',
                                       moa = 'pct_chg',
                                       hr_level = '2',
                                       estimates = estimates_coastside_heat_risk, 
                                       subset = 'residents in the Coastside region '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'heat_risk', 'ind_lag6_coastside_death_hr2_pct_chg.png'), 
       height = 8, width = 12 )

################################################################################-
## 2: Clean up ----
################################################################################-

rm(data_source_name,
   selected_constraints_death, region_grid,
   estimates_north_tmean, estimates_north_heat_risk,
   estimates_mid_tmean, estimates_mid_heat_risk,
   estimates_south_tmean, estimates_south_heat_risk,
   estimates_coastside_tmean, estimates_coastside_heat_risk,   
   mod_north_tmean, mod_north_heat_risk,
   mod_mid_tmean, mod_mid_heat_risk,
   mod_south_tmean, mod_south_heat_risk,
   mod_coastside_tmean, mod_coastside_heat_risk,
   plot_north_tmean_per95,
   plot_mid_tmean_per95,
   plot_south_tmean_per95, 
   plot_coastside_tmean_per95,
   plot_north_tmean_per95_pct_chg,
   plot_mid_tmean_per95_pct_chg,
   plot_south_tmean_per95_pct_chg, 
   plot_coastside_tmean_per95_pct_chg,
   plot_north_tmean_per05,
   plot_mid_tmean_per05,
   plot_south_tmean_per05,
   plot_coastside_tmean_per05,
   plot_north_tmean_per05_pct_chg,
   plot_mid_tmean_per05_pct_chg,
   plot_south_tmean_per05_pct_chg,
   plot_coastside_tmean_per05_pct_chg,
   plot_north_hr3,
   plot_mid_hr3,
   plot_south_hr3, 
   plot_coastside_hr3,
   plot_north_hr3_pct_chg,
   plot_mid_hr3_pct_chg,
   plot_south_hr3_pct_chg, 
   plot_coastside_hr3_pct_chg,
   plot_north_hr2,
   plot_mid_hr2,
   plot_south_hr2, 
   plot_coastside_hr2,
   plot_north_hr2_pct_chg,
   plot_mid_hr2_pct_chg,
   plot_south_hr2_pct_chg, 
   plot_coastside_hr2_pct_chg
)

