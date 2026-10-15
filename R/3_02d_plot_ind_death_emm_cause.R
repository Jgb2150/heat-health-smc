################################################################################-
# Plot: Individual lags 0-6 for mean temperature and HeatRisk, deaths, by cause ----
# Comparing HeatRisk and temperature for local heat surveillance: A time-stratified 
#  case-crossover study of emergency department visits in San Mateo County, 
#  California, 2017–2025
# Juliana GE Bartels
# Updated: 8/26/26
################################################################################-

# This file plots the lagged percent change in death rates for 0-6 days following 
# exposure to temperature and HeatRisk, by cause.  

################################################################################-
## 0: Upload data ----
################################################################################-

# Generate suffix for file calling and writing, based on data source
data_source_name <- case_when(data_source == 'original' ~ '',
                              data_source == 'synth' ~ '_synth')

# Load estimates amd models for age EMMs
selected_constraints_death <- read_csv(here::here('outputs', 'tables', 'selected_constraints_death.csv') ) %>%
  filter(sensitivity == 'main')

cause_grid <- expand_grid(
  subset = c('cardiovascular', 
             'respiratory', 
             'stroke', 
             'intentinjury',
             'unintentinjury', 
             #'heat',
             'substance',
             #'mental', 
             'renal', 
             'genitourinary'),
  temp_var = c('tmean', 'heat_risk')) %>%
  mutate(estimate_name = paste0('estimates_', subset, '_', temp_var),
         mod_name = paste0('mod_', subset, '_', temp_var))

load_estimates(selected_constraints_death, 'death', 'cause', cause_grid, 'ind', data_source_name)

################################################################################-
## 1a: Plot relative risk over 6 days lag (cardiovascular) ----
################################################################################-

### Mean temperature ----

# Plot 6 days of lag relative risk of death for the 95th percentile of temperature compared to the median observed temperature
plot_cardiovascular_tmean_per95 <- plot_ind_tmean(lag_num = 6,
                                          mod = mod_cardiovascular_tmean, 
                                          outcome = 'death',
                                          moa = 'pct_chg',
                                          percentile = 'per_95',
                                          temp_var = 'tmean', 
                                          estimates = estimates_cardiovascular_tmean, 
                                          subset = 'patients with cardiovascular cause of death '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'tmean', 'ind_lag6_cardiovascular_death_per95_pctchg.png'), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of death for the 5th percentile of temperature compared to the median observed temperature
plot_cardiovascular_tmean_per05 <- plot_ind_tmean(lag_num = 6,
                                          mod = mod_cardiovascular_tmean, 
                                          outcome = 'death',
                                          moa = 'pct_chg',
                                          percentile = 'per_05',
                                          temp_var = 'tmean', 
                                          estimates = estimates_cardiovascular_tmean, 
                                          subset = 'patients with cardiovascular cause of death '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'tmean', 'ind_lag6_cardiovascular_death_per05_pctchg.png'), 
       height = 8, width = 12 )

### HeatRisk ----

# Plot 6 days of lag relative risk of death for HR 3 compared to 0
plot_cardiovascular_hr3 <- plot_ind_hr(lag_num = 6,
                                  mod = mod_cardiovascular_heat_risk, 
                                  outcome = 'death',
                                  moa = 'pct_chg',
                                  hr_level = '3',
                                  estimates = estimates_cardiovascular_heat_risk, 
                                  subset = 'patients with cardiovascular cause of death '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'heat_risk', 'ind_lag6_cardiovascular_death_hr3_pctchg.png'), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of death for HR 2 compared to 0
plot_cardiovascular_hr2 <- plot_ind_hr(lag_num = 6,
                                      mod = mod_cardiovascular_heat_risk, 
                                      outcome = 'death',
                                      moa = 'pct_chg',
                                      hr_level = '2',
                                      estimates = estimates_cardiovascular_heat_risk, 
                                      subset = 'patients with cardiovascular cause of death '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'heat_risk', 'ind_lag6_cardiovascular_death_hr2_pctchg.png'), 
       height = 8, width = 12 )
################################################################################-
## 1b: Plot relative risk over 6 days lag (respiratory) ----
################################################################################-

### Mean temperature ----

# Plot 6 days of lag relative risk of death for the 95th percentile of temperature compared to the median observed temperature
plot_respiratory_tmean_per95 <- plot_ind_tmean(lag_num = 6,
                                                  mod = mod_respiratory_tmean, 
                                                  outcome = 'death',
                                                  moa = 'pct_chg',
                                                  percentile = 'per_95',
                                                  temp_var = 'tmean', 
                                                  estimates = estimates_respiratory_tmean, 
                                                  subset = 'patients with respiratory cause of death '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'tmean', 'ind_lag6_respiratory_death_per95_pctchg.png'), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of death for the 5th percentile of temperature compared to the median observed temperature
plot_respiratory_tmean_per05 <- plot_ind_tmean(lag_num = 6,
                                                  mod = mod_respiratory_tmean, 
                                                  outcome = 'death',
                                                  moa = 'pct_chg',
                                                  percentile = 'per_05',
                                                  temp_var = 'tmean', 
                                                  estimates = estimates_respiratory_tmean, 
                                                  subset = 'patients with respiratory cause of death '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'tmean', 'ind_lag6_respiratory_death_per05_pctchg.png'), 
       height = 8, width = 12 )

### HeatRisk ----

# Plot 6 days of lag relative risk of death for HR 3 compared to 0
plot_respiratory_hr3 <- plot_ind_hr(lag_num = 6,
                                       mod = mod_respiratory_heat_risk, 
                                       outcome = 'death',
                                       moa = 'pct_chg',
                                       hr_level = '3',
                                       estimates = estimates_respiratory_heat_risk, 
                                       subset = 'patients with respiratory cause of death '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'heat_risk', 'ind_lag6_respiratory_death_hr3_pctchg.png'), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of death for HR 2 compared to 0
plot_respiratory_hr2 <- plot_ind_hr(lag_num = 6,
                                       mod = mod_respiratory_heat_risk, 
                                       outcome = 'death',
                                       moa = 'pct_chg',
                                       hr_level = '2',
                                       estimates = estimates_respiratory_heat_risk, 
                                       subset = 'patients with respiratory cause of death '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'heat_risk', 'ind_lag6_respiratory_death_hr2_pctchg.png'), 
       height = 8, width = 12 )

################################################################################-
## 1c: Plot relative risk over 6 days lag (stroke) ----
################################################################################-

### Mean temperature ----

# Plot 6 days of lag relative risk of death for the 95th percentile of temperature compared to the median observed temperature
plot_stroke_tmean_per95 <- plot_ind_tmean(lag_num = 6,
                                               mod = mod_stroke_tmean, 
                                               outcome = 'death',
                                               moa = 'pct_chg',
                                               percentile = 'per_95',
                                               temp_var = 'tmean', 
                                               estimates = estimates_stroke_tmean, 
                                               subset = 'patients with stroke cause of death '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'tmean', 'ind_lag6_stroke_death_per95_pctchg.png'), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of death for the 5th percentile of temperature compared to the median observed temperature
plot_stroke_tmean_per05 <- plot_ind_tmean(lag_num = 6,
                                               mod = mod_stroke_tmean, 
                                               outcome = 'death',
                                               moa = 'pct_chg',
                                               percentile = 'per_05',
                                               temp_var = 'tmean', 
                                               estimates = estimates_stroke_tmean, 
                                               subset = 'patients with stroke cause of death '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'tmean', 'ind_lag6_stroke_death_per05_pctchg.png'), 
       height = 8, width = 12 )

### HeatRisk ----

# Plot 6 days of lag relative risk of death for HR 3 compared to 0
plot_stroke_hr3 <- plot_ind_hr(lag_num = 6,
                                    mod = mod_stroke_heat_risk, 
                                    outcome = 'death',
                                    moa = 'pct_chg',
                                    hr_level = '3',
                                    estimates = estimates_stroke_heat_risk, 
                                    subset = 'patients with stroke cause of death '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'heat_risk', 'ind_lag6_stroke_death_hr3_pctchg.png'), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of death for HR 2 compared to 0
plot_stroke_hr2 <- plot_ind_hr(lag_num = 6,
                                    mod = mod_stroke_heat_risk, 
                                    outcome = 'death',
                                    moa = 'pct_chg',
                                    hr_level = '2',
                                    estimates = estimates_stroke_heat_risk, 
                                    subset = 'patients with stroke cause of death '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'heat_risk', 'ind_lag6_stroke_death_hr2_pctchg.png'), 
       height = 8, width = 12 )

################################################################################-
## 1d: Plot relative risk over 6 days lag (intentinjury) ----
################################################################################-

### Mean temperature ----

# Plot 6 days of lag relative risk of death for the 95th percentile of temperature compared to the median observed temperature
plot_intentinjury_tmean_per95 <- plot_ind_tmean(lag_num = 6,
                                               mod = mod_intentinjury_tmean, 
                                               outcome = 'death',
                                               moa = 'pct_chg',
                                               percentile = 'per_95',
                                               temp_var = 'tmean', 
                                               estimates = estimates_intentinjury_tmean, 
                                               subset = 'patients with intentinjury cause of death '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'tmean', 'ind_lag6_intentinjury_death_per95_pctchg.png'), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of death for the 5th percentile of temperature compared to the median observed temperature
plot_intentinjury_tmean_per05 <- plot_ind_tmean(lag_num = 6,
                                               mod = mod_intentinjury_tmean, 
                                               outcome = 'death',
                                               moa = 'pct_chg',
                                               percentile = 'per_05',
                                               temp_var = 'tmean', 
                                               estimates = estimates_intentinjury_tmean, 
                                               subset = 'patients with intentinjury cause of death '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'tmean', 'ind_lag6_intentinjury_death_per05_pctchg.png'), 
       height = 8, width = 12 )

### HeatRisk ----

# Plot 6 days of lag relative risk of death for HR 3 compared to 0
plot_intentinjury_hr3 <- plot_ind_hr(lag_num = 6,
                                    mod = mod_intentinjury_heat_risk, 
                                    outcome = 'death',
                                    moa = 'pct_chg',
                                    hr_level = '3',
                                    estimates = estimates_intentinjury_heat_risk, 
                                    subset = 'patients with intentinjury cause of death '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'heat_risk', 'ind_lag6_intentinjury_death_hr3_pctchg.png'), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of death for HR 2 compared to 0
plot_intentinjury_hr2 <- plot_ind_hr(lag_num = 6,
                                    mod = mod_intentinjury_heat_risk, 
                                    outcome = 'death',
                                    moa = 'pct_chg',
                                    hr_level = '2',
                                    estimates = estimates_intentinjury_heat_risk, 
                                    subset = 'patients with intentinjury cause of death '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'heat_risk', 'ind_lag6_intentinjury_death_hr2_pctchg.png'), 
       height = 8, width = 12 )

################################################################################-
## 1e: Plot relative risk over 6 days lag (unintentinjury) ----
################################################################################-

### Mean temperature ----

# Plot 6 days of lag relative risk of death for the 95th percentile of temperature compared to the median observed temperature
plot_unintentinjury_tmean_per95 <- plot_ind_tmean(lag_num = 6,
                                               mod = mod_unintentinjury_tmean, 
                                               outcome = 'death',
                                               moa = 'pct_chg',
                                               percentile = 'per_95',
                                               temp_var = 'tmean', 
                                               estimates = estimates_unintentinjury_tmean, 
                                               subset = 'patients with unintentinjury cause of death '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'tmean', 'ind_lag6_unintentinjury_death_per95_pctchg.png'), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of death for the 5th percentile of temperature compared to the median observed temperature
plot_unintentinjury_tmean_per05 <- plot_ind_tmean(lag_num = 6,
                                               mod = mod_unintentinjury_tmean, 
                                               outcome = 'death',
                                               moa = 'pct_chg',
                                               percentile = 'per_05',
                                               temp_var = 'tmean', 
                                               estimates = estimates_unintentinjury_tmean, 
                                               subset = 'patients with unintentinjury cause of death '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'tmean', 'ind_lag6_unintentinjury_death_per05_pctchg.png'), 
       height = 8, width = 12 )

### HeatRisk ----

# Plot 6 days of lag relative risk of death for HR 3 compared to 0
plot_unintentinjury_hr3 <- plot_ind_hr(lag_num = 6,
                                    mod = mod_unintentinjury_heat_risk, 
                                    outcome = 'death',
                                    moa = 'pct_chg',
                                    hr_level = '3',
                                    estimates = estimates_unintentinjury_heat_risk, 
                                    subset = 'patients with unintentinjury cause of death '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'heat_risk', 'ind_lag6_unintentinjury_death_hr3_pctchg.png'), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of death for HR 2 compared to 0
plot_unintentinjury_hr2 <- plot_ind_hr(lag_num = 6,
                                    mod = mod_unintentinjury_heat_risk, 
                                    outcome = 'death',
                                    moa = 'pct_chg',
                                    hr_level = '2',
                                    estimates = estimates_unintentinjury_heat_risk, 
                                    subset = 'patients with unintentinjury cause of death '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'heat_risk', 'ind_lag6_unintentinjury_death_hr2_pctchg.png'), 
       height = 8, width = 12 )

################################################################################-
## 1f: Plot relative risk over 6 days lag (substance) ----
################################################################################-

### Mean temperature ----

# Plot 6 days of lag relative risk of death for the 95th percentile of temperature compared to the median observed temperature
plot_substance_tmean_per95 <- plot_ind_tmean(lag_num = 6,
                                               mod = mod_substance_tmean, 
                                               outcome = 'death',
                                               moa = 'pct_chg',
                                               percentile = 'per_95',
                                               temp_var = 'tmean', 
                                               estimates = estimates_substance_tmean, 
                                               subset = 'patients with substance cause of death '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'tmean', 'ind_lag6_substance_death_per95_pctchg.png'), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of death for the 5th percentile of temperature compared to the median observed temperature
plot_substance_tmean_per05 <- plot_ind_tmean(lag_num = 6,
                                               mod = mod_substance_tmean, 
                                               outcome = 'death',
                                               moa = 'pct_chg',
                                               percentile = 'per_05',
                                               temp_var = 'tmean', 
                                               estimates = estimates_substance_tmean, 
                                               subset = 'patients with substance cause of death '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'tmean', 'ind_lag6_substance_death_per05_pctchg.png'), 
       height = 8, width = 12 )

### HeatRisk ----

# Plot 6 days of lag relative risk of death for HR 3 compared to 0
plot_substance_hr3 <- plot_ind_hr(lag_num = 6,
                                    mod = mod_substance_heat_risk, 
                                    outcome = 'death',
                                    moa = 'pct_chg',
                                    hr_level = '3',
                                    estimates = estimates_substance_heat_risk, 
                                    subset = 'patients with substance cause of death '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'heat_risk', 'ind_lag6_substance_death_hr3_pctchg.png'), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of death for HR 2 compared to 0
plot_substance_hr2 <- plot_ind_hr(lag_num = 6,
                                    mod = mod_substance_heat_risk, 
                                    outcome = 'death',
                                    moa = 'pct_chg',
                                    hr_level = '2',
                                    estimates = estimates_substance_heat_risk, 
                                    subset = 'patients with substance cause of death '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'heat_risk', 'ind_lag6_substance_death_hr2_pctchg.png'), 
       height = 8, width = 12 )

################################################################################-
## 1g: Plot relative risk over 6 days lag (renal) ----
################################################################################-

### Mean temperature ----

# Plot 6 days of lag relative risk of death for the 95th percentile of temperature compared to the median observed temperature
plot_renal_tmean_per95 <- plot_ind_tmean(lag_num = 6,
                                               mod = mod_renal_tmean, 
                                               outcome = 'death',
                                               moa = 'pct_chg',
                                               percentile = 'per_95',
                                               temp_var = 'tmean', 
                                               estimates = estimates_renal_tmean, 
                                               subset = 'patients with renal cause of death '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'tmean', 'ind_lag6_renal_death_per95_pctchg.png'), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of death for the 5th percentile of temperature compared to the median observed temperature
plot_renal_tmean_per05 <- plot_ind_tmean(lag_num = 6,
                                               mod = mod_renal_tmean, 
                                               outcome = 'death',
                                               moa = 'pct_chg',
                                               percentile = 'per_05',
                                               temp_var = 'tmean', 
                                               estimates = estimates_renal_tmean, 
                                               subset = 'patients with renal cause of death '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'tmean', 'ind_lag6_renal_death_per05_pctchg.png'), 
       height = 8, width = 12 )

### HeatRisk ----

# Plot 6 days of lag relative risk of death for HR 3 compared to 0
plot_renal_hr3 <- plot_ind_hr(lag_num = 6,
                                    mod = mod_renal_heat_risk, 
                                    outcome = 'death',
                                    moa = 'pct_chg',
                                    hr_level = '3',
                                    estimates = estimates_renal_heat_risk, 
                                    subset = 'patients with renal cause of death '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'heat_risk', 'ind_lag6_renal_death_hr3_pctchg.png'), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of death for HR 2 compared to 0
plot_renal_hr2 <- plot_ind_hr(lag_num = 6,
                                    mod = mod_renal_heat_risk, 
                                    outcome = 'death',
                                    moa = 'pct_chg',
                                    hr_level = '2',
                                    estimates = estimates_renal_heat_risk, 
                                    subset = 'patients with renal cause of death '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'heat_risk', 'ind_lag6_renal_death_hr2_pctchg.png'), 
       height = 8, width = 12 )

################################################################################-
## 1h: Plot relative risk over 6 days lag (genitourinary) ----
################################################################################-

### Mean temperature ----

# Plot 6 days of lag relative risk of death for the 95th percentile of temperature compared to the median observed temperature
plot_genitourinary_tmean_per95 <- plot_ind_tmean(lag_num = 6,
                                               mod = mod_genitourinary_tmean, 
                                               outcome = 'death',
                                               moa = 'pct_chg',
                                               percentile = 'per_95',
                                               temp_var = 'tmean', 
                                               estimates = estimates_genitourinary_tmean, 
                                               subset = 'patients with genitourinary cause of death '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'tmean', 'ind_lag6_genitourinary_death_per95_pctchg.png'), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of death for the 5th percentile of temperature compared to the median observed temperature
plot_genitourinary_tmean_per05 <- plot_ind_tmean(lag_num = 6,
                                               mod = mod_genitourinary_tmean, 
                                               outcome = 'death',
                                               moa = 'pct_chg',
                                               percentile = 'per_05',
                                               temp_var = 'tmean', 
                                               estimates = estimates_genitourinary_tmean, 
                                               subset = 'patients with genitourinary cause of death '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'tmean', 'ind_lag6_genitourinary_death_per05_pctchg.png'), 
       height = 8, width = 12 )

### HeatRisk ----

# Plot 6 days of lag relative risk of death for HR 3 compared to 0
plot_genitourinary_hr3 <- plot_ind_hr(lag_num = 6,
                                    mod = mod_genitourinary_heat_risk, 
                                    outcome = 'death',
                                    moa = 'pct_chg',
                                    hr_level = '3',
                                    estimates = estimates_genitourinary_heat_risk, 
                                    subset = 'patients with genitourinary cause of death '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'heat_risk', 'ind_lag6_genitourinary_death_hr3_pctchg.png'), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of death for HR 2 compared to 0
plot_genitourinary_hr2 <- plot_ind_hr(lag_num = 6,
                                    mod = mod_genitourinary_heat_risk, 
                                    outcome = 'death',
                                    moa = 'pct_chg',
                                    hr_level = '2',
                                    estimates = estimates_genitourinary_heat_risk, 
                                    subset = 'patients with genitourinary cause of death '
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'heat_risk', 'ind_lag6_genitourinary_death_hr2_pctchg.png'), 
       height = 8, width = 12 )

################################################################################-
## 2: Clean up ----
################################################################################-

rm(data_source_name, 
   selected_constraints_death, cause_grid,
   estimates_cardiovascular_tmean, estimates_cardiovascular_heat_risk,
   estimates_respiratory_tmean, estimates_respiratory_heat_risk,
   estimates_stroke_tmean, estimates_stroke_heat_risk,
   estimates_intentinjury_tmean, estimates_intentinjury_heat_risk,
   estimates_unintentinjury_tmean, estimates_unintentinjury_heat_risk,
   estimates_substance_tmean, estimates_substance_heat_risk,
   estimates_genitourinary_tmean, estimates_genitourinary_heat_risk,
   mod_cardiovascular_tmean, mod_cardiovascular_heat_risk,
   mod_respiratory_tmean, mod_respiratory_heat_risk,
   mod_stroke_tmean, mod_stroke_heat_risk,
   mod_intentinjury_tmean, mod_intentinjury_heat_risk,
   mod_unintentinjury_tmean, mod_unintentinjury_heat_risk,
   mod_substance_tmean, mod_substance_heat_risk,
   mod_genitourinary_tmean, mod_genitourinary_heat_risk,
   plot_cardiovascular_tmean_per95, plot_cardiovascular_tmean_per05,
   plot_cardiovascular_hr3, plot_cardiovascular_hr2,
   plot_respiratory_tmean_per95, plot_respiratory_tmean_per05,
   plot_respiratory_hr3, plot_respiratory_hr2,
   plot_stroke_tmean_per95, plot_stroke_tmean_per05,
   plot_stroke_hr3, plot_stroke_hr2,
   plot_intentinjury_tmean_per95, plot_intentinjury_tmean_per05,
   plot_intentinjury_hr3, plot_intentinjury_hr2,
   plot_unintentinjury_tmean_per95, plot_unintentinjury_tmean_per05,
   plot_unintentinjury_hr3, plot_unintentinjury_hr2,
   plot_substance_tmean_per95, plot_substance_tmean_per05,
   plot_substance_hr3, plot_substance_hr2,
   plot_renal_tmean_per95, plot_renal_tmean_per05,
   plot_renal_hr3, plot_renal_hr2,
   plot_genitourinary_tmean_per95, plot_genitourinary_tmean_per05,
   plot_genitourinary_hr3, plot_genitourinary_hr2
   )