################################################################################-
# Plot: Cumulative of lags 0-6 for mean temperatures and HeatRisk, deaths, cause ----
# Comparing HeatRisk and temperature for local heat surveillance: A time-stratified 
#  case-crossover study of emergency department visits in San Mateo County, 
#  California, 2017–2025
# Juliana GE Bartels
# Updated: 6/22/2026
################################################################################-

# This file plots cumulative percent change in death rate for lag 0-6 days,
# by cause. 

################################################################################-
## 0: Upload data ----
################################################################################-

# Generate suffix for file calling and writing, based on data source
data_source_name <- case_when(data_source == 'original' ~ '',
                              data_source == 'synth' ~ '_synth')

# Load estimates and models for cause EMMs
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

load_estimates(selected_constraints_death, 'death', 'cause', cause_grid, 'cum', data_source_name)

# Set number of lags for cumulative plots
lag_num <- 6

################################################################################-
## 1a: Plot cumulative risk, (cardiovascular-) ----
################################################################################-

### Mean temperature ----

# Plot cumulative relative risk of death for all temperatures compared to the median observed temperature
plot_cum_tmean(lag_num = lag_num,
               mod = mod_cardiovascular_tmean, 
               outcome = 'death',
               moa = 'rr',
               temp_var = 'tmean', 
               estimates = estimates_cardiovascular_tmean, 
               subset = 'patients with cardiovascular cause of death ',
               with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'tmean', paste0('cum_lag', lag_num, '_cardiovascular_death_rr.png')), 
       height = 8, width = 12 )

# Plot cumulative percent change of death for all temperatures compared to the median observed temperature
plot_cum_tmean(lag_num = lag_num,
               mod = mod_cardiovascular_tmean, 
               outcome = 'death',
               moa = 'pct_chg',
               temp_var = 'tmean', 
               estimates = estimates_cardiovascular_tmean, 
               subset = 'patients with cardiovascular cause of death ',
               with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'tmean', paste0('cum_lag', lag_num, '_cardiovascular_death_pct_chg.png')), 
       height = 8, width = 12 )

## Create plot in environment without histogram for temperature
plot_cardiovascular_tmean <- plot_cum_tmean(lag_num = lag_num,
                                            mod = mod_cardiovascular_tmean, 
                                            outcome = 'death',
                                            moa = 'pct_chg',
                                            temp_var = 'tmean', 
                                            estimates = estimates_cardiovascular_tmean, 
                                            subset = 'patients with cardiovascular cause of death ',
                                            with_hist = FALSE
)

### HeatRisk ----

# Plot cumulative relative risk of death for all temperatures compared to the median observed temperature
plot_cum_hr(lag_num = lag_num,
            mod = mod_cardiovascular_heat_risk, 
            outcome = 'death',
            moa = 'rr',
            temp_var = 'heat_risk', 
            estimates = estimates_cardiovascular_heat_risk,
            subset = 'patients with cardiovascular cause of death ',
            with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'heat_risk', paste0('cum_lag', lag_num, '_cardiovascular_death_rr.png')), 
       height = 8, width = 12 )

# Plot cumulative percent change of death for all temperatures compared to the median observed temperature
plot_cum_hr(lag_num = lag_num,
            mod = mod_cardiovascular_heat_risk, 
            outcome = 'death',
            moa = 'pct_chg',
            temp_var = 'heat_risk', 
            estimates = estimates_cardiovascular_heat_risk,
            subset = 'patients with cardiovascular cause of death ',
            with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'heat_risk', paste0('cum_lag', lag_num, '_cardiovascular_death_pct_chg.png')), 
       height = 8, width = 12 )

## Create plot in environment without histogram for temperature
plot_cardiovascular_heat_risk <- plot_cum_hr(lag_num = lag_num,
                                             mod = mod_cardiovascular_heat_risk, 
                                             outcome = 'death',
                                             moa = 'pct_chg',
                                             temp_var = 'heat_risk', 
                                             estimates = estimates_cardiovascular_heat_risk,
                                             subset = 'patients with cardiovascular cause of death ',
                                             with_hist = FALSE
)

################################################################################-
## 1b: Plot cumulative risk, (respiratory ) ----
################################################################################-

### Mean temperature ----

# Plot cumulative relative risk of death for all temperatures compared to the median observed temperature
plot_cum_tmean(lag_num = lag_num,
               mod = mod_respiratory_tmean, 
               outcome = 'death',
               moa = 'rr',
               temp_var = 'tmean', 
               estimates = estimates_respiratory_tmean, 
               subset = 'patients with respiratory cause of death ',
               with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'tmean', paste0('cum_lag', lag_num, '_respiratory_death_rr.png')), 
       height = 8, width = 12 )

# Plot cumulative percent change of death for all temperatures compared to the median observed temperature
plot_cum_tmean(lag_num = lag_num,
               mod = mod_respiratory_tmean, 
               outcome = 'death',
               moa = 'pct_chg',
               temp_var = 'tmean', 
               estimates = estimates_respiratory_tmean, 
               subset = 'patients with respiratory cause of death ',
               with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'tmean', paste0('cum_lag', lag_num, '_respiratory_death_pct_chg.png')), 
       height = 8, width = 12 )

## Create plot in environment without histogram for temperature
plot_respiratory_tmean <- plot_cum_tmean(lag_num = lag_num,
                                         mod = mod_respiratory_tmean, 
                                         outcome = 'death',
                                         moa = 'pct_chg',
                                         temp_var = 'tmean', 
                                         estimates = estimates_respiratory_tmean, 
                                         subset = 'patients with respiratory cause of death ',
                                         with_hist = FALSE
)

### HeatRisk ----

# Plot cumulative relative risk of death for all temperatures compared to the median observed temperature
plot_cum_hr(lag_num = lag_num,
            mod = mod_respiratory_heat_risk, 
            outcome = 'death',
            moa = 'rr',
            temp_var = 'heat_risk', 
            estimates = estimates_respiratory_heat_risk,
            subset = 'patients with respiratory cause of death ',
            with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'heat_risk', paste0('cum_lag', lag_num, '_respiratory_death_rr.png')), 
       height = 8, width = 12 )

# Plot cumulative percent change of death for all temperatures compared to the median observed temperature
plot_cum_hr(lag_num = lag_num,
            mod = mod_respiratory_heat_risk, 
            outcome = 'death',
            moa = 'pct_chg',
            temp_var = 'heat_risk', 
            estimates = estimates_respiratory_heat_risk,
            subset = 'patients with respiratory cause of death ',
            with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'heat_risk', paste0('cum_lag', lag_num, '_respiratory_death_pct_chg.png')), 
       height = 8, width = 12 )

## Create plot in environment without histogram for temperature
plot_respiratory_heat_risk <- plot_cum_hr(lag_num = lag_num,
                                          mod = mod_respiratory_heat_risk, 
                                          outcome = 'death',
                                          moa = 'pct_chg',
                                          temp_var = 'heat_risk', 
                                          estimates = estimates_respiratory_heat_risk,
                                          subset = 'patients with respiratory cause of death ',
                                          with_hist = FALSE
)

################################################################################-
## 1c: Plot cumulative risk, (stroke) ----
################################################################################-

### Mean temperature ----

# Plot cumulative relative risk of death for all temperatures compared to the median observed temperature
plot_cum_tmean(lag_num = lag_num,
               mod = mod_stroke_tmean, 
               outcome = 'death',
               moa = 'rr',
               temp_var = 'tmean', 
               estimates = estimates_stroke_tmean, 
               subset = 'patients with stroke as cause of death ',
               with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'tmean', paste0('cum_lag', lag_num, '_stroke_death_rr.png')), 
       height = 8, width = 12 )

# Plot cumulative percent change of death for all temperatures compared to the median observed temperature
plot_cum_tmean(lag_num = lag_num,
               mod = mod_stroke_tmean, 
               outcome = 'death',
               moa = 'pct_chg',
               temp_var = 'tmean', 
               estimates = estimates_stroke_tmean, 
               subset = 'patients with stroke as cause of death ',
               with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'tmean', paste0('cum_lag', lag_num, '_stroke_death_pct_chg.png')), 
       height = 8, width = 12 )

## Create plot in environment without histogram for temperature
plot_stroke_tmean <- plot_cum_tmean(lag_num = lag_num,
                                    mod = mod_stroke_tmean, 
                                    outcome = 'death',
                                    moa = 'pct_chg',
                                    temp_var = 'tmean', 
                                    estimates = estimates_stroke_tmean, 
                                    subset = 'patients with stroke as cause of death ',
                                    with_hist = FALSE
)

### HeatRisk ----

# Plot cumulative relative risk of death for all temperatures compared to the median observed temperature
plot_cum_hr(lag_num = lag_num,
            mod = mod_stroke_heat_risk, 
            outcome = 'death',
            moa = 'rr',
            temp_var = 'heat_risk', 
            estimates = estimates_stroke_heat_risk,
            subset = 'patients with stroke as cause of death ',
            with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'heat_risk', paste0('cum_lag', lag_num, '_stroke_death_rr.png')), 
       height = 8, width = 12 )

# Plot cumulative percent change of death for all temperatures compared to the median observed temperature
plot_cum_hr(lag_num = lag_num,
            mod = mod_stroke_heat_risk, 
            outcome = 'death',
            moa = 'pct_chg',
            temp_var = 'heat_risk', 
            estimates = estimates_stroke_heat_risk,
            subset = 'patients with stroke as cause of death ',
            with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'heat_risk', paste0('cum_lag', lag_num, '_stroke_death_pct_chg.png')), 
       height = 8, width = 12 )

## Create plot in environment without histogram for temperature
plot_stroke_heat_risk <- plot_cum_hr(lag_num = lag_num,
                                     mod = mod_stroke_heat_risk, 
                                     outcome = 'death',
                                     moa = 'pct_chg',
                                     temp_var = 'heat_risk', 
                                     estimates = estimates_stroke_heat_risk,
                                     subset = 'patients with stroke as cause of death ',
                                     with_hist = FALSE
)

################################################################################-
## 1d: Plot cumulative risk, (intentinjury) ----
################################################################################-

### Mean temperature ----

# Plot cumulative relative risk of death for all temperatures compared to the median observed temperature
plot_cum_tmean(lag_num = lag_num,
               mod = mod_intentinjury_tmean, 
               outcome = 'death',
               moa = 'rr',
               temp_var = 'tmean', 
               estimates = estimates_intentinjury_tmean, 
               subset = 'patients with intentional injury as cause of death ',
               with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'tmean', paste0('cum_lag', lag_num, '_intentinjury_death_rr.png')), 
       height = 8, width = 12 )

# Plot cumulative percent change of death for all temperatures compared to the median observed temperature
plot_cum_tmean(lag_num = lag_num,
               mod = mod_intentinjury_tmean, 
               outcome = 'death',
               moa = 'pct_chg',
               temp_var = 'tmean', 
               estimates = estimates_intentinjury_tmean, 
               subset = 'patients with intentional injury as cause of death ',
               with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'tmean', paste0('cum_lag', lag_num, '_intentinjury_death_pct_chg.png')), 
       height = 8, width = 12 )

## Create plot in environment without histogram for temperature
plot_intentinjury_tmean <- plot_cum_tmean(lag_num = lag_num,
                                            mod = mod_intentinjury_tmean, 
                                            outcome = 'death',
                                            moa = 'pct_chg',
                                            temp_var = 'tmean', 
                                            estimates = estimates_intentinjury_tmean, 
                                            subset = 'patients with intentional injury as cause of death ',
                                            with_hist = FALSE
)

### HeatRisk ----

# Plot cumulative relative risk of death for all temperatures compared to the median observed temperature
plot_cum_hr(lag_num = lag_num,
            mod = mod_intentinjury_heat_risk, 
            outcome = 'death',
            moa = 'rr',
            temp_var = 'heat_risk', 
            estimates = estimates_intentinjury_heat_risk,
            subset = 'patients with intentional injury as cause of death ',
            with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'heat_risk', paste0('cum_lag', lag_num, '_intentinjury_death_rr.png')), 
       height = 8, width = 12 )

# Plot cumulative percent change of death for all temperatures compared to the median observed temperature
plot_cum_hr(lag_num = lag_num,
            mod = mod_intentinjury_heat_risk, 
            outcome = 'death',
            moa = 'pct_chg',
            temp_var = 'heat_risk', 
            estimates = estimates_intentinjury_heat_risk,
            subset = 'patients with intentional injury as cause of death ',
            with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'heat_risk', paste0('cum_lag', lag_num, '_intentinjury_death_pct_chg.png')), 
       height = 8, width = 12 )

## Create plot in environment without histogram for temperature
plot_intentinjury_heat_risk <- plot_cum_hr(lag_num = lag_num,
                                             mod = mod_intentinjury_heat_risk, 
                                             outcome = 'death',
                                             moa = 'pct_chg',
                                             temp_var = 'heat_risk', 
                                             estimates = estimates_intentinjury_heat_risk,
                                             subset = 'patients with intentional injury as cause of death ',
                                             with_hist = FALSE
)

################################################################################-
## 1e: Plot cumulative risk, (unintentinjury) ----
################################################################################-

### Mean temperature ----

# Plot cumulative relative risk of death for all temperatures compared to the median observed temperature
plot_cum_tmean(lag_num = lag_num,
               mod = mod_unintentinjury_tmean, 
               outcome = 'death',
               moa = 'rr',
               temp_var = 'tmean', 
               estimates = estimates_unintentinjury_tmean, 
               subset = 'patients with unintentional injury as cause of death ',
               with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'tmean', paste0('cum_lag', lag_num, '_unintentinjury_death_rr.png')), 
       height = 8, width = 12 )

# Plot cumulative percent change of death for all temperatures compared to the median observed temperature
plot_cum_tmean(lag_num = lag_num,
               mod = mod_unintentinjury_tmean, 
               outcome = 'death',
               moa = 'pct_chg',
               temp_var = 'tmean', 
               estimates = estimates_unintentinjury_tmean, 
               subset = 'patients with unintentional injury as cause of death ',
               with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'tmean', paste0('cum_lag', lag_num, '_unintentinjury_death_pct_chg.png')), 
       height = 8, width = 12 )

## Create plot in environment without histogram for temperature
plot_unintentinjury_tmean <- plot_cum_tmean(lag_num = lag_num,
                                            mod = mod_unintentinjury_tmean, 
                                            outcome = 'death',
                                            moa = 'pct_chg',
                                            temp_var = 'tmean', 
                                            estimates = estimates_unintentinjury_tmean, 
                                            subset = 'patients with unintentional injury as cause of death ',
                                            with_hist = FALSE
)

### HeatRisk ----

# Plot cumulative relative risk of death for all temperatures compared to the median observed temperature
plot_cum_hr(lag_num = lag_num,
            mod = mod_unintentinjury_heat_risk, 
            outcome = 'death',
            moa = 'rr',
            temp_var = 'heat_risk', 
            estimates = estimates_unintentinjury_heat_risk,
            subset = 'patients with unintentional injury as cause of death ',
            with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'heat_risk', paste0('cum_lag', lag_num, '_unintentinjury_death_rr.png')), 
       height = 8, width = 12 )

# Plot cumulative percent change of death for all temperatures compared to the median observed temperature
plot_cum_hr(lag_num = lag_num,
            mod = mod_unintentinjury_heat_risk, 
            outcome = 'death',
            moa = 'pct_chg',
            temp_var = 'heat_risk', 
            estimates = estimates_unintentinjury_heat_risk,
            subset = 'patients with unintentional injury as cause of death ',
            with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'heat_risk', paste0('cum_lag', lag_num, '_unintentinjury_death_pct_chg.png')), 
       height = 8, width = 12 )

## Create plot in environment without histogram for temperature
plot_unintentinjury_heat_risk <- plot_cum_hr(lag_num = lag_num,
                                             mod = mod_unintentinjury_heat_risk, 
                                             outcome = 'death',
                                             moa = 'pct_chg',
                                             temp_var = 'heat_risk', 
                                             estimates = estimates_unintentinjury_heat_risk,
                                             subset = 'patients with unintentional injury as cause of death ',
                                             with_hist = FALSE
)

################################################################################-
## 1f: Plot cumulative risk, (substance) ----
################################################################################-

### Mean temperature ----

# Plot cumulative relative risk of death for all temperatures compared to the median observed temperature
plot_cum_tmean(lag_num = lag_num,
               mod = mod_substance_tmean, 
               outcome = 'death',
               moa = 'rr',
               temp_var = 'tmean', 
               estimates = estimates_substance_tmean, 
               subset = 'patients with substance use as cause of death ',
               with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'tmean', paste0('cum_lag', lag_num, '_substance_death_rr.png')), 
       height = 8, width = 12 )

# Plot cumulative percent change of death for all temperatures compared to the median observed temperature
plot_cum_tmean(lag_num = lag_num,
               mod = mod_substance_tmean, 
               outcome = 'death',
               moa = 'pct_chg',
               temp_var = 'tmean', 
               estimates = estimates_substance_tmean, 
               subset = 'patients with substance use as cause of death ',
               with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'tmean', paste0('cum_lag', lag_num, '_substance_death_pct_chg.png')), 
       height = 8, width = 12 )

## Create plot in environment without histogram for temperature
plot_substance_tmean <- plot_cum_tmean(lag_num = lag_num,
                                       mod = mod_substance_tmean, 
                                       outcome = 'death',
                                       moa = 'pct_chg',
                                       temp_var = 'tmean', 
                                       estimates = estimates_substance_tmean, 
                                       subset = 'patients with substance use as cause of death ',
                                       with_hist = FALSE
)

### HeatRisk ----

# Plot cumulative relative risk of death for all temperatures compared to the median observed temperature
plot_cum_hr(lag_num = lag_num,
            mod = mod_substance_heat_risk, 
            outcome = 'death',
            moa = 'rr',
            temp_var = 'heat_risk', 
            estimates = estimates_substance_heat_risk,
            subset = 'patients with substance use as cause of death ',
            with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'heat_risk', paste0('cum_lag', lag_num, '_substance_death_rr.png')), 
       height = 8, width = 12 )

# Plot cumulative percent change of death for all temperatures compared to the median observed temperature
plot_cum_hr(lag_num = lag_num,
            mod = mod_substance_heat_risk, 
            outcome = 'death',
            moa = 'pct_chg',
            temp_var = 'heat_risk', 
            estimates = estimates_substance_heat_risk,
            subset = 'patients with substance use as cause of death ',
            with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'heat_risk', paste0('cum_lag', lag_num, '_substance_death_pct_chg.png')), 
       height = 8, width = 12 )

## Create plot in environment without histogram for temperature
plot_substance_heat_risk <- plot_cum_hr(lag_num = lag_num,
                                        mod = mod_substance_heat_risk, 
                                        outcome = 'death',
                                        moa = 'pct_chg',
                                        temp_var = 'heat_risk', 
                                        estimates = estimates_substance_heat_risk,
                                        subset = 'patients with substance use as cause of death ',
                                        with_hist = FALSE
)

################################################################################-
## 1g: Plot cumulative risk, (renal) ----
################################################################################-

### Mean temperature ----

# Plot cumulative relative risk of death for all temperatures compared to the median observed temperature
plot_cum_tmean(lag_num = lag_num,
               mod = mod_renal_tmean, 
               outcome = 'death',
               moa = 'rr',
               temp_var = 'tmean', 
               estimates = estimates_renal_tmean, 
               subset = 'patients with renal cause of death ',
               with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'tmean', paste0('cum_lag', lag_num, '_renal_death_rr.png')), 
       height = 8, width = 12 )

# Plot cumulative percent change of death for all temperatures compared to the median observed temperature
plot_cum_tmean(lag_num = lag_num,
               mod = mod_renal_tmean, 
               outcome = 'death',
               moa = 'pct_chg',
               temp_var = 'tmean', 
               estimates = estimates_renal_tmean, 
               subset = 'patients with renal cause of death ',
               with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'tmean', paste0('cum_lag', lag_num, '_renal_death_pct_chg.png')), 
       height = 8, width = 12 )

## Create plot in environment without histogram for temperature
plot_renal_tmean <- plot_cum_tmean(lag_num = lag_num,
                                   mod = mod_renal_tmean, 
                                   outcome = 'death',
                                   moa = 'pct_chg',
                                   temp_var = 'tmean', 
                                   estimates = estimates_renal_tmean, 
                                   subset = 'patients with renal cause of death ',
                                   with_hist = FALSE
)

### HeatRisk ----

# Plot cumulative relative risk of death for all temperatures compared to the median observed temperature
plot_cum_hr(lag_num = lag_num,
            mod = mod_renal_heat_risk, 
            outcome = 'death',
            moa = 'rr',
            temp_var = 'heat_risk', 
            estimates = estimates_renal_heat_risk,
            subset = 'patients with renal as cause of death ',
            with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'heat_risk', paste0('cum_lag', lag_num, '_renal_death_rr.png')), 
       height = 8, width = 12 )

# Plot cumulative percent change of death for all temperatures compared to the median observed temperature
plot_cum_hr(lag_num = lag_num,
            mod = mod_renal_heat_risk, 
            outcome = 'death',
            moa = 'pct_chg',
            temp_var = 'heat_risk', 
            estimates = estimates_renal_heat_risk,
            subset = 'patients with renal cause of death ',
            with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'heat_risk', paste0('cum_lag', lag_num, '_renal_death_pct_chg.png')), 
       height = 8, width = 12 )

## Create plot in environment without histogram for temperature
plot_renal_heat_risk <- plot_cum_hr(lag_num = lag_num,
                                    mod = mod_renal_heat_risk, 
                                    outcome = 'death',
                                    moa = 'pct_chg',
                                    temp_var = 'heat_risk', 
                                    estimates = estimates_renal_heat_risk,
                                    subset = 'patients with renal as cause of death ',
                                    with_hist = FALSE
)

################################################################################-
## 1h: Plot cumulative risk, (genitourinary) ----
################################################################################-

### Mean temperature ----

# Plot cumulative relative risk of death for all temperatures compared to the median observed temperature
plot_cum_tmean(lag_num = lag_num,
               mod = mod_genitourinary_tmean, 
               outcome = 'death',
               moa = 'rr',
               temp_var = 'tmean', 
               estimates = estimates_genitourinary_tmean, 
               subset = 'patients with genitourinary cause of death ',
               with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'tmean', paste0('cum_lag', lag_num, '_genitourinary_death_rr.png')), 
       height = 8, width = 12 )

# Plot cumulative percent change of death for all temperatures compared to the median observed temperature
plot_cum_tmean(lag_num = lag_num,
               mod = mod_genitourinary_tmean, 
               outcome = 'death',
               moa = 'pct_chg',
               temp_var = 'tmean', 
               estimates = estimates_genitourinary_tmean, 
               subset = 'patients with genitourinary cause of death ',
               with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'tmean', paste0('cum_lag', lag_num, '_genitourinary_death_pct_chg.png')), 
       height = 8, width = 12 )

## Create plot in environment without histogram for temperature
plot_genitourinary_tmean <- plot_cum_tmean(lag_num = lag_num,
                                           mod = mod_genitourinary_tmean, 
                                           outcome = 'death',
                                           moa = 'pct_chg',
                                           temp_var = 'tmean', 
                                           estimates = estimates_genitourinary_tmean, 
                                           subset = 'patients with genitourinary cause of death ',
                                           with_hist = FALSE
)

### HeatRisk ----

# Plot cumulative relative risk of death for all temperatures compared to the median observed temperature
plot_cum_hr(lag_num = lag_num,
            mod = mod_genitourinary_heat_risk, 
            outcome = 'death',
            moa = 'rr',
            temp_var = 'heat_risk', 
            estimates = estimates_genitourinary_heat_risk,
            subset = 'patients with genitourinary as cause of death ',
            with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'heat_risk', paste0('cum_lag', lag_num, '_genitourinary_death_rr.png')), 
       height = 8, width = 12 )

# Plot cumulative percent change of death for all temperatures compared to the median observed temperature
plot_cum_hr(lag_num = lag_num,
            mod = mod_genitourinary_heat_risk, 
            outcome = 'death',
            moa = 'pct_chg',
            temp_var = 'heat_risk', 
            estimates = estimates_genitourinary_heat_risk,
            subset = 'patients with genitourinary cause of death ',
            with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'heat_risk', paste0('cum_lag', lag_num, '_genitourinary_death_pct_chg.png')), 
       height = 8, width = 12 )

## Create plot in environment without histogram for temperature
plot_genitourinary_heat_risk <- plot_cum_hr(lag_num = lag_num,
                                            mod = mod_genitourinary_heat_risk, 
                                            outcome = 'death',
                                            moa = 'pct_chg',
                                            temp_var = 'heat_risk', 
                                            estimates = estimates_genitourinary_heat_risk,
                                            subset = 'patients with genitourinary as cause of death ',
                                            with_hist = FALSE
)

################################################################################-
## 2: Plot all causes cumulative risk, together ----
################################################################################-

### Mean temperature ----

#### All on same figure ----
plot_cum_tmean_together(
  lag_num = lag_num,
  mod = mod_unintentinjury_tmean,
  outcome = 'death',
  moa = 'rr',
  temp_var = 'tmean',
  group = c('Cardiovascular', 'Respiratory', 'Intentional injury', 'Unintentional injury', 'Stroke',
            'Substance use',  'Renal', 'Genitourinary'),
  estimates = list(estimates_cardiovascular_tmean, estimates_respiratory_tmean, estimates_intentinjury_tmean,
                   estimates_unintentinjury_tmean, estimates_stroke_tmean,
                   estimates_substance_tmean, 
                   estimates_renal_tmean, estimates_genitourinary_tmean),
  subset_var = 'Cause',
  with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'tmean', paste0('cum_lag', lag_num, '_allcause_death_together.png')), 
       height = 8, width = 12 )


#### Using cowplot ----

# Manually set ymin and ymax for  gridded plot, so that y axes align
ymin <- -100
ymax <- 200
ymin_scale <- -100
ymax_scale <- 200


# Plot all cause together
plot_grid(
  plot_cardiovascular_tmean + 
    coord_cartesian(ylim = c(ymin, ymax),
                    xlim = c(2, 34)) +
    scale_y_continuous(breaks = seq(ymin_scale, ymax_scale, by = 100)) +
    labs(title = 'Cardiovascular') +
    theme(plot.margin = margin(t = 1, b = 1),
          plot.title = element_text(size = 20),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()),
  plot_respiratory_tmean + 
    coord_cartesian(ylim = c(ymin, ymax),
                    xlim = c(2, 34)) +
    scale_y_continuous(breaks = seq(ymin_scale, ymax_scale, by = 100))  +
    labs(title = 'Respiratory') +
    theme(plot.margin = margin(t = 1, b = 1),
          plot.title = element_text(size = 20),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()),
  plot_genitourinary_tmean + 
    coord_cartesian(ylim = c(ymin, ymax),
                    xlim = c(2, 34)) +
    scale_y_continuous(breaks = seq(ymin_scale, ymax_scale, by = 100)) +
    labs(title = 'Genitourinary') +
    theme(plot.margin = margin(t = 1, b = 1),
          plot.title = element_text(size = 20),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()),
  plot_stroke_tmean + 
    coord_cartesian(ylim = c(ymin, ymax),
                    xlim = c(2, 34)) +
    scale_y_continuous(breaks = seq(ymin_scale, ymax_scale, by = 100)) +
    labs(title = 'Stroke') +
    theme(plot.margin = margin(t = 1, b = 1),
          plot.title = element_text(size = 20),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) ,
  plot_intentinjury_tmean + 
    coord_cartesian(ylim = c(ymin, ymax),
                    xlim = c(2, 34)) +
    scale_y_continuous(breaks = seq(ymin_scale, ymax_scale, by = 100)) +
    labs(title = 'Intentional injury') +
    theme(plot.margin = margin(t = 1, b = 1),
          plot.title = element_text(size = 20),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()),
  plot_substance_tmean + 
    coord_cartesian(ylim = c(ymin, ymax),
                    xlim = c(2, 34)) +
    scale_y_continuous(breaks = seq(ymin_scale, ymax_scale, by = 100)) +
    labs(title = 'Substance use') +
    theme(plot.margin = margin(t = 1, b = 1),
          plot.title = element_text(size = 20),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()),
  plot_renal_tmean + 
    coord_cartesian(ylim = c(ymin, ymax),
                    xlim = c(2, 34)) +
    scale_y_continuous(breaks = seq(ymin_scale, ymax_scale, by = 100)) +
    labs(title = 'Renal') +
    theme(plot.margin = margin(t = 1, b = 1),
          plot.title = element_text(size = 20)) ,
  plot_unintentinjury_tmean + 
    coord_cartesian(ylim = c(ymin, ymax),
                    xlim = c(2, 34)) +
    scale_y_continuous(breaks = seq(ymin_scale, ymax_scale, by = 100)) +
    labs(title = 'Unintentional injury') +
    theme(plot.margin = margin(t = 1, b = 1),
          plot.title = element_text(size = 20)),
  nrow = 4,
  ncol = 2,
  rel_heights = c(1, 1, 1, 1.1),
  align = 'hv',
  axis = 'tblr'
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'tmean', paste0('cum_lag', lag_num, '_allcauses_death.png')), 
       height = 12, width = 9 )


### HeatRisk ----

#### All on same figure ----
plot_cum_hr_together(
  lag_num = lag_num,
  mod = mod_unintentinjury_heat_risk,
  outcome = 'death',
  moa = 'rr',
  temp_var = 'heat_risk',
  group = c('Cardiovascular', 'Respiratory', 'Intentional injury', 'Unintentional injury', 'Stroke',
            'Substance use', 'Renal', 'Genitourinary'),
  estimates = list(estimates_cardiovascular_heat_risk, estimates_respiratory_heat_risk, 
                   estimates_intentinjury_heat_risk, estimates_unintentinjury_heat_risk, estimates_stroke_heat_risk,
                   estimates_substance_heat_risk, 
                   estimates_renal_heat_risk, estimates_genitourinary_heat_risk), subset_var = 'Cause',
  with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'heat_risk', paste0('cum_lag', lag_num, '_allcause_death_together.png')), 
       height = 8, width = 12 )

#### Using cowplot ----

# Manually set ymin and ymax for  gridded plot, so that y axes align
ymin <- 0
ymax <- 2

# Plot all causes together
# Plot all cause together
plot_grid(
  plot_cardiovascular_heat_risk + 
    coord_cartesian(ylim = c(ymin, ymax)) +
    scale_y_continuous(breaks = seq(ymin_scale, ymax_scale, by = 0.5)) +
    labs(title = 'Cardiovascular') +
    theme(plot.margin = margin(t = 5, b = 5),
          plot.title = element_text(size = 20),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()),
  plot_respiratory_heat_risk + 
    coord_cartesian(ylim = c(ymin, ymax)) +
    scale_y_continuous(breaks = seq(ymin_scale, ymax_scale, by = 0.5))  +
    labs(title = 'Respiratory') +
    theme(plot.margin = margin(t = 5, b = 5),
          plot.title = element_text(size = 20),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()),
  plot_intentinjury_heat_risk + 
    coord_cartesian(ylim = c(ymin, ymax)) +
    scale_y_continuous(breaks = seq(ymin_scale, ymax_scale, by = 0.5)) +
    labs(title = 'Intentional injury') +
    theme(plot.margin = margin(t = 5, b = 5),
          plot.title = element_text(size = 20),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) ,
  plot_unintentinjury_heat_risk + 
    coord_cartesian(ylim = c(ymin, ymax)) +
    scale_y_continuous(breaks = seq(ymin_scale, ymax_scale, by = 0.5)) +
    labs(title = 'Unintentional injury') +
    theme(plot.margin = margin(t = 5, b = 5),
          plot.title = element_text(size = 20),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) ,
  plot_stroke_heat_risk + 
    coord_cartesian(ylim = c(ymin, ymax)) +
    scale_y_continuous(breaks = seq(ymin_scale, ymax_scale, by = 0.5)) +
    labs(title = 'Stroke') +
    theme(plot.margin = margin(t = 5, b = 5),
          plot.title = element_text(size = 20),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) ,
  plot_substance_heat_risk + 
    coord_cartesian(ylim = c(ymin, ymax)) +
    scale_y_continuous(breaks = seq(ymin_scale, ymax_scale, by = 0.5)) +
    labs(title = 'Substance use') +
    theme(plot.margin = margin(t = 5, b = 5),
          plot.title = element_text(size = 20),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) ,
  plot_renal_heat_risk + 
    coord_cartesian(ylim = c(ymin, ymax)) +
    scale_y_continuous(breaks = seq(ymin_scale, ymax_scale, by = 0.5)) +
    labs(title = 'Renal') +
    theme(plot.margin = margin(t = 5, b = 5),
          plot.title = element_text(size = 20),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) ,
  plot_genitourinary_heat_risk + 
    coord_cartesian(ylim = c(ymin, ymax)) +
    scale_y_continuous(breaks = seq(ymin_scale, ymax_scale, by = 0.5)) +
    labs(title = 'Genitourinary') +
    theme(plot.margin = margin(t = 5, b = 5),
          plot.title = element_text(size = 20)) ,
  nrow = 8,
  align = 'hv',
  axis = 'tblr'
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'heat_risk', paste0('cum_lag', lag_num, '_allcause_death.png')), 
       height = 12, width = 4.5 )

################################################################################-
## 3: Clean up ----
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
   plot_cardiovascular_tmean, plot_cardiovascular_heat_risk,
   plot_respiratory_tmean, plot_respiratory_heat_risk,
   plot_stroke_tmean, plot_stroke_heat_risk,
   plot_intentinjury_tmean, plot_intentinjury_heat_risk,
   plot_unintentinjury_tmean, plot_unintentinjury_heat_risk,
   plot_substance_tmean, plot_substance_heat_risk,
   plot_genitourinary_tmean, plot_genitourinary_heat_risk,
   ymin, ymax, ymin_scale, ymax_scale
)
