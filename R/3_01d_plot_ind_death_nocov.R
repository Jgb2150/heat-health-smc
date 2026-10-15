################################################################################-
# Plot: Individual lags 0-6 for mean temperature and HeatRisk, deaths, no covid model ----
# Comparing HeatRisk and temperature for local heat surveillance: A time-stratified 
#  case-crossover study of emergency department visits in San Mateo County, 
#  California, 2017–2025 
# Juliana GE Bartels
# Updated: 8/26/2026
################################################################################-

# This file plots the lagged percent change in death rates for 0-6 days following 
# exposure to temperature and HeatRisk, for no covid model.  

################################################################################-
## 0: Upload data ----
################################################################################-

# Generate suffix for file calling and writing, based on data source
data_source_name <- case_when(data_source == 'original' ~ '',
                              data_source == 'synth' ~ '_synth')

# Load estimates for no covid years (2020, 2021) sensitivity model - mean temperature
selected_constraints_death <- read_csv(here::here('outputs', 'tables', paste0('selected_constraints_death', data_source_name, '.csv')) ) %>%
  filter(sensitivity == 'nocov')

estimates_tmean <- read_csv(here::here('outputs', 'estimates', 'death', 'ind', 'tmean',
                                 paste0('est_ind_death_', 
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
estimates_heat_risk <- read_csv(here::here('outputs', 'estimates', 'death', 'ind', 'heat_risk',
                                       paste0('est_ind_death_', 
                                              selected_constraints_death[1,1], '_', selected_constraints_death[1,2], '_', 
                                              selected_constraints_death[1,3], '_', selected_constraints_death[1,4], '_', 
                                              selected_constraints_death[1,5], '_', selected_constraints_death[1,6], '_', 
                                              selected_constraints_death[1,7], '_', 'heat_risk', data_source_name,
                                              '.csv')))

## Load no covid years (2020, 2021) sensitivity model - HeatRisk
mod_heat_risk <- readRDS(here::here('outputs', 'models', 'death', 'heat_risk', 
                                paste0('death_', selected_constraints_death[1,1], '_', selected_constraints_death[1,2], '_', 
                                       selected_constraints_death[1,3], '_', selected_constraints_death[1,4], '_', 
                                       selected_constraints_death[1,5], '_', selected_constraints_death[1,6], '_', 
                                       selected_constraints_death[1,7],'_', 'heat_risk', data_source_name,
                                       '.RDS')))

################################################################################-
## 1: Plot means of risk for temperatures compared to median, 6 days lag - mean temperature ----
################################################################################-

# Plot 6 days of lag relative risk of death for the 95th percentile of temperature compared to the median observed temperature
per_95_plot <- plot_ind_tmean(lag_num = 6, 
                              mod = mod_tmean, 
                              outcome = 'death',
                              moa = 'rr',
                              percentile = 'per_95',
                              temp_var = 'tmean', 
                              estimates = estimates_tmean, 
                              subset = 'residents ')

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'tmean', paste0('ind_lag6_full_death_per95_nocov', data_source_name, '.png')), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of death for the 95th percentile of temperature compared to the median observed temperature
per_95_plot_pctchg <- plot_ind_tmean(lag_num = 6, 
                                     mod = mod_tmean, 
                                     outcome = 'death',
                                     moa = 'pct_chg',
                                     percentile = 'per_95',
                                     temp_var = 'tmean', 
                                     estimates = estimates_tmean, 
                                     subset = 'residents ')

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'tmean', paste0('ind_lag6_full_death_per95_pct_chg_nocov', data_source_name, '.png')), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of death for the 90th percentile of temperature compared to the median observed temperature
plot_ind_tmean(lag_num = 6, 
               mod = mod_tmean, 
               outcome = 'death',
               moa = 'rr',
               percentile = 'per_90',
               temp_var = 'tmean', 
               estimates = estimates_tmean, 
               subset = 'residents ')

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'tmean', paste0('ind_lag6_full_death_per90_nocov', data_source_name, '.png')), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of death for the 90th percentile of temperature compared to the median observed temperature
plot_ind_tmean(lag_num = 6, 
               mod = mod_tmean, 
               outcome = 'death',
               moa = 'pct_chg',
               percentile = 'per_90',
               temp_var = 'tmean', 
               estimates = estimates_tmean, 
               subset = 'residents ')

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'tmean', paste0('ind_lag6_full_death_per90_pctchg_nocov', data_source_name, '.png')), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of death for the 10th percentile of temperature compared to the median observed temperature
plot_ind_tmean(lag_num = 6, 
               mod = mod_tmean, 
               outcome = 'death',
               moa = 'rr',
               percentile = 'per_10',
               temp_var = 'tmean', 
               estimates = estimates_tmean, 
               subset = 'residents ')

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'tmean', paste0('ind_lag6_full_death_per10_nocov', data_source_name, '.png')), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of death for the 10th percentile of temperature compared to the median observed temperature
plot_ind_tmean(lag_num = 6, 
               mod = mod_tmean, 
               outcome = 'death',
               moa = 'pct_chg',
               percentile = 'per_10',
               temp_var = 'tmean', 
               estimates = estimates_tmean, 
               subset = 'residents ')

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'tmean', paste0('ind_lag6_full_death_per10_pct_chg_nocov.', data_source_name, 'png')), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of death for the 5th percentile of temperature compared to the median observed temperature
per_05_plot <- plot_ind_tmean(lag_num = 6, 
                              mod = mod_tmean, 
                              outcome = 'death',
                              moa = 'rr',
                              percentile = 'per_05',
                              temp_var = 'tmean', 
                              estimates = estimates_tmean, 
                              subset = 'residents ')

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'tmean', paste0('ind_lag6_full_death_per5_nocov', data_source_name, '.png')), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of death for the 5th percentile of temperature compared to the median observed temperature
per_05_plot_pctchg <- plot_ind_tmean(lag_num = 6, 
                                     mod = mod_tmean, 
                                     outcome = 'death',
                                     moa = 'pct_chg',
                                     percentile = 'per_05',
                                     temp_var = 'tmean', 
                                     estimates = estimates_tmean, 
                                     subset = 'residents ')

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'tmean', paste0('ind_lag6_full_death_per5_pct_chg_nocov', data_source_name, '.png')), 
       height = 8, width = 12 )

# Plot 5th and 95th percentile together

ymin <- -10
ymax <- 10

plot_grid(
  per_05_plot_pctchg +
    coord_cartesian(ylim = c(ymin, ymax)) +
    labs(title = 'A | 5th percentile') +
    theme(plot.margin = margin(t = 20, b = 10),
          plot.title = element_text(size = 20),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()),
  per_95_plot_pctchg + 
    coord_cartesian(ylim = c(ymin, ymax)) +
    labs(title = 'B | 95th percentile') +
    theme(plot.margin = margin(t = 20, b = 10),
          plot.title = element_text(size = 20)),
  nrow = 2,
  align = 'v'
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'tmean', paste0('ind_lag6_full_death_per5per95_pctchg_nocov', data_source_name, '.png')), 
       height = 8, width = 12 )

################################################################################-
## 2: Plot means of risk for HR levels compared to HR 0, 6 days lag - HeatRisk ----
################################################################################-

# Plot 6 days of lag relative risk of death for HR level 3 compared to HR 0
plot_hr3 <- plot_ind_hr(lag_num = 6, 
                        mod = mod_heat_risk, 
                        outcome = 'death',
                        moa = 'rr',
                        hr_level = '3',
                        estimates = estimates_heat_risk, 
                        subset = 'residents ')

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'heat_risk', paste0('ind_lag6_full_death_hr3_nocov', data_source_name, '.png')), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of death for HR level 3 compared to HR 0
plot_hr3_pctchg <- plot_ind_hr(lag_num = 6, 
                               mod = mod_heat_risk, 
                               outcome = 'death',
                               moa = 'pct_chg',
                               hr_level = '3',
                               estimates = estimates_heat_risk, 
                               subset = 'residents ')

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'heat_risk', paste0('ind_lag6_full_death_hr3_pct_chg_nocov', data_source_name, '.png')), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of death for HR level 3 compared to HR 0
plot_hr2 <- plot_ind_hr(lag_num = 6, 
                        mod = mod_heat_risk, 
                        outcome = 'death',
                        moa = 'rr',
                        hr_level = '2',
                        estimates = estimates_heat_risk, 
                        subset = 'residents ')

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'heat_risk', paste0('ind_lag6_full_death_hr2_nocov', data_source_name, '.png')), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of death for HR level 3 compared to HR 0
plot_hr2_pctchg <- plot_ind_hr(lag_num = 6, 
                               mod = mod_heat_risk, 
                               outcome = 'death',
                               moa = 'pct_chg',
                               hr_level = '2',
                               estimates = estimates_heat_risk, 
                               subset = 'residents ')

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'heat_risk', paste0('ind_lag6_full_death_hr2_pct_chg_nocov', data_source_name, '.png')), 
       height = 8, width = 12 )

# Plot relative risk for HR 3 and 2 together

## Manually set ylims
ymin <- -10
ymax <- 24

plot_grid(
  plot_hr2_pctchg + 
    coord_cartesian(ylim = c(ymin, ymax)) +
    labs(title = 'A | Moderate HeatRisk') +
    theme(plot.margin = margin(t = 20, b = 10),
          plot.title = element_text(size = 20),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()),
  plot_hr3_pctchg + 
    coord_cartesian(ylim = c(ymin, ymax)) +
    labs(title = 'B | Major/extreme HeatRisk') +
    theme(plot.margin = margin(t = 20, b = 10),
          plot.title = element_text(size = 20)),
  hjust = -0.17,
  nrow = 2,
  align = 'hv'
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'ind', 'heat_risk', paste0('ind_lag6_full_death_hr2and3_pct_chg_nocov', data_source_name, '.png')), 
       height = 8, width = 12 )

################################################################################-
## 2: Clean up ----
################################################################################-

rm(data_source_name,
   selected_constraints_death, 
   estimates_tmean, estimates_heat_risk,
   mod_tmean, mod_heat_risk,
   per_05_plot, per_05_plot_pctchg,
   per_95_plot, per_95_plot_pctchg,
   plot_hr3, plot_hr2,
   plot_hr3_pctchg, plot_hr2_pctchg)
