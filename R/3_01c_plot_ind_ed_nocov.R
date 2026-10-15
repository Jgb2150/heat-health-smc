################################################################################-
# Plot: Individual lags 0-6 for mean temperature and HeatRisk, ED visits, no covid model ----
# Comparing HeatRisk and temperature for local heat surveillance: A time-stratified 
#  case-crossover study of emergency department visits in San Mateo County, 
#  California, 2017–2025
# Juliana GE Bartels
# Updated: 8/26/2026
################################################################################-

# This file plots the lagged percent change in ED visit rates for 0-6 days following 
# exposure to temperature and HeatRisk, no covid model.  

################################################################################-
## 0: Upload data ----
################################################################################-

# Generate suffix for file calling and writing, based on data source
data_source_name <- case_when(data_source == 'original' ~ '',
                              data_source == 'synth' ~ '_synth')

# Load estimates for no covid years (2020, 2021) sensitivity model - mean temperature
selected_constraints_ed <- read_csv(here::here('outputs', 'tables', paste0('selected_constraints_ed', data_source_name, '.csv')) ) %>%
  filter(sensitivity == 'nocov')

estimates_tmean <- read_csv(here::here('outputs', 'estimates', 'ed', 'ind', 'tmean',
                                 paste0('est_ind_ed_', 
                                        selected_constraints_ed[2,1], '_', selected_constraints_ed[2,2], '_', 
                                        selected_constraints_ed[2,3], '_', selected_constraints_ed[2,4], '_', 
                                        selected_constraints_ed[2,5], '_', selected_constraints_ed[2,6], '_', 
                                        selected_constraints_ed[2,7], '_', 'tmean', data_source_name,
                                        '.csv')))

## Load no covid years (2020, 2021) sensitivity model - mean temperature
mod_tmean <- readRDS(here::here('outputs', 'models', 'ed', 'tmean', 
                       paste0('ed_', selected_constraints_ed[2,1], '_', selected_constraints_ed[2,2], '_', 
                       selected_constraints_ed[2,3], '_', selected_constraints_ed[2,4], '_', 
                       selected_constraints_ed[2,5], '_', selected_constraints_ed[2,6], '_', 
                       selected_constraints_ed[2,7],'_', 'tmean', data_source_name,
                       '.RDS')))

# Load estimates for no covid years (2020, 2021) sensitivity model - HeatRisk
estimates_heat_risk <- read_csv(here::here('outputs', 'estimates', 'ed', 'ind', 'heat_risk',
                                       paste0('est_ind_ed_', 
                                              selected_constraints_ed[1,1], '_', selected_constraints_ed[1,2], '_', 
                                              selected_constraints_ed[1,3], '_', selected_constraints_ed[1,4], '_', 
                                              selected_constraints_ed[1,5], '_', selected_constraints_ed[1,6], '_', 
                                              selected_constraints_ed[1,7], '_', 'heat_risk', data_source_name,
                                              '.csv')))

# Load no covid years (2020, 2021) sensitivity model - HeatRisk
mod_heat_risk <- readRDS(here::here('outputs', 'models', 'ed', 'heat_risk', 
                                paste0('ed_', selected_constraints_ed[1,1], '_', selected_constraints_ed[1,2], '_', 
                                       selected_constraints_ed[1,3], '_', selected_constraints_ed[1,4], '_', 
                                       selected_constraints_ed[1,5], '_', selected_constraints_ed[1,6], '_', 
                                       selected_constraints_ed[1,7],'_', 'heat_risk', data_source_name,
                                       '.RDS')))

################################################################################-
## 1: Plot means of risk for temperatures compared to median, 6 days lag - mean temperature ----
################################################################################-

# Plot 6 days of lag relative risk of ed for the 95th percentile of temperature compared to the median observed temperature
per_95_plot <- plot_ind_tmean(lag_num = 6, 
                              mod = mod_tmean, 
                              outcome = 'ED visit',
                              moa = 'rr',
                              percentile = 'per_95',
                              temp_var = 'tmean', 
                              estimates = estimates_tmean, 
                              subset = 'residents ')

ggsave(filename = here::here('outputs', 'plots', 'ed', 'ind', 'tmean', paste0('ind_lag6_full_ed_per95_nocov', data_source_name, '.png')), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of ed for the 95th percentile of temperature compared to the median observed temperature
per_95_plot_pctchg <- plot_ind_tmean(lag_num = 6, 
                                     mod = mod_tmean, 
                                     outcome = 'ED visit',
                                     moa = 'pct_chg',
                                     percentile = 'per_95',
                                     temp_var = 'tmean', 
                                     estimates = estimates_tmean, 
                                     subset = 'residents ')

ggsave(filename = here::here('outputs', 'plots', 'ed', 'ind', 'tmean', paste0('ind_lag6_full_ed_per95_pct_chg_nocov', data_source_name, '.png')), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of ed for the 90th percentile of temperature compared to the median observed temperature
plot_ind_tmean(lag_num = 6, 
               mod = mod_tmean, 
               outcome = 'ED visit',
               moa = 'rr',
               percentile = 'per_90',
               temp_var = 'tmean', 
               estimates = estimates_tmean, 
               subset = 'residents ')

ggsave(filename = here::here('outputs', 'plots', 'ed', 'ind', 'tmean', paste0('ind_lag6_full_ed_per90_nocov', data_source_name, '.png')), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of ed for the 90th percentile of temperature compared to the median observed temperature
plot_ind_tmean(lag_num = 6, 
               mod = mod_tmean, 
               outcome = 'ED visit',
               moa = 'pct_chg',
               percentile = 'per_90',
               temp_var = 'tmean', 
               estimates = estimates_tmean, 
               subset = 'residents ')

ggsave(filename = here::here('outputs', 'plots', 'ed', 'ind', 'tmean', paste0('ind_lag6_full_ed_per90_pctchg_nocov', data_source_name, '.png')), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of ed for the 10th percentile of temperature compared to the median observed temperature
plot_ind_tmean(lag_num = 6, 
               mod = mod_tmean, 
               outcome = 'ED visit',
               moa = 'rr',
               percentile = 'per_10',
               temp_var = 'tmean', 
               estimates = estimates_tmean, 
               subset = 'residents ')

ggsave(filename = here::here('outputs', 'plots', 'ed', 'ind', 'tmean', paste0('ind_lag6_full_ed_per10_nocov', data_source_name, '.png')), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of ed for the 10th percentile of temperature compared to the median observed temperature
plot_ind_tmean(lag_num = 6, 
               mod = mod_tmean, 
               outcome = 'ED visit',
               moa = 'pct_chg',
               percentile = 'per_10',
               temp_var = 'tmean', 
               estimates = estimates_tmean, 
               subset = 'residents ')

ggsave(filename = here::here('outputs', 'plots', 'ed', 'ind', 'tmean', paste0('ind_lag6_full_ed_per10_pct_chg_nocov', data_source_name, '.png')), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of ed for the 5th percentile of temperature compared to the median observed temperature
per_05_plot <- plot_ind_tmean(lag_num = 6, 
                              mod = mod_tmean, 
                              outcome = 'ED visit',
                              moa = 'rr',
                              percentile = 'per_05',
                              temp_var = 'tmean', 
                              estimates = estimates_tmean, 
                              subset = 'residents ')

ggsave(filename = here::here('outputs', 'plots', 'ed', 'ind', 'tmean', paste0('ind_lag6_full_ed_per5_nocov', data_source_name, '.png')), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of ed for the 5th percentile of temperature compared to the median observed temperature
per_05_plot_pctchg <- plot_ind_tmean(lag_num = 6, 
                                     mod = mod_tmean, 
                                     outcome = 'ED visit',
                                     moa = 'pct_chg',
                                     percentile = 'per_05',
                                     temp_var = 'tmean', 
                                     estimates = estimates_tmean, 
                                     subset = 'residents ')

ggsave(filename = here::here('outputs', 'plots', 'ed', 'ind', 'tmean', paste0('ind_lag6_full_ed_per5_pct_chg_nocov', data_source_name, '.png')), 
       height = 8, width = 12 )

# Plot 5th and 95th percentile together

ymin <- -4
ymax <- 3

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

ggsave(filename = here::here('outputs', 'plots', 'ed', 'ind', 'tmean', paste0('ind_lag6_full_ed_per5per95_pctchg_nocov', data_source_name, '.png')), 
       height = 8, width = 12 )

################################################################################-
## 2: Plot means of risk for HR levels compared to HR 0, 6 days lag - HeatRisk ----
################################################################################-

# Plot 6 days of lag relative risk of ed for HR level 3 compared to HR 0
plot_hr3 <- plot_ind_hr(lag_num = 6, 
                        mod = mod_heat_risk, 
                        outcome = 'ED visit',
                        moa = 'rr',
                        hr_level = '3',
                        estimates = estimates_heat_risk, 
                        subset = 'residents ')

ggsave(filename = here::here('outputs', 'plots', 'ed', 'ind', 'heat_risk', paste0('ind_lag6_full_ed_hr3_nocov', data_source_name, '.png')), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of ed for HR level 3 compared to HR 0
plot_hr3_pctchg <- plot_ind_hr(lag_num = 6, 
                               mod = mod_heat_risk, 
                               outcome = 'ED visit',
                               moa = 'pct_chg',
                               hr_level = '3',
                               estimates = estimates_heat_risk, 
                               subset = 'residents ')

ggsave(filename = here::here('outputs', 'plots', 'ed', 'ind', 'heat_risk', paste0('ind_lag6_full_ed_hr3_pct_chg_nocov', data_source_name, '.png')), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of ed for HR level 3 compared to HR 0
plot_hr2 <- plot_ind_hr(lag_num = 6, 
                        mod = mod_heat_risk, 
                        outcome = 'ED visit',
                        moa = 'rr',
                        hr_level = '2',
                        estimates = estimates_heat_risk, 
                        subset = 'residents ')

ggsave(filename = here::here('outputs', 'plots', 'ed', 'ind', 'heat_risk', paste0('ind_lag6_full_ed_hr2_nocov', data_source_name, '.png')), 
       height = 8, width = 12 )

# Plot 6 days of lag relative risk of ed for HR level 3 compared to HR 0
plot_hr2_pctchg <- plot_ind_hr(lag_num = 6, 
                               mod = mod_heat_risk, 
                               outcome = 'ED visit',
                               moa = 'pct_chg',
                               hr_level = '2',
                               estimates = estimates_heat_risk, 
                               subset = 'residents ')

ggsave(filename = here::here('outputs', 'plots', 'ed', 'ind', 'heat_risk', paste0('ind_lag6_full_ed_hr2_pct_chg_nocov', data_source_name, '.png')), 
       height = 8, width = 12 )

# Plot relative risk for HR 3 and 2 together

## Manually set ylims
ymin <- -3
ymax <- 8

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

ggsave(filename = here::here('outputs', 'plots', 'ed', 'ind', 'heat_risk', paste0('ind_lag6_full_ed_hr2and3_pct_chg_nocov', data_source_name, '.png')), 
       height = 8, width = 12 )

################################################################################-
## 2: Clean up ----
################################################################################-

rm(data_source_name,
   selected_constraints_ed, 
   estimates_tmean, estimates_heat_risk,
   mod_tmean, mod_heat_risk,
   per_05_plot, per_05_plot_pctchg,
   per_95_plot, per_95_plot_pctchg,
   plot_hr3, plot_hr2,
   plot_hr3_pctchg, plot_hr2_pctchg)
