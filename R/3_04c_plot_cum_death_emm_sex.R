################################################################################-
# Plot: Cumulative of lags 0-6 for mean temperatures and HeatRisk, deaths, sex ----
# Comparing HeatRisk and temperature for local heat surveillance: A time-stratified 
#  case-crossover study of emergency department visits in San Mateo County, 
#  California, 2017–2025
# Juliana GE Bartels
# Updated: 6/22/2026
################################################################################-

# This file plots cumulative percent change in death rate for lag 0-6 days,
# by sex. 

################################################################################-
## 0: Upload data ----
################################################################################-

# Generate suffix for file calling and writing, based on data source
data_source_name <- case_when(data_source == 'original' ~ '',
                              data_source == 'synth' ~ '_synth')

# Load estimates and models for age EMMs
selected_constraints_death <- read_csv(here::here('outputs', 'tables', paste0('selected_constraints_death', data_source_name, '.csv')) ) %>%
  filter(sensitivity == 'main')

sex_grid <- expand_grid(
  subset = c('f', 'm'),
  temp_var = c('tmean', 'heat_risk')) %>%
  mutate(estimate_name = paste0('estimates_', subset, '_', temp_var),
         mod_name = paste0('mod_', subset, '_', temp_var))

load_estimates(selected_constraints_death, 'death', 'sex', sex_grid, 'cum', data_source_name)

# Set number of lags for cumulative plots
lag_num <- 6

################################################################################-
## 1a: Plot cumulative risk, (female) ----
################################################################################-

### Mean temperature ----

# Plot cumulative relative risk of death for all temperatures compared to the median observed temperature
plot_cum_tmean(lag_num = lag_num,
               mod = mod_f_tmean, 
               outcome = 'death',
               moa = 'rr',
               temp_var = 'tmean', 
               estimates = estimates_f_tmean, 
               subset = 'female residents ',
               with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'tmean', paste0('cum_lag', lag_num, '_f_death_rr', data_source_name, '.png')), 
       height = 8, width = 12 )

# Plot cumulative percent change of death for all temperatures compared to the median observed temperature
plot_cum_tmean(lag_num = lag_num,
               mod = mod_f_tmean, 
               outcome = 'death',
               moa = 'pct_chg',
               temp_var = 'tmean', 
               estimates = estimates_f_tmean, 
               subset = 'female residents ',
               with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'tmean', paste0('cum_lag', lag_num, '_f_death_pct_chg', data_source_name, '.png')), 
       height = 8, width = 12 )

## Create plot in environment without histogram for temperature
plot_f_tmean <- plot_cum_tmean(lag_num = lag_num,
                               mod = mod_f_tmean, 
                               outcome = 'death',
                               moa = 'pct_chg',
                               temp_var = 'tmean', 
                               estimates = estimates_f_tmean, 
                               subset = 'female residents ',
                               with_hist = FALSE
)

### HeatRisk ----

# Plot cumulative relative risk of death for all temperatures compared to the median observed temperature
plot_cum_hr(lag_num = lag_num,
                   mod = mod_f_heat_risk, 
                   outcome = 'death',
                   moa = 'rr',
                   temp_var = 'heat_risk', 
                   estimates = estimates_f_heat_risk,
                   subset = 'female residents ',
                   with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'heat_risk', paste0('cum_lag', lag_num, '_f_death_rr', data_source_name, '.png')), 
       height = 8, width = 12 )

# Plot cumulative percent change of death for all temperatures compared to the median observed temperature
plot_cum_hr(lag_num = lag_num,
                   mod = mod_f_heat_risk, 
                   outcome = 'death',
                   moa = 'pct_chg',
                   temp_var = 'heat_risk', 
                   estimates = estimates_f_heat_risk,
                   subset = 'female residents ',
                   with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'heat_risk', paste0('cum_lag', lag_num, '_f_death_pct_chg', data_source_name, '.png')), 
       height = 8, width = 12 )

## Create plot in environment without histogram for temperature
plot_f_heat_risk <- plot_cum_hr(lag_num = lag_num,
                                       mod = mod_f_heat_risk, 
                                       outcome = 'death',
                                       moa = 'pct_chg',
                                       temp_var = 'heat_risk', 
                                       estimates = estimates_f_heat_risk,
                                       subset = 'female residents ',
                                       with_hist = FALSE
)

################################################################################-
## 1b: Plot cumulative risk, (male) ----
################################################################################-
### Mean temperature ----

# Plot cumulative relative risk of death for all temperatures compared to the median observed temperature
plot_cum_tmean(lag_num = lag_num,
               mod = mod_m_tmean, 
               outcome = 'death',
               moa = 'rr',
               temp_var = 'tmean', 
               estimates = estimates_m_tmean, 
               subset = 'male residents ',
               with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'tmean', paste0('cum_lag', lag_num, '_m_death_rr', data_source_name, '.png')), 
       height = 8, width = 12 )

# Plot cumulative percent change of death for all temperatures compared to the median observed temperature
plot_cum_tmean(lag_num = lag_num,
               mod = mod_m_tmean, 
               outcome = 'death',
               moa = 'pct_chg',
               temp_var = 'tmean', 
               estimates = estimates_m_tmean, 
               subset = 'male residents ',
               with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'tmean', paste0('cum_lag', lag_num, '_m_death_pct_chg', data_source_name, '.png')), 
       height = 8, width = 12 )

## Create plot in environment without histogram for temperature
plot_m_tmean <- plot_cum_tmean(lag_num = lag_num,
                               mod = mod_m_tmean, 
                               outcome = 'death',
                               moa = 'pct_chg',
                               temp_var = 'tmean', 
                               estimates = estimates_m_tmean, 
                               subset = 'male residents ',
                               with_hist = FALSE
)

### HeatRisk ----

# Plot cumulative relative risk of death for all temperatures compared to the median observed temperature
plot_cum_hr(lag_num = lag_num,
                   mod = mod_m_heat_risk, 
                   outcome = 'death',
                   moa = 'rr',
                   temp_var = 'heat_risk', 
                   estimates = estimates_m_heat_risk,
                   subset = 'male residents ',
                   with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'heat_risk', paste0('cum_lag', lag_num, '_m_death_rr', data_source_name, '.png')), 
       height = 8, width = 12 )

# Plot cumulative percent change of death for all temperatures compared to the median observed temperature
plot_cum_hr(lag_num = lag_num,
                   mod = mod_m_heat_risk, 
                   outcome = 'death',
                   moa = 'pct_chg',
                   temp_var = 'heat_risk', 
                   estimates = estimates_m_heat_risk,
                   subset = 'male residents ',
                   with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'heat_risk', paste0('cum_lag', lag_num, '_m_death_pct_chg', data_source_name, '.png')), 
       height = 8, width = 12 )

## Create plot in environment without histogram for temperature
plot_m_heat_risk <- plot_cum_hr(lag_num = lag_num,
                                       mod = mod_m_heat_risk, 
                                       outcome = 'death',
                                       moa = 'pct_chg',
                                       temp_var = 'heat_risk', 
                                       estimates = estimates_m_heat_risk,
                                       subset = 'male residents ',
                                       with_hist = FALSE
)

################################################################################-
## 2: Plot all sex cumulative risk, together ----
################################################################################-

### Mean temperature ----

#### All on same figure ----
plot_cum_tmean_together(
  lag_num = lag_num,
  mod = mod_f_tmean,
  outcome = 'death',
  moa = 'pct_chg',
  temp_var = 'tmean',
  group = c('Female', 'Male'),
  estimates = list(estimates_f_tmean, estimates_m_tmean),
  subset_var = 'Sex',
  with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'tmean', paste0('cum_lag', lag_num, '_allsex_death_together', data_source_name, '.png')), 
       height = 8, width = 12 )

#### Using cowplot ----

# Manually set ymin and ymax for  gridded plot, so that y axes align
ymin <- 0
ymax <- 50
ymin_scale <- 0
ymax_scale <- 50

# Plot all sexes together
plot_grid(
  plot_f_tmean + 
    coord_cartesian(ylim = c(ymin, ymax),
                    xlim = c(2, 34)) +
    scale_y_continuous(breaks = seq(ymin_scale, ymax_scale, by = 25)) +
    labs(title = 'Female') +
    theme(plot.margin = margin(t = 20, b = 10),
          plot.title = element_text(size = 20),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()),
  plot_m_tmean + 
    coord_cartesian(ylim = c(ymin, ymax),
                    xlim = c(2, 34)) +
    scale_y_continuous(breaks = seq(ymin_scale, ymax_scale, by = 25)) +
    labs(title = 'Male') +
    theme(plot.margin = margin(t = 20, b = 10),
          plot.title = element_text(size = 20)),
  NULL,
  NULL,
  nrow = 4,
  rel_heights = c(1, 1.2, 1, 1),
  align = 'v'
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'tmean', paste0('cum_lag', lag_num, '_allsex_death', data_source_name, '.png')), 
       height = 12, width = 4.5 )


### HeatRisk ----

#### All on same figure ----
plot_cum_hr_together(
  lag_num = lag_num,
  mod = mod_f_heat_risk,
  outcome = 'death',
  moa = 'pct_chg',
  temp_var = 'heat_risk',
  group = c('Female', 'Male'),
  estimates = list(estimates_f_heat_risk, estimates_m_heat_risk),
  subset_var = 'Sex',
  with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'heat_risk', paste0('cum_lag', lag_num, '_allsex_death_together', data_source_name, '.png')), 
       height = 8, width = 12 )

#### Using cowplot ----

# Manually set ymin and ymax for  gridded plot, so that y axes align
ymin <- 0
ymax <- 50

# Plot all sexes together
plot_grid(
  plot_f_heat_risk + 
    coord_cartesian(ylim = c(ymin, ymax)) +
    labs(title = 'Female') +
    theme(plot.margin = margin(t = 20, b = 10),
          plot.title = element_text(size = 20),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()),
  plot_m_heat_risk + 
    coord_cartesian(ylim = c(ymin, ymax)) +
    labs(title = 'Male') +
    theme(plot.margin = margin(t = 20, b = 10),
          plot.title = element_text(size = 20)),
  labels = c('Female', '  Male'),
  hjust = -1.5,
  nrow = 2,
  align = 'v'
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'heat_risk', paste0('cum_lag', lag_num, '_allsex_death', data_source_name, '.png')), 
       height = 8, width = 12 )

################################################################################-
## 3: Clean up ----
################################################################################-

rm(data_source_name,
   selected_constraints_death, sex_grid,
   estimates_f_tmean, estimates_f_heat_risk,
   estimates_m_tmean, estimates_m_heat_risk,
   mod_f_tmean, mod_f_heat_risk,
   mod_m_tmean, mod_m_heat_risk,
   plot_f_tmean, plot_f_heat_risk,
   plot_m_tmean, plot_m_heat_risk,
   ymin, ymax
)
