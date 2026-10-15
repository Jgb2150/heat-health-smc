################################################################################-
# Plot: Cumulative of lags 0-6 for mean temperatures and HeatRisk, deaths, region ----
# Comparing HeatRisk and temperature for local heat surveillance: A time-stratified 
#  case-crossover study of emergency department visits in San Mateo County, 
#  California, 2017–2025
# Juliana GE Bartels
# Updated: 6/22/2026
################################################################################-

# This file plots cumulative percent change in death rate for lag 0-6 days,
# by region. 

################################################################################-
## 0: Upload data ----
################################################################################-

# Generate suffix for file calling and writing, based on data source
data_source_name <- case_when(data_source == 'original' ~ '',
                              data_source == 'synth' ~ '_synth')

# Load estimates and models for region EMMs
selected_constraints_death <- read_csv(here::here('outputs', 'tables', 'selected_constraints_death.csv') ) %>%
  filter(sensitivity == 'main')

region_grid <- expand_grid(
  subset = c('coastside', 'mid', 'north', 'south'),
  temp_var = c('tmean', 'heat_risk')) %>%
  mutate(estimate_name = paste0('estimates_', subset, '_', temp_var),
         mod_name = paste0('mod_', subset, '_', temp_var))

load_estimates(selected_constraints_death, 'death', 'region', region_grid, 'cum', data_source_name)

# Set number of lags for cumulative plots
lag_num <- 6

################################################################################-
## 1a: Plot cumulative risk, (Coastside) ----
################################################################################-

### Mean temperature ----

# Plot cumulative relative risk of death for all temperatures compared to the median observed temperature
plot_cum_tmean(lag_num = lag_num,
               mod = mod_coastside_tmean, 
               outcome = 'death',
               moa = 'rr',
               temp_var = 'tmean', 
               estimates = estimates_coastside_tmean, 
               subset = 'residents in the Coastside region ',
               with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'tmean', paste0('cum_lag', lag_num, '_coastside_death_rr.png')), 
       height = 8, width = 12 )

# Plot cumulative percent change of death for all temperatures compared to the median observed temperature
plot_cum_tmean(lag_num = lag_num,
               mod = mod_coastside_tmean, 
               outcome = 'death',
               moa = 'pct_chg',
               temp_var = 'tmean', 
               estimates = estimates_coastside_tmean, 
               subset = 'residents in the Coastside region ',
               with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'tmean', paste0('cum_lag', lag_num, '_coastside_death_pct_chg.png')), 
       height = 8, width = 12 )

## Create plot in environment without histogram for temperature
plot_coastside_tmean <- plot_cum_tmean(lag_num = lag_num,
                                       mod = mod_coastside_tmean, 
                                       outcome = 'death',
                                       moa = 'pct_chg',
                                       temp_var = 'tmean', 
                                       estimates = estimates_coastside_tmean, 
                                       subset = 'residents in the Coastside region ',
                                       with_hist = FALSE
)

### HeatRisk ----

# Plot cumulative relative risk of death for all temperatures compared to the median observed temperature
plot_cum_hr(lag_num = lag_num,
                   mod = mod_coastside_heat_risk, 
                   outcome = 'death',
                   moa = 'rr',
                   temp_var = 'heat_risk', 
                   estimates = estimates_coastside_heat_risk,
                   subset = 'residents in the Coastside region ',
                   with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'heat_risk', paste0('cum_lag', lag_num, '_coastside_death_rr.png')), 
       height = 8, width = 12 )

# Plot cumulative percent change of death for all temperatures compared to the median observed temperature
plot_cum_hr(lag_num = lag_num,
                   mod = mod_coastside_heat_risk, 
                   outcome = 'death',
                   moa = 'pct_chg',
                   temp_var = 'heat_risk', 
                   estimates = estimates_coastside_heat_risk,
                   subset = 'residents in the Coastside region ',
                   with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'heat_risk', paste0('cum_lag', lag_num, '_coastside_death_pct_chg.png')), 
       height = 8, width = 12 )

## Create plot in environment without histogram for temperature
plot_coastside_heat_risk <- plot_cum_hr(lag_num = lag_num,
                                               mod = mod_coastside_heat_risk, 
                                               outcome = 'death',
                                               moa = 'pct_chg',
                                               temp_var = 'heat_risk', 
                                               estimates = estimates_coastside_heat_risk,
                                               subset = 'residents in the Coastside region ',
                                               with_hist = FALSE
)

################################################################################-
## 1b: Plot cumulative risk, (Mid-county) ----
################################################################################-

### Mean temperature ----

# Plot cumulative relative risk of death for all temperatures compared to the median observed temperature
plot_cum_tmean(lag_num = lag_num,
               mod = mod_mid_tmean, 
               outcome = 'death',
               moa = 'rr',
               temp_var = 'tmean', 
               estimates = estimates_mid_tmean, 
               subset = 'residents in the Mid-county region ',
               with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'tmean', paste0('cum_lag', lag_num, '_mid_death_rr.png')), 
       height = 8, width = 12 )

# Plot cumulative percent change of death for all temperatures compared to the median observed temperature
plot_cum_tmean(lag_num = lag_num,
               mod = mod_mid_tmean, 
               outcome = 'death',
               moa = 'pct_chg',
               temp_var = 'tmean', 
               estimates = estimates_mid_tmean, 
               subset = 'residents in the Mid-county region ',
               with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'tmean', paste0('cum_lag', lag_num, '_mid_death_pct_chg.png')), 
       height = 8, width = 12 )

## Create plot in environment without histogram for temperature
plot_mid_tmean <- plot_cum_tmean(lag_num = lag_num,
                                 mod = mod_mid_tmean, 
                                 outcome = 'death',
                                 moa = 'pct_chg',
                                 temp_var = 'tmean', 
                                 estimates = estimates_mid_tmean, 
                                 subset = 'residents in the Mid-county region ',
                                 with_hist = FALSE
)

### HeatRisk ----

# Plot cumulative relative risk of death for all temperatures compared to the median observed temperature
plot_cum_hr(lag_num = lag_num,
                   mod = mod_mid_heat_risk, 
                   outcome = 'death',
                   moa = 'rr',
                   temp_var = 'heat_risk', 
                   estimates = estimates_mid_heat_risk,
                   subset = 'residents in the Mid-county region ',
                   with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'heat_risk', paste0('cum_lag', lag_num, '_mid_death_rr.png')), 
       height = 8, width = 12 )

# Plot cumulative percent change of death for all temperatures compared to the median observed temperature
plot_cum_hr(lag_num = lag_num,
                   mod = mod_mid_heat_risk, 
                   outcome = 'death',
                   moa = 'pct_chg',
                   temp_var = 'heat_risk', 
                   estimates = estimates_mid_heat_risk,
                   subset = 'residents in the Mid-county region ',
                   with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'heat_risk', paste0('cum_lag', lag_num, '_mid_death_pct_chg.png')), 
       height = 8, width = 12 )

## Create plot in environment without histogram for temperature
plot_mid_heat_risk <- plot_cum_hr(lag_num = lag_num,
                                         mod = mod_mid_heat_risk, 
                                         outcome = 'death',
                                         moa = 'pct_chg',
                                         temp_var = 'heat_risk', 
                                         estimates = estimates_mid_heat_risk,
                                         subset = 'residents in the Mid-county region ',
                                         with_hist = FALSE
)

################################################################################-
## 1c: Plot cumulative risk, (North county) ----
################################################################################-

### Mean temperature ----

# Plot cumulative relative risk of death for all temperatures compared to the median observed temperature
plot_cum_tmean(lag_num = lag_num,
               mod = mod_north_tmean, 
               outcome = 'death',
               moa = 'rr',
               temp_var = 'tmean', 
               estimates = estimates_north_tmean, 
               subset = 'residents in the North county region ',
               with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'tmean', paste0('cum_lag', lag_num, '_north_death_rr.png')), 
       height = 8, width = 12 )

# Plot cumulative percent change of death for all temperatures compared to the median observed temperature
plot_cum_tmean(lag_num = lag_num,
               mod = mod_north_tmean, 
               outcome = 'death',
               moa = 'pct_chg',
               temp_var = 'tmean', 
               estimates = estimates_north_tmean, 
               subset = 'residents in the North county region ',
               with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'tmean', paste0('cum_lag', lag_num, '_north_death_pct_chg.png')), 
       height = 8, width = 12 )

## Create plot in environment without histogram for temperature
plot_north_tmean <- plot_cum_tmean(lag_num = lag_num,
                                   mod = mod_north_tmean, 
                                   outcome = 'death',
                                   moa = 'pct_chg',
                                   temp_var = 'tmean', 
                                   estimates = estimates_north_tmean, 
                                   subset = 'residents in the North county region ',
                                   with_hist = FALSE
)

### HeatRisk ----

# Plot cumulative relative risk of death for all temperatures compared to the median observed temperature
plot_cum_hr(lag_num = lag_num,
                   mod = mod_north_heat_risk, 
                   outcome = 'death',
                   moa = 'rr',
                   temp_var = 'heat_risk', 
                   estimates = estimates_north_heat_risk,
                   subset = 'residents in the North county region ',
                   with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'heat_risk', paste0('cum_lag', lag_num, '_north_death_rr.png')), 
       height = 8, width = 12 )

# Plot cumulative percent change of death for all temperatures compared to the median observed temperature
plot_cum_hr(lag_num = lag_num,
                   mod = mod_north_heat_risk, 
                   outcome = 'death',
                   moa = 'pct_chg',
                   temp_var = 'heat_risk', 
                   estimates = estimates_north_heat_risk,
                   subset = 'residents in the North county region ',
                   with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'heat_risk', paste0('cum_lag', lag_num, '_north_death_pct_chg.png')), 
       height = 8, width = 12 )

## Create plot in environment without histogram for temperature
plot_north_heat_risk <- plot_cum_hr(lag_num = lag_num,
                                           mod = mod_north_heat_risk, 
                                           outcome = 'death',
                                           moa = 'pct_chg',
                                           temp_var = 'heat_risk', 
                                           estimates = estimates_north_heat_risk,
                                           subset = 'residents in the North county region ',
                                           with_hist = FALSE
)

################################################################################-
## 1d: Plot cumulative risk, (South county) ----
################################################################################-

### Mean temperature ----

# Plot cumulative relative risk of death for all temperatures compared to the median observed temperature
plot_cum_tmean(lag_num = lag_num,
               mod = mod_south_tmean, 
               outcome = 'death',
               moa = 'rr',
               temp_var = 'tmean', 
               estimates = estimates_south_tmean, 
               subset = 'residents in the South county region ',
               with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'tmean', paste0('cum_lag', lag_num, '_south_death_rr.png')), 
       height = 8, width = 12 )

# Plot cumulative percent change of death for all temperatures compared to the median observed temperature
plot_cum_tmean(lag_num = lag_num,
               mod = mod_south_tmean, 
               outcome = 'death',
               moa = 'pct_chg',
               temp_var = 'tmean', 
               estimates = estimates_south_tmean, 
               subset = 'residents in the South county region ',
               with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'tmean', paste0('cum_lag', lag_num, '_south_death_pct_chg.png')), 
       height = 8, width = 12 )

## Create plot in environment without histogram for temperature
plot_south_tmean <- plot_cum_tmean(lag_num = lag_num,
                                   mod = mod_south_tmean, 
                                   outcome = 'death',
                                   moa = 'pct_chg',
                                   temp_var = 'tmean', 
                                   estimates = estimates_south_tmean, 
                                   subset = 'residents in the South county region ',
                                   with_hist = FALSE
)

### HeatRisk ----

# Plot cumulative relative risk of death for all temperatures compared to the median observed temperature
plot_cum_hr(lag_num = lag_num,
                   mod = mod_south_heat_risk, 
                   outcome = 'death',
                   moa = 'rr',
                   temp_var = 'heat_risk', 
                   estimates = estimates_south_heat_risk,
                   subset = 'residents in the South county region ',
                   with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'heat_risk', paste0('cum_lag', lag_num, '_south_death_rr.png')), 
       height = 8, width = 12 )

# Plot cumulative percent change of death for all temperatures compared to the median observed temperature
plot_cum_hr(lag_num = lag_num,
                   mod = mod_south_heat_risk, 
                   outcome = 'death',
                   moa = 'pct_chg',
                   temp_var = 'heat_risk', 
                   estimates = estimates_south_heat_risk,
                   subset = 'residents in the South county region ',
                   with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'heat_risk', paste0('cum_lag', lag_num, '_south_death_pct_chg.png')), 
       height = 8, width = 12 )

## Create plot in environment without histogram for temperature
plot_south_heat_risk <- plot_cum_hr(lag_num = lag_num,
                                           mod = mod_south_heat_risk, 
                                           outcome = 'death',
                                           moa = 'pct_chg',
                                           temp_var = 'heat_risk', 
                                           estimates = estimates_south_heat_risk,
                                           subset = 'residents in the South county region ',
                                           with_hist = FALSE
)

################################################################################-
## 2: Plot all regions cumulative risk, together ----
################################################################################-

### Mean temperature ----

#### All on same figure ----
plot_cum_tmean_together(
  lag_num = lag_num,
  mod = mod_south_tmean,
  outcome = 'death',
  moa = 'pct_chg',
  temp_var = 'tmean',
  group = c('North', 'Mid', 'South', 'Coastside'),
  estimates = list(estimates_north_tmean, estimates_mid_tmean, estimates_south_tmean, estimates_coastside_tmean),
  subset_var = 'Region',
  with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'tmean', paste0('cum_lag', lag_num, '_allregions_death_together.png')), 
       height = 8, width = 12 )

#### Using cowplot ----

# Manually set ymin and ymax for  gridded plot, so that y axes align
ymin <- -25
ymax <- 100
ymin_scale <- -25
ymax_scale <- 100

# Plot all regions together
plot_grid(
  plot_north_tmean + 
    coord_cartesian(ylim = c(ymin, ymax),
                    xlim = c(2, 34)) +
    scale_y_continuous(breaks = seq(ymin_scale, ymax_scale, by = 25)) +
    labs(title = 'North county') +
    theme(plot.margin = margin(t = 20, b = 10),
          plot.title = element_text(size = 20),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()),
  plot_mid_tmean + 
    coord_cartesian(ylim = c(ymin, ymax),
                    xlim = c(2, 34)) +
    scale_y_continuous(breaks = seq(ymin_scale, ymax_scale, by = 25)) +
    labs(title = 'Mid-county') +
    theme(plot.margin = margin(t = 20, b = 10),
          plot.title = element_text(size = 20),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()),
  plot_south_tmean + 
    coord_cartesian(ylim = c(ymin, ymax),
                    xlim = c(2, 34)) +
    scale_y_continuous(breaks = seq(ymin_scale, ymax_scale, by = 25)) +
    labs(title = 'South county') +
    theme(plot.margin = margin(t = 20, b = 10),
          plot.title = element_text(size = 20),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()),
  plot_coastside_tmean + 
    coord_cartesian(ylim = c(ymin, ymax),
                    xlim = c(2, 34)) +
    scale_y_continuous(breaks = seq(ymin_scale, ymax_scale, by = 25)) +
    labs(title = 'Coastside') +
    theme(plot.margin = margin(t = 20, b = 10),
          plot.title = element_text(size = 20)),
  nrow = 4,
  rel_heights = c(1, 1, 1, 1.1),
  align = 'v'
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'tmean', paste0('cum_lag', lag_num, '_allregions_death.png')), 
       height = 12, width = 4.5 )

### HeatRisk ----

#### All on same figure ----
plot_cum_hr_together(
  lag_num = lag_num,
  mod = mod_south_heat_risk,
  outcome = 'death',
  moa = 'pct_chg',
  temp_var = 'heat_risk',
  group = c('North', 'Mid', 'South', 'Coastside'),
  estimates = list(estimates_north_heat_risk, estimates_mid_heat_risk, estimates_south_heat_risk, estimates_coastside_heat_risk),
  subset_var = 'Region',
  with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'heat_risk', paste0('cum_lag', lag_num, '_allregions_death_together.png')), 
       height = 8, width = 12 )

#### Using cowplot ----

# Manually set ymin and ymax for  gridded plot, so that y axes align
ymin <- -25
ymax <- 150

# Plot all regions together
plot_grid(
  plot_north_heat_risk + 
    coord_cartesian(ylim = c(ymin, ymax)) +
    labs(title = 'North county') +
    theme(plot.margin = margin(t = 20, b = 10),
          plot.title = element_text(size = 20),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()),
  plot_mid_heat_risk + 
    coord_cartesian(ylim = c(ymin, ymax)) +
    labs(title = 'Mid-county') +
    theme(plot.margin = margin(t = 20, b = 10),
          plot.title = element_text(size = 20),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()),
  plot_south_heat_risk + 
    coord_cartesian(ylim = c(ymin, ymax)) +
    labs(title = 'South county') +
    theme(plot.margin = margin(t = 20, b = 10),
          plot.title = element_text(size = 20),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()),
  plot_coastside_heat_risk + 
    coord_cartesian(ylim = c(ymin, ymax)) +
    labs(title = 'Coastside') +
    theme(plot.margin = margin(t = 20, b = 10),
          plot.title = element_text(size = 20)),
  nrow = 4,
  align = 'v'
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'heat_risk', paste0('cum_lag', lag_num, '_allregions_death.png')), 
       height = 8, width = 12 )

################################################################################-
## 3: Clean up ----
################################################################################-

rm(data_source_name,
   selected_constraints_death, region_grid,
   estimates_coastside_tmean, estimates_coastside_heat_risk,
   estimates_mid_tmean, estimates_mid_heat_risk,
   estimates_north_tmean, estimates_north_heat_risk,
   estimates_south_tmean, estimates_south_heat_risk,
   mod_coastside_tmean, mod_coastside_heat_risk,
   mod_mid_tmean, mod_mid_heat_risk,
   mod_north_tmean, mod_north_heat_risk,
   mod_south_tmean, mod_south_heat_risk,
   plot_coastside_tmean, plot_coastside_heat_risk,
   plot_mid_tmean, plot_mid_heat_risk,
   plot_north_tmean, plot_north_heat_risk,
   plot_south_tmean, plot_south_heat_risk,
   ymin, ymax
)
