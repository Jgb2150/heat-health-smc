################################################################################-
# Plot: Cumulative of lags 0-6 for mean temperatures and HeatRisk, deaths, age group----
# Moderate temperatures and HeatRisk associated with increased risk of all-cause 
#  emergency department visits among residents of San Mateo County, CA, 2017-2025: 
#  a time-stratified case crossover study 
# Juliana GE Bartels
# Updated: 9/3/2026
################################################################################-

# This file plots cumulative percent change in death rate for lag 0-6 days,
# by age group. 

################################################################################-
## 0: Upload data ----
################################################################################-

# Generate suffix for file calling and writing, based on data source
data_source_name <- case_when(data_source == 'original' ~ '',
                              data_source == 'synth' ~ '_synth')

# Load estimates and models for age EMMs
selected_constraints_death <- read_csv(here::here('outputs', 'tables', 'selected_constraints_death.csv') ) %>%
  filter(sensitivity == 'main')

age_grid <- expand_grid(
  subset = c('under5', '18to64', '65plus'),
  temp_var = c('tmean', 'heat_risk')) %>%
  mutate(estimate_name = paste0('estimates_', subset, '_', temp_var),
         mod_name = paste0('mod_', subset, '_', temp_var))

load_estimates(selected_constraints_death, 'death', 'age', age_grid, 'cum', data_source_name)

# Set number of lags for cumulative plots
lag_num <- 6

################################################################################-
## 1a: Plot cumulative risk, (< 5) ----
################################################################################-

### Mean temperature ----

# Plot cumulative relative risk of death for all temperatures compared to the median observed temperature
plot_cum_tmean(lag_num = lag_num,
               mod = mod_under5_tmean, 
               outcome = 'death',
               moa = 'rr',
               temp_var = 'tmean', 
               estimates = estimates_under5_tmean, 
               subset = 'residents under age 5 ',
               with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'tmean', paste0('cum_lag', lag_num, '_under5_death_rr.png')), 
       height = 8, width = 12 )

# Plot cumulative percent change of death for all temperatures compared to the median observed temperature
plot_cum_tmean(lag_num = lag_num,
               mod = mod_under5_tmean, 
               outcome = 'death',
               moa = 'pct_chg',
               temp_var = 'tmean', 
               estimates = estimates_under5_tmean, 
               subset = 'residents under age 5 ',
               with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'tmean', paste0('cum_lag', lag_num, '_under5_death_pct_chg.png')), 
       height = 8, width = 12 )

## Create plot in environment without histogram for temperature
plot_under5_tmean <- plot_cum_tmean(lag_num = lag_num,
                                    mod = mod_under5_tmean, 
                                    outcome = 'death',
                                    moa = 'pct_chg',
                                    temp_var = 'tmean', 
                                    estimates = estimates_under5_tmean, 
                                    subset = 'residents under age 5 ',
                                    with_hist = FALSE
)

### HeatRisk ----

# Plot cumulative relative risk of death for all temperatures compared to the median observed temperature
plot_cum_hr(lag_num = lag_num,
            mod = mod_under5_heat_risk, 
            outcome = 'death',
            moa = 'rr',
            temp_var = 'heat_risk', 
            estimates = estimates_under5_heat_risk,
            subset = 'residents under age 5 ',
            with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'heat_risk', paste0('cum_lag', lag_num, '_under5_death_rr.png')), 
       height = 8, width = 12 )

# Plot cumulative percent change of death for all temperatures compared to the median observed temperature
plot_cum_hr(lag_num = lag_num,
            mod = mod_under5_heat_risk, 
            outcome = 'death',
            moa = 'pct_chg',
            temp_var = 'heat_risk', 
            estimates = estimates_under5_heat_risk,
            subset = 'residents under age 5 ',
            with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'heat_risk', paste0('cum_lag', lag_num, '_under5_death_pct_chg.png')), 
       height = 8, width = 12 )

## Create plot in environment without histogram for temperature
plot_under5_heat_risk <- plot_cum_hr(lag_num = lag_num,
                                     mod = mod_under5_heat_risk, 
                                     outcome = 'death',
                                     moa = 'pct_chg',
                                     temp_var = 'heat_risk', 
                                     estimates = estimates_under5_heat_risk,
                                     subset = 'residents under age 5 ',
                                     with_hist = FALSE
)

################################################################################-
## 1b: Plot cumulative risk, (18-64) ----
################################################################################-

### Mean temperature ----

# Plot cumulative relative risk of death for all temperatures compared to the median observed temperature
plot_cum_tmean(lag_num = lag_num,
               mod = mod_18to64_tmean, 
               outcome = 'death',
               moa = 'rr',
               temp_var = 'tmean', 
               estimates = estimates_18to64_tmean, 
               subset = 'residents age 18-64 ',
               with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'tmean', paste0('cum_lag', lag_num, '_18to64_death_rr.png')), 
       height = 8, width = 12 )

# Plot cumulative percent change of death for all temperatures compared to the median observed temperature
plot_cum_tmean(lag_num = lag_num,
               mod = mod_18to64_tmean, 
               outcome = 'death',
               moa = 'pct_chg',
               temp_var = 'tmean', 
               estimates = estimates_18to64_tmean, 
               subset = 'residents age 18-64 ',
               with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'tmean', paste0('cum_lag', lag_num, '_18to64_death_pct_chg.png')), 
       height = 8, width = 12 )

## Create plot in environment without histogram for temperature
plot_18to64_tmean <- plot_cum_tmean(lag_num = lag_num,
                                    mod = mod_18to64_tmean, 
                                    outcome = 'death',
                                    moa = 'pct_chg',
                                    temp_var = 'tmean', 
                                    estimates = estimates_18to64_tmean, 
                                    subset = 'residents age 18-64 ',
                                    with_hist = FALSE
)

### HeatRisk ----

# Plot cumulative relative risk of death for all temperatures compared to the median observed temperature
plot_cum_hr(lag_num = lag_num,
            mod = mod_18to64_heat_risk, 
            outcome = 'death',
            moa = 'rr',
            temp_var = 'heat_risk', 
            estimates = estimates_18to64_heat_risk,
            subset = 'residents age 18-64 ',
            with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'heat_risk', paste0('cum_lag', lag_num, '_18to64_death_rr.png')), 
       height = 8, width = 12 )

# Plot cumulative percent change of death for all temperatures compared to the median observed temperature
plot_cum_hr(lag_num = lag_num,
            mod = mod_18to64_heat_risk, 
            outcome = 'death',
            moa = 'pct_chg',
            temp_var = 'heat_risk', 
            estimates = estimates_18to64_heat_risk,
            subset = 'residents age 18-64 ',
            with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'heat_risk', paste0('cum_lag', lag_num, '_18to64_death_pct_chg.png')), 
       height = 8, width = 12 )

## Create plot in environment without histogram for temperature
plot_18to64_heat_risk <- plot_cum_hr(lag_num = lag_num,
                                     mod = mod_18to64_heat_risk, 
                                     outcome = 'death',
                                     moa = 'pct_chg',
                                     temp_var = 'heat_risk', 
                                     estimates = estimates_18to64_heat_risk,
                                     subset = 'residents age 18-64 ',
                                     with_hist = FALSE
)

################################################################################-
## 1c: Plot cumulative risk, (\u2265 65) ----
################################################################################-

### Mean temperature ----

# Plot cumulative relative risk of death for all temperatures compared to the median observed temperature
plot_cum_tmean(lag_num = lag_num,
               mod = mod_65plus_tmean, 
               outcome = 'death',
               moa = 'rr',
               temp_var = 'tmean', 
               estimates = estimates_65plus_tmean, 
               subset = 'residents over age 65 ',
               with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'tmean', paste0('cum_lag', lag_num, '_65plus_death_rr.png')), 
       height = 8, width = 12 )

# Plot cumulative percent change of death for all temperatures compared to the median observed temperature
plot_cum_tmean(lag_num = lag_num,
               mod = mod_65plus_tmean, 
               outcome = 'death',
               moa = 'pct_chg',
               temp_var = 'tmean', 
               estimates = estimates_65plus_tmean, 
               subset = 'residents over age 65 ',
               with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'tmean', paste0('cum_lag', lag_num, '_65plus_death_pct_chg.png')), 
       height = 8, width = 12 )

## Create plot in environment without histogram for temperature
plot_65plus_tmean <- plot_cum_tmean(lag_num = lag_num,
                                    mod = mod_65plus_tmean, 
                                    outcome = 'death',
                                    moa = 'pct_chg',
                                    temp_var = 'tmean', 
                                    estimates = estimates_65plus_tmean, 
                                    subset = 'residents over age 65 ',
                                    with_hist = FALSE
)

### HeatRisk ----

# Plot cumulative relative risk of death for all temperatures compared to the median observed temperature
plot_cum_hr(lag_num = lag_num,
            mod = mod_65plus_heat_risk, 
            outcome = 'death',
            moa = 'rr',
            temp_var = 'heat_risk', 
            estimates = estimates_65plus_heat_risk,
            subset = 'residents over age 65 ',
            with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'heat_risk', paste0('cum_lag', lag_num, '_65plus_death_rr.png')), 
       height = 8, width = 12 )

# Plot cumulative percent change of death for all temperatures compared to the median observed temperature
plot_cum_hr(lag_num = lag_num,
            mod = mod_65plus_heat_risk, 
            outcome = 'death',
            moa = 'pct_chg',
            temp_var = 'heat_risk', 
            estimates = estimates_65plus_heat_risk,
            subset = 'residents over age 65 ',
            with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'heat_risk', paste0('cum_lag', lag_num, '_65plus_death_pct_chg.png')), 
       height = 8, width = 12 )

## Create plot in environment without histogram for temperature
plot_65plus_heat_risk <- plot_cum_hr(lag_num = lag_num,
                                     mod = mod_65plus_heat_risk, 
                                     outcome = 'death',
                                     moa = 'pct_chg',
                                     temp_var = 'heat_risk', 
                                     estimates = estimates_65plus_heat_risk,
                                     subset = 'residents over age 65 ',
                                     with_hist = FALSE
)

################################################################################-
## 2: Plot all age group cumulative risk, together ----
################################################################################-

### Mean temperature ----

#### All on same figure ----
plot_cum_tmean_together(
  lag_num = lag_num,
  mod = mod_18to64_tmean,
  outcome = 'death',
  moa = 'pct_chg',
  temp_var = 'tmean',
  group = c('< 5', '18-64', '\u2265 65'),
  estimates = list(estimates_under5_tmean, estimates_18to64_tmean, estimates_65plus_tmean),
  subset_var = 'Age group',
  with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'tmean', paste0('cum_lag', lag_num, '_allages_death_together.png')), 
       height = 8, width = 12 )


#### Using cowplot ----

# Manually set ymin and ymax for  gridded plot, so that y axes align
ymin <- -100
ymax <- 100
ymin_scale <- -100
ymax_scale <- 100

# Plot all ages together
plot_grid(
  plot_under5_tmean +
    coord_cartesian(ylim = c(ymin, ymax),
                    xlim = c(2, 34)) +
    scale_y_continuous(breaks = seq(ymin_scale, ymax_scale, by = 50)) +
    labs(title = '< 5') +
    theme(plot.margin = margin(t = 20, b = 10),
          plot.title = element_text(size = 20),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()),
  NULL,
  plot_18to64_tmean + 
    coord_cartesian(ylim = c(ymin, ymax),
                    xlim = c(2, 34)) +
    scale_y_continuous(breaks = seq(ymin_scale, ymax_scale, by = 50)) +
    labs(title = '18-64') +
    theme(plot.margin = margin(t = 20, b = 10),
          plot.title = element_text(size = 20),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()),
  plot_65plus_tmean + 
    coord_cartesian(ylim = c(ymin, ymax),
                    xlim = c(2, 34)) +
    scale_y_continuous(breaks = seq(ymin_scale, ymax_scale, by = 50)) +
    labs(title = '\u2265 65') +
    theme(plot.margin = margin(t = 20, b = 10),
          plot.title = element_text(size = 20)),
  nrow = 4,
  rel_heights = c(1, 1, 1, 1.1),
  align = 'v'
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'tmean', paste0('cum_lag', lag_num, '_allages_death.png')), 
       height = 12, width = 4.5 )


### HeatRisk ----

#### All on same figure ----
# Have to manually adjust the legend position in the function to 00_14d_plot_cum_hr_together()
plot_cum_hr_together(
  lag_num = lag_num,
  mod = mod_18to64_heat_risk,
  outcome = 'death',
  moa = 'pct_chg',
  temp_var = 'heat_risk',
  group = c('< 5', '18-64', '\u2265 65'),
  estimates = list(estimates_under5_heat_risk, estimates_18to64_heat_risk, estimates_65plus_heat_risk),
  subset_var = 'Age group',
  with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'heat_risk', paste0('cum_lag', lag_num, '_allages_death_together.png')), 
       height = 8, width = 12 )

#### Using cowplot ----

# Manually set ymin and ymax for  gridded plot, so that y axes align
ymin <- -100
ymax <- 100

# Plot all ages together
plot_grid(
  plot_under5_heat_risk +
    coord_cartesian(ylim = c(ymin, ymax)) +
    labs(title = '< 5') +
    theme(plot.margin = margin(t = 20, b = 10),
          plot.title = element_text(size = 20),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()),
  NULL,
  plot_18to64_heat_risk + 
    coord_cartesian(ylim = c(ymin, ymax)) +
    labs(title = '18-64') +
    theme(plot.margin = margin(t = 20, b = 10),
          plot.title = element_text(size = 20),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()),
  plot_65plus_heat_risk + 
    coord_cartesian(ylim = c(ymin, ymax)) +
    labs(title = '\u2265 65') +
    theme(plot.margin = margin(t = 20, b = 10),
          plot.title = element_text(size = 20)),
  nrow = 4,
  align = 'v'
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'heat_risk', paste0('cum_lag', lag_num, '_allages_death.png')), 
       height = 12, width = 4.5)

################################################################################-
## 3: Clean up ----
################################################################################-

rm(data_source_name,
   selected_constraints_death, age_grid,
   estimates_under5_tmean, estimates_under5_heat_risk,
   estimates_18to64_tmean, estimates_18to64_heat_risk,
   estimates_65plus_tmean, estimates_65plus_heat_risk,
   mod_under5_tmean, mod_under5_heat_risk,
   mod_18to64_tmean, mod_18to64_heat_risk,
   mod_65plus_tmean, mod_65plus_heat_risk,
   plot_under5_tmean, plot_under5_heat_risk,
   plot_18to64_tmean, plot_18to64_heat_risk,
   plot_65plus_tmean, plot_65plus_heat_risk,
   ymin, ymax
)
