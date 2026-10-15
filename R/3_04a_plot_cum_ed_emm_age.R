################################################################################-
# Plot: Cumulative of lags 0-6 for mean temperatures and HeatRisk, ED visits, age group----
# Comparing HeatRisk and temperature for local heat surveillance: A time-stratified 
#  case-crossover study of emergency department visits in San Mateo County, 
#  California, 2017–2025
# Juliana GE Bartels
# Updated: 6/22/2026
################################################################################-

# This file plots cumulative percent change in ED visit rate for lag 0-6 days,
# by age group. 

################################################################################-
## 0: Upload data ----
################################################################################-

# Generate suffix for file calling and writing, based on data source
data_source_name <- case_when(data_source == 'original' ~ '',
                              data_source == 'synth' ~ '_synth')

# Load estimates and models for age EMMs
selected_constraints_ed <- read_csv(here::here('outputs', 'tables', 'selected_constraints_ed.csv') ) %>%
  filter(sensitivity == 'main')

age_grid <- expand_grid(
  subset = c('under5', '5to17', '18to64', '65plus'),
  temp_var = c('tmean', 'heat_risk')) %>%
  mutate(estimate_name = paste0('estimates_', subset, '_', temp_var),
         mod_name = paste0('mod_', subset, '_', temp_var))

load_estimates(selected_constraints_ed, 'ed', 'age', age_grid, 'cum', data_source_name)

# Set number of lags for cumulative plots
lag_num <- 6

################################################################################-
## 1a: Plot cumulative risk, (under 5) ----
################################################################################-

### Mean temperature ----

# Plot cumulative relative risk of ED visit for all temperatures compared to the median observed temperature
plot_cum_tmean(lag_num = lag_num,
               mod = mod_under5_tmean, 
               outcome = 'ED visit',
               moa = 'rr',
               temp_var = 'tmean', 
               estimates = estimates_under5_tmean, 
               subset = 'residents under age 5 ',
               with_hist = TRUE
               )

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'ed', paste0('cum', 'tmean', 'cum_lag', lag_num, '_under5_ed_rr.png')), 
       height = 8, width = 12 )

# Plot cumulative percent change of ED visit for all temperatures compared to the median observed temperature
plot_cum_tmean(lag_num = lag_num,
               mod = mod_under5_tmean, 
               outcome = 'ED visit',
               moa = 'pct_chg',
               temp_var = 'tmean', 
               estimates = estimates_under5_tmean, 
               subset = 'residents under age 5 ',
               with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'ed', 'cum', 'tmean', paste0('cum_lag', lag_num, '_under5_ed_pct_chg.png')), 
       height = 8, width = 12 )

## Create plot in environment without histogram for temperature
plot_under5_tmean <- plot_cum_tmean(lag_num = lag_num,
                                    mod = mod_under5_tmean, 
                                    outcome = 'ED visit',
                                    moa = 'pct_chg',
                                    temp_var = 'tmean', 
                                    estimates = estimates_under5_tmean, 
                                    subset = 'residents under age 5 ',
                                    with_hist = FALSE
                                    )

### HeatRisk ----

# Plot cumulative relative risk of ED visit for all temperatures compared to the median observed temperature
plot_cum_hr(lag_num = lag_num,
                   mod = mod_under5_heat_risk, 
                   outcome = 'ED visit',
                   moa = 'rr',
                   temp_var = 'heat_risk', 
                   estimates = estimates_under5_heat_risk,
                   subset = 'residents under age 5 ',
                   with_hist = TRUE
                   )

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'ed', 'cum', 'heat_risk', paste0('cum_lag', lag_num, '_under5_ed_rr.png')), 
       height = 8, width = 12 )

# Plot cumulative percent change of ED visit for all temperatures compared to the median observed temperature
plot_cum_hr(lag_num = lag_num,
                   mod = mod_under5_heat_risk, 
                   outcome = 'ED visit',
                   moa = 'pct_chg',
                   temp_var = 'heat_risk', 
                   estimates = estimates_under5_heat_risk,
                   subset = 'residents under age 5 ',
                   with_hist = TRUE
                   )

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'ed', 'cum', 'heat_risk', paste0('cum_lag', lag_num, '_under5_ed_pct_chg.png')), 
       height = 8, width = 12 )

## Create plot in environment without histogram for temperature
plot_under5_heat_risk <- plot_cum_hr(lag_num = lag_num,
                                            mod = mod_under5_heat_risk, 
                                            outcome = 'ED visit',
                                            moa = 'pct_chg',
                                            temp_var = 'heat_risk', 
                                            estimates = estimates_under5_heat_risk,
                                            subset = 'residents under age 5 ',
                                            with_hist = FALSE
                                            )

################################################################################-
## 1b: Plot cumulative risk, (5 to 17) ----
################################################################################-

### Mean temperature ----

# Plot cumulative relative risk of ED visit for all temperatures compared to the median observed temperature
plot_cum_tmean(lag_num = lag_num,
               mod = mod_5to17_tmean, 
               outcome = 'ED visit',
               moa = 'rr',
               temp_var = 'tmean', 
               estimates = estimates_5to17_tmean, 
               subset = 'residents age 5 to 17 ',
               with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'ed', 'cum', 'tmean', paste0('cum_lag', lag_num, '_5to17_ed_rr.png')), 
       height = 8, width = 12 )

# Plot cumulative percent change of ED visit for all temperatures compared to the median observed temperature
plot_cum_tmean(lag_num = lag_num,
               mod = mod_5to17_tmean, 
               outcome = 'ED visit',
               moa = 'pct_chg',
               temp_var = 'tmean', 
               estimates = estimates_5to17_tmean, 
               subset = 'residents age 5 to 17 ',
               with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'ed', 'cum', 'tmean', paste0('cum_lag', lag_num, '_5to17_ed_pct_chg.png')), 
       height = 8, width = 12 )

## Create plot in environment without histogram for temperature
plot_5to17_tmean <- plot_cum_tmean(lag_num = lag_num,
                                    mod = mod_5to17_tmean, 
                                    outcome = 'ED visit',
                                    moa = 'pct_chg',
                                    temp_var = 'tmean', 
                                    estimates = estimates_5to17_tmean, 
                                    subset = 'residents age 5 to 17 ',
                                    with_hist = FALSE
)

### HeatRisk ----

# Plot cumulative relative risk of ED visit for all temperatures compared to the median observed temperature
plot_cum_hr(lag_num = lag_num,
                   mod = mod_5to17_heat_risk, 
                   outcome = 'ED visit',
                   moa = 'rr',
                   temp_var = 'heat_risk', 
                   estimates = estimates_5to17_heat_risk,
                   subset = 'residents age 5 to 17 ',
                   with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'ed', 'cum', 'heat_risk', paste0('cum_lag', lag_num, '_5to17_ed_rr.png')), 
       height = 8, width = 12 )

# Plot cumulative percent change of ED visit for all temperatures compared to the median observed temperature
plot_cum_hr(lag_num = lag_num,
                   mod = mod_5to17_heat_risk, 
                   outcome = 'ED visit',
                   moa = 'pct_chg',
                   temp_var = 'heat_risk', 
                   estimates = estimates_5to17_heat_risk,
                   subset = 'residents age 5 to 17 ',
                   with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'ed', 'cum', 'heat_risk', paste0('cum_lag', lag_num, '_5to17_ed_pct_chg.png')), 
       height = 8, width = 12 )

## Create plot in environment without histogram for temperature
plot_5to17_heat_risk <- plot_cum_hr(lag_num = lag_num,
                                            mod = mod_5to17_heat_risk, 
                                            outcome = 'ED visit',
                                            moa = 'pct_chg',
                                            temp_var = 'heat_risk', 
                                            estimates = estimates_5to17_heat_risk,
                                            subset = 'residents age 5 to 17 ',
                                            with_hist = FALSE
)

################################################################################-
## 1c: Plot cumulative risk, (18 to 64) ----
################################################################################-

### Mean temperature ----

# Plot cumulative relative risk of ED visit for all temperatures compared to the median observed temperature
plot_cum_tmean(lag_num = lag_num,
               mod = mod_18to64_tmean, 
               outcome = 'ED visit',
               moa = 'rr',
               temp_var = 'tmean', 
               estimates = estimates_18to64_tmean, 
               subset = 'residents age 18 to 64 ',
               with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'ed', 'cum', 'tmean', paste0('cum_lag', lag_num, '_18to64_ed_rr.png')), 
       height = 8, width = 12 )

# Plot cumulative percent change of ED visit for all temperatures compared to the median observed temperature
plot_cum_tmean(lag_num = lag_num,
               mod = mod_18to64_tmean, 
               outcome = 'ED visit',
               moa = 'pct_chg',
               temp_var = 'tmean', 
               estimates = estimates_18to64_tmean, 
               subset = 'residents age 18 to 64 ',
               with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'ed', 'cum', 'tmean', paste0('cum_lag', lag_num, '_18to64_ed_pct_chg.png')), 
       height = 8, width = 12 )

## Create plot in environment without histogram for temperature
plot_18to64_tmean <- plot_cum_tmean(lag_num = lag_num,
                                   mod = mod_18to64_tmean, 
                                   outcome = 'ED visit',
                                   moa = 'pct_chg',
                                   temp_var = 'tmean', 
                                   estimates = estimates_18to64_tmean, 
                                   subset = 'residents age 18 to 64 ',
                                   with_hist = FALSE
)

### HeatRisk ----

# Plot cumulative relative risk of ED visit for all temperatures compared to the median observed temperature
plot_cum_hr(lag_num = lag_num,
                   mod = mod_18to64_heat_risk, 
                   outcome = 'ED visit',
                   moa = 'rr',
                   temp_var = 'heat_risk', 
                   estimates = estimates_18to64_heat_risk,
                   subset = 'residents age 18 to 64 ',
                   with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'ed', 'cum', 'heat_risk', paste0('cum_lag', lag_num, '_18to64_ed_rr.png')), 
       height = 8, width = 12 )

# Plot cumulative percent change of ED visit for all temperatures compared to the median observed temperature
plot_cum_hr(lag_num = lag_num,
                   mod = mod_18to64_heat_risk, 
                   outcome = 'ED visit',
                   moa = 'pct_chg',
                   temp_var = 'heat_risk', 
                   estimates = estimates_18to64_heat_risk,
                   subset = 'residents age 18 to 64 ',
                   with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'ed', 'cum', 'heat_risk', paste0('cum_lag', lag_num, '_18to64_ed_pct_chg.png')), 
       height = 8, width = 12 )

## Create plot in environment without histogram for temperature
plot_18to64_heat_risk <- plot_cum_hr(lag_num = lag_num,
                                           mod = mod_18to64_heat_risk, 
                                           outcome = 'ED visit',
                                           moa = 'pct_chg',
                                           temp_var = 'heat_risk', 
                                           estimates = estimates_18to64_heat_risk,
                                           subset = 'residents age 18 to 64 ',
                                           with_hist = FALSE
)

################################################################################-
## 1d: Plot cumulative risk, (65 plus) ----
################################################################################-

### Mean temperature ----

# Plot cumulative relative risk of ED visit for all temperatures compared to the median observed temperature
plot_cum_tmean(lag_num = lag_num,
               mod = mod_65plus_tmean, 
               outcome = 'ED visit',
               moa = 'rr',
               temp_var = 'tmean', 
               estimates = estimates_65plus_tmean, 
               subset = 'residents over age 65 ',
               with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'ed', 'cum', 'tmean', paste0('cum_lag', lag_num, '_65plus_ed_rr.png')), 
       height = 8, width = 12 )

# Plot cumulative percent change of ED visit for all temperatures compared to the median observed temperature
plot_cum_tmean(lag_num = lag_num,
               mod = mod_65plus_tmean, 
               outcome = 'ED visit',
               moa = 'pct_chg',
               temp_var = 'tmean', 
               estimates = estimates_65plus_tmean, 
               subset = 'residents over age 65 ',
               with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'ed', 'cum', 'tmean', paste0('cum_lag', lag_num, '_65plus_ed_pct_chg.png')), 
       height = 8, width = 12 )

## Create plot in environment without histogram for temperature
plot_65plus_tmean <- plot_cum_tmean(lag_num = lag_num,
                                    mod = mod_65plus_tmean, 
                                    outcome = 'ED visit',
                                    moa = 'pct_chg',
                                    temp_var = 'tmean', 
                                    estimates = estimates_65plus_tmean, 
                                    subset = 'residents over age 65 ',
                                    with_hist = FALSE
)

### HeatRisk ----

# Plot cumulative relative risk of ED visit for all temperatures compared to the median observed temperature
plot_cum_hr(lag_num = lag_num,
                   mod = mod_65plus_heat_risk, 
                   outcome = 'ED visit',
                   moa = 'rr',
                   temp_var = 'heat_risk', 
                   estimates = estimates_65plus_heat_risk,
                   subset = 'residents over age 65 ',
                   with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'ed', 'cum', 'heat_risk', paste0('cum_lag', lag_num, '_65plus_ed_rr.png')), 
       height = 8, width = 12 )

# Plot cumulative percent change of ED visit for all temperatures compared to the median observed temperature
plot_cum_hr(lag_num = lag_num,
                   mod = mod_65plus_heat_risk, 
                   outcome = 'ED visit',
                   moa = 'pct_chg',
                   temp_var = 'heat_risk', 
                   estimates = estimates_65plus_heat_risk,
                   subset = 'residents over age 65 ',
                   with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'ed', 'cum', 'heat_risk', paste0('cum_lag', lag_num, '_65plus_ed_pct_chg.png')), 
       height = 8, width = 12 )

## Create plot in environment without histogram for temperature
plot_65plus_heat_risk <- plot_cum_hr(lag_num = lag_num,
                                            mod = mod_65plus_heat_risk, 
                                            outcome = 'ED visit',
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
  mod = mod_under5_tmean,
  outcome = 'ED visit',
  moa = 'pct_chg',
  temp_var = 'tmean',
  group = c('< 5', '5-17', '18-64', '\u2265 65'),
  estimates = list(estimates_under5_tmean, estimates_5to17_tmean, estimates_18to64_tmean, estimates_65plus_tmean),
  subset_var = 'Age group',
  with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'ed', 'cum', 'tmean', paste0('cum_lag', lag_num, '_allages_ed_together.png')), 
       height = 8, width = 12 )


#### Using cowplot ----

# Manually set ymin and ymax for  gridded plot, so that y axes align
ymin <- -50
ymax <- 100
ymin_scale <- -50
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
  plot_5to17_tmean + 
    coord_cartesian(ylim = c(ymin, ymax),
                    xlim = c(2, 34)) +
    scale_y_continuous(breaks = seq(ymin_scale, ymax_scale, by = 50)) +
    labs(title = '5-17') +
    theme(plot.margin = margin(t = 20, b = 10),
          plot.title = element_text(size = 20),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()),
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
    theme(plot.margin = margin(t = 20, b = 20),
          plot.title = element_text(size = 20)),
  rel_heights = c(1, 1, 1, 1.1),
  nrow = 4,
  align = 'v'
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'ed', 'cum', 'tmean', paste0('cum_lag', lag_num, '_allages_ed.png')), 
       height = 12, width = 4.5 )


### HeatRisk ----

#### All on same figure ----
plot_cum_hr_together(
  lag_num = lag_num,
  mod = mod_under5_heat_risk,
  outcome = 'ED visit',
  moa = 'rr',
  temp_var = 'heat_risk',
  group = c('< 5', '5-17', '18-64', '\u2265 65'),
  estimates = list(estimates_under5_heat_risk, estimates_5to17_heat_risk, estimates_18to64_heat_risk, estimates_65plus_heat_risk),
  subset_var = 'Age group',
  with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'ed', 'cum', 'heat_risk', paste0('cum_lag', lag_num, '_allages_ed_together.png')), 
       height = 8, width = 12 )

#### All on same figure percent change ----
plot_cum_hr_together(
  lag_num = lag_num,
  mod = mod_under5_heat_risk,
  outcome = 'ED visit',
  moa = 'pct_chg',
  temp_var = 'heat_risk',
  group = c('< 5', '5-17', '18-64', '\u2265 65'),
  estimates = list(estimates_under5_heat_risk, estimates_5to17_heat_risk, estimates_18to64_heat_risk, estimates_65plus_heat_risk),
  subset_var = 'Age group',
  with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'ed', 'cum', 'heat_risk', paste0('cum_lag', lag_num, '_allages_ed_together_pct_chg.png')), 
       height = 8, width = 12 )

################################################################################-
## 3: Clean up ----
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
   plot_under5_tmean, plot_under5_heat_risk,
   plot_5to17_tmean, plot_5to17_heat_risk,
   plot_18to64_tmean, plot_18to64_heat_risk,
   plot_65plus_tmean, plot_65plus_heat_risk,
   ymin, ymax
   )

