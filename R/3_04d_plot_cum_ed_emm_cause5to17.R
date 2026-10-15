################################################################################-
# Plot: Cumulative of lags 0-6 for mean temperatures and HeatRisk, ED visits, by cause, among residents age 5 to 17----
# Comparing HeatRisk and temperature for local heat surveillance: A time-stratified 
#  case-crossover study of emergency department visits in San Mateo County, 
#  California, 2017–2025
# Juliana GE Bartels
# Updated: 8/28/2026
################################################################################-

# This file plots cumulative percent change in ED visit rate for lag 0-6 days,
# by cause, among residents age 5 to 17. 

################################################################################-
## 0: Upload data ----
################################################################################-

# Generate suffix for file calling and writing, based on data source
data_source_name <- case_when(data_source == 'original' ~ '',
                              data_source == 'synth' ~ '_synth')

# Load estimates and models for cause EMMs
selected_constraints_ed <- read_csv(here::here('outputs', 'tables', 'selected_constraints_ed.csv') ) %>%
  filter(sensitivity == 'main')

cause_grid <- expand_grid(
  subset = c('cardiovascular', 
             'respiratory', 
             #'stroke', 
             #'intentinjury',
             'unintentinjury', 
             #'heat',
             'substance',
             'mental', 
             #'renal', 
             'genitourinary'),
  temp_var = c('tmean', 'heat_risk')) %>%
  mutate(estimate_name = paste0('estimates_', subset, '_', temp_var),
         mod_name = paste0('mod_', subset, '_', temp_var))

load_estimates(selected_constraints_ed, 'ed', 'cause5to17', cause_grid, 'cum', data_source_name)

# Set number of lags for cumulative plots
lag_num <- 6

################################################################################-
## 1a: Plot cumulative risk, (cardiovascular-) ----
################################################################################-

### Mean temperature ----

# Plot cumulative  relative risk of ED visit for all temperatures compared to the median observed temperature
plot_cum_tmean(lag_num = lag_num,
               mod = mod_cardiovascular_tmean, 
               outcome = 'ED visit',
               moa = 'rr',
               temp_var = 'tmean', 
               estimates = estimates_cardiovascular_tmean, 
               subset = 'patients with cardiovascular cause of ED visit ',
               with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'ed', 'cum', 'tmean', paste0('cum_lag', lag_num, '_cardiovascular_5to17_ed_rr.png')), 
       height = 8, width = 12 )

# Plot cumulative  percent change of ED visit for all temperatures compared to the median observed temperature
plot_cum_tmean(lag_num = lag_num,
               mod = mod_cardiovascular_tmean, 
               outcome = 'ED visit',
               moa = 'pct_chg',
               temp_var = 'tmean', 
               estimates = estimates_cardiovascular_tmean, 
               subset = 'patients with cardiovascular cause of ED visit ',
               with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'ed', 'cum', 'tmean', paste0('cum_lag', lag_num, '_cardiovascular_5to17_ed_pct_chg.png')), 
       height = 8, width = 12 )

## Create plot in environment without histogram for temperature
plot_cardiovascular_tmean <- plot_cum_tmean(lag_num = lag_num,
                                            mod = mod_cardiovascular_tmean, 
                                            outcome = 'ED visit',
                                            moa = 'pct_chg',
                                            temp_var = 'tmean', 
                                            estimates = estimates_cardiovascular_tmean, 
                                            subset = 'patients with cardiovascular cause of ED visit ',
                                            with_hist = FALSE
)

### HeatRisk ----

# Plot cumulative  relative risk of ED visit for all temperatures compared to the median observed temperature
plot_cum_hr(lag_num = lag_num,
            mod = mod_cardiovascular_heat_risk, 
            outcome = 'ED visit',
            moa = 'rr',
            temp_var = 'heat_risk', 
            estimates = estimates_cardiovascular_heat_risk,
            subset = 'patients with cardiovascular cause of ED visit ',
            with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'ed', 'cum', 'heat_risk', paste0('cum_lag', lag_num, '_cardiovascular_5to17_ed_rr.png')), 
       height = 8, width = 12 )

# Plot cumulative  percent change of ED visit for all temperatures compared to the median observed temperature
plot_cum_hr(lag_num = lag_num,
            mod = mod_cardiovascular_heat_risk, 
            outcome = 'ED visit',
            moa = 'pct_chg',
            temp_var = 'heat_risk', 
            estimates = estimates_cardiovascular_heat_risk,
            subset = 'patients with cardiovascular cause of ED visit ',
            with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'ed', 'cum', 'heat_risk', paste0('cum_lag', lag_num, '_cardiovascular_5to17_ed_pct_chg.png')), 
       height = 8, width = 12 )

## Create plot in environment without histogram for temperature
plot_cardiovascular_heat_risk <- plot_cum_hr(lag_num = lag_num,
                                             mod = mod_cardiovascular_heat_risk, 
                                             outcome = 'ED visit',
                                             moa = 'pct_chg',
                                             temp_var = 'heat_risk', 
                                             estimates = estimates_cardiovascular_heat_risk,
                                             subset = 'patients with cardiovascular cause of ED visit ',
                                             with_hist = FALSE
)

################################################################################-
## 1b: Plot cumulative risk, (respiratory ) ----
################################################################################-

### Mean temperature ----

# Plot cumulative  relative risk of ED visit for all temperatures compared to the median observed temperature
plot_cum_tmean(lag_num = lag_num,
               mod = mod_respiratory_tmean, 
               outcome = 'ED visit',
               moa = 'rr',
               temp_var = 'tmean', 
               estimates = estimates_respiratory_tmean, 
               subset = 'patients with respiratory cause of ED visit ',
               with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'ed', 'cum', 'tmean', paste0('cum_lag', lag_num, '_respiratory_5to17_ed_rr.png')), 
       height = 8, width = 12 )

# Plot cumulative  percent change of ED visit for all temperatures compared to the median observed temperature
plot_cum_tmean(lag_num = lag_num,
               mod = mod_respiratory_tmean, 
               outcome = 'ED visit',
               moa = 'pct_chg',
               temp_var = 'tmean', 
               estimates = estimates_respiratory_tmean, 
               subset = 'patients with respiratory cause of ED visit ',
               with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'ed', 'cum', 'tmean', paste0('cum_lag', lag_num, '_respiratory_5to17_ed_pct_chg.png')), 
       height = 8, width = 12 )

## Create plot in environment without histogram for temperature
plot_respiratory_tmean <- plot_cum_tmean(lag_num = lag_num,
                                         mod = mod_respiratory_tmean, 
                                         outcome = 'ED visit',
                                         moa = 'pct_chg',
                                         temp_var = 'tmean', 
                                         estimates = estimates_respiratory_tmean, 
                                         subset = 'patients with respiratory cause of ED visit ',
                                         with_hist = FALSE
)

### HeatRisk ----

# Plot cumulative  relative risk of ED visit for all temperatures compared to the median observed temperature
plot_cum_hr(lag_num = lag_num,
            mod = mod_respiratory_heat_risk, 
            outcome = 'ED visit',
            moa = 'rr',
            temp_var = 'heat_risk', 
            estimates = estimates_respiratory_heat_risk,
            subset = 'patients with respiratory cause of ED visit ',
            with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'ed', 'cum', 'heat_risk', paste0('cum_lag', lag_num, '_respiratory_5to17_ed_rr.png')), 
       height = 8, width = 12 )

# Plot cumulative  percent change of ED visit for all temperatures compared to the median observed temperature
plot_cum_hr(lag_num = lag_num,
            mod = mod_respiratory_heat_risk, 
            outcome = 'ED visit',
            moa = 'pct_chg',
            temp_var = 'heat_risk', 
            estimates = estimates_respiratory_heat_risk,
            subset = 'patients with respiratory cause of ED visit ',
            with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'ed', 'cum', 'heat_risk', paste0('cum_lag', lag_num, '_respiratory_5to17_ed_pct_chg.png')), 
       height = 8, width = 12 )

## Create plot in environment without histogram for temperature
plot_respiratory_heat_risk <- plot_cum_hr(lag_num = lag_num,
                                          mod = mod_respiratory_heat_risk, 
                                          outcome = 'ED visit',
                                          moa = 'pct_chg',
                                          temp_var = 'heat_risk', 
                                          estimates = estimates_respiratory_heat_risk,
                                          subset = 'patients with respiratory cause of ED visit ',
                                          with_hist = FALSE
)

################################################################################-
## 1c: Plot cumulative risk, (unintentinjury) ----
################################################################################-

### Mean temperature ----

# Plot cumulative  relative risk of ED visit for all temperatures compared to the median observed temperature
plot_cum_tmean(lag_num = lag_num,
               mod = mod_unintentinjury_tmean, 
               outcome = 'ED visit',
               moa = 'rr',
               temp_var = 'tmean', 
               estimates = estimates_unintentinjury_tmean, 
               subset = 'patients with unintentional injury as cause of ED visit ',
               with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'ed', 'cum', 'tmean', paste0('cum_lag', lag_num, '_unintentinjury_5to17_ed_rr.png')), 
       height = 8, width = 12 )

# Plot cumulative  percent change of ED visit for all temperatures compared to the median observed temperature
plot_cum_tmean(lag_num = lag_num,
               mod = mod_unintentinjury_tmean, 
               outcome = 'ED visit',
               moa = 'pct_chg',
               temp_var = 'tmean', 
               estimates = estimates_unintentinjury_tmean, 
               subset = 'patients with unintentional injury as cause of ED visit ',
               with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'ed', 'cum', 'tmean', paste0('cum_lag', lag_num, '_unintentinjury_5to17_ed_pct_chg.png')), 
       height = 8, width = 12 )

## Create plot in environment without histogram for temperature
plot_unintentinjury_tmean <- plot_cum_tmean(lag_num = lag_num,
                                            mod = mod_unintentinjury_tmean, 
                                            outcome = 'ED visit',
                                            moa = 'pct_chg',
                                            temp_var = 'tmean', 
                                            estimates = estimates_unintentinjury_tmean, 
                                            subset = 'patients with unintentional injury as cause of ED visit ',
                                            with_hist = FALSE
)

### HeatRisk ----

# Plot cumulative  relative risk of ED visit for all temperatures compared to the median observed temperature
plot_cum_hr(lag_num = lag_num,
            mod = mod_unintentinjury_heat_risk, 
            outcome = 'ED visit',
            moa = 'rr',
            temp_var = 'heat_risk', 
            estimates = estimates_unintentinjury_heat_risk,
            subset = 'patients with unintentional injury as cause of ED visit ',
            with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'ed', 'cum', 'heat_risk', paste0('cum_lag', lag_num, '_unintentinjury_5to17_ed_rr.png')), 
       height = 8, width = 12 )

# Plot cumulative  percent change of ED visit for all temperatures compared to the median observed temperature
plot_cum_hr(lag_num = lag_num,
            mod = mod_unintentinjury_heat_risk, 
            outcome = 'ED visit',
            moa = 'pct_chg',
            temp_var = 'heat_risk', 
            estimates = estimates_unintentinjury_heat_risk,
            subset = 'patients with unintentional injury as cause of ED visit ',
            with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'ed', 'cum', 'heat_risk', paste0('cum_lag', lag_num, '_unintentinjury_5to17_ed_pct_chg.png')), 
       height = 8, width = 12 )

## Create plot in environment without histogram for temperature
plot_unintentinjury_heat_risk <- plot_cum_hr(lag_num = lag_num,
                                             mod = mod_unintentinjury_heat_risk, 
                                             outcome = 'ED visit',
                                             moa = 'pct_chg',
                                             temp_var = 'heat_risk', 
                                             estimates = estimates_unintentinjury_heat_risk,
                                             subset = 'patients with unintentional injury as cause of ED visit ',
                                             with_hist = FALSE
)

################################################################################-
## 1d: Plot cumulative risk, (substance) ----
################################################################################-

### Mean temperature ----

# Plot cumulative  relative risk of ED visit for all temperatures compared to the median observed temperature
plot_cum_tmean(lag_num = lag_num,
               mod = mod_substance_tmean, 
               outcome = 'ED visit',
               moa = 'rr',
               temp_var = 'tmean', 
               estimates = estimates_substance_tmean, 
               subset = 'patients with substance use as cause of ED visit ',
               with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'ed', 'cum', 'tmean', paste0('cum_lag', lag_num, '_substance_5to17_ed_rr.png')), 
       height = 8, width = 12 )

# Plot cumulative  percent change of ED visit for all temperatures compared to the median observed temperature
plot_cum_tmean(lag_num = lag_num,
               mod = mod_substance_tmean, 
               outcome = 'ED visit',
               moa = 'pct_chg',
               temp_var = 'tmean', 
               estimates = estimates_substance_tmean, 
               subset = 'patients with substance use as cause of ED visit ',
               with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'ed', 'cum', 'tmean', paste0('cum_lag', lag_num, '_substance_5to17_ed_pct_chg.png')), 
       height = 8, width = 12 )

## Create plot in environment without histogram for temperature
plot_substance_tmean <- plot_cum_tmean(lag_num = lag_num,
                                       mod = mod_substance_tmean, 
                                       outcome = 'ED visit',
                                       moa = 'pct_chg',
                                       temp_var = 'tmean', 
                                       estimates = estimates_substance_tmean, 
                                       subset = 'patients with substance use as cause of ED visit ',
                                       with_hist = FALSE
)

### HeatRisk ----

# Plot cumulative  relative risk of ED visit for all temperatures compared to the median observed temperature
plot_cum_hr(lag_num = lag_num,
            mod = mod_substance_heat_risk, 
            outcome = 'ED visit',
            moa = 'rr',
            temp_var = 'heat_risk', 
            estimates = estimates_substance_heat_risk,
            subset = 'patients with substance use as cause of ED visit ',
            with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'ed', 'cum', 'heat_risk', paste0('cum_lag', lag_num, '_substance_5to17_ed_rr.png')), 
       height = 8, width = 12 )

# Plot cumulative  percent change of ED visit for all temperatures compared to the median observed temperature
plot_cum_hr(lag_num = lag_num,
            mod = mod_substance_heat_risk, 
            outcome = 'ED visit',
            moa = 'pct_chg',
            temp_var = 'heat_risk', 
            estimates = estimates_substance_heat_risk,
            subset = 'patients with substance use as cause of ED visit ',
            with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'ed', 'cum', 'heat_risk', paste0('cum_lag', lag_num, '_substance_5to17_ed_pct_chg.png')), 
       height = 8, width = 12 )

## Create plot in environment without histogram for temperature
plot_substance_heat_risk <- plot_cum_hr(lag_num = lag_num,
                                        mod = mod_substance_heat_risk, 
                                        outcome = 'ED visit',
                                        moa = 'pct_chg',
                                        temp_var = 'heat_risk', 
                                        estimates = estimates_substance_heat_risk,
                                        subset = 'patients with substance use as cause of ED visit ',
                                        with_hist = FALSE
)

################################################################################-
## 1e: Plot cumulative risk, (mental) ----
################################################################################-

### Mean temperature ----

# Plot cumulative  relative risk of ED visit for all temperatures compared to the median observed temperature
plot_cum_tmean(lag_num = lag_num,
               mod = mod_mental_tmean, 
               outcome = 'ED visit',
               moa = 'rr',
               temp_var = 'tmean', 
               estimates = estimates_mental_tmean, 
               subset = 'patients with mental health as cause of ED visit ',
               with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'ed', 'cum', 'tmean', paste0('cum_lag', lag_num, '_mental_5to17_ed_rr.png')), 
       height = 8, width = 12 )

# Plot cumulative  percent change of ED visit for all temperatures compared to the median observed temperature
plot_cum_tmean(lag_num = lag_num,
               mod = mod_mental_tmean, 
               outcome = 'ED visit',
               moa = 'pct_chg',
               temp_var = 'tmean', 
               estimates = estimates_mental_tmean, 
               subset = 'patients with mental health as cause of ED visit ',
               with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'ed', 'cum', 'tmean', paste0('cum_lag', lag_num, '_mental_5to17_ed_pct_chg.png')), 
       height = 8, width = 12 )

## Create plot in environment without histogram for temperature
plot_mental_tmean <- plot_cum_tmean(lag_num = lag_num,
                                    mod = mod_mental_tmean, 
                                    outcome = 'ED visit',
                                    moa = 'pct_chg',
                                    temp_var = 'tmean', 
                                    estimates = estimates_mental_tmean, 
                                    subset = 'patients with mental health as cause of ED visit ',
                                    with_hist = FALSE
)

### HeatRisk ----

# Plot cumulative  relative risk of ED visit for all temperatures compared to the median observed temperature
plot_cum_hr(lag_num = lag_num,
            mod = mod_mental_heat_risk, 
            outcome = 'ED visit',
            moa = 'rr',
            temp_var = 'heat_risk', 
            estimates = estimates_mental_heat_risk,
            subset = 'patients with mental health as cause of ED visit ',
            with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'ed', 'cum', 'heat_risk', paste0('cum_lag', lag_num, '_mental_5to17_ed_rr.png')), 
       height = 8, width = 12 )

# Plot cumulative  percent change of ED visit for all temperatures compared to the median observed temperature
plot_cum_hr(lag_num = lag_num,
            mod = mod_mental_heat_risk, 
            outcome = 'ED visit',
            moa = 'pct_chg',
            temp_var = 'heat_risk', 
            estimates = estimates_mental_heat_risk,
            subset = 'patients with mental health as cause of ED visit ',
            with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'ed', 'cum', 'heat_risk', paste0('cum_lag', lag_num, '_mental_5to17_ed_pct_chg.png')), 
       height = 8, width = 12 )

## Create plot in environment without histogram for temperature
plot_mental_heat_risk <- plot_cum_hr(lag_num = lag_num,
                                     mod = mod_mental_heat_risk, 
                                     outcome = 'ED visit',
                                     moa = 'pct_chg',
                                     temp_var = 'heat_risk', 
                                     estimates = estimates_mental_heat_risk,
                                     subset = 'patients with mental health as cause of ED visit ',
                                     with_hist = FALSE
)

################################################################################-
## 1f: Plot cumulative risk, (genitourinary) ----
################################################################################-

### Mean temperature ----

# Plot cumulative  relative risk of ED visit for all temperatures compared to the median observed temperature
plot_cum_tmean(lag_num = lag_num,
               mod = mod_genitourinary_tmean, 
               outcome = 'ED visit',
               moa = 'rr',
               temp_var = 'tmean', 
               estimates = estimates_genitourinary_tmean, 
               subset = 'patients with genitourinary cause of ED visit ',
               with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'ed', 'cum', 'tmean', paste0('cum_lag', lag_num, '_genitourinary_5to17_ed_rr.png')), 
       height = 8, width = 12 )

# Plot cumulative  percent change of ED visit for all temperatures compared to the median observed temperature
plot_cum_tmean(lag_num = lag_num,
               mod = mod_genitourinary_tmean, 
               outcome = 'ED visit',
               moa = 'pct_chg',
               temp_var = 'tmean', 
               estimates = estimates_genitourinary_tmean, 
               subset = 'patients with genitourinary cause of ED visit ',
               with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'ed', 'cum', 'tmean', paste0('cum_lag', lag_num, '_genitourinary_5to17_ed_pct_chg.png')), 
       height = 8, width = 12 )

## Create plot in environment without histogram for temperature
plot_genitourinary_tmean <- plot_cum_tmean(lag_num = lag_num,
                                           mod = mod_genitourinary_tmean, 
                                           outcome = 'ED visit',
                                           moa = 'pct_chg',
                                           temp_var = 'tmean', 
                                           estimates = estimates_genitourinary_tmean, 
                                           subset = 'patients with genitourinary cause of ED visit ',
                                           with_hist = FALSE
)

### HeatRisk ----

# Plot cumulative  relative risk of ED visit for all temperatures compared to the median observed temperature
plot_cum_hr(lag_num = lag_num,
            mod = mod_genitourinary_heat_risk, 
            outcome = 'ED visit',
            moa = 'rr',
            temp_var = 'heat_risk', 
            estimates = estimates_genitourinary_heat_risk,
            subset = 'patients with genitourinary as cause of ED visit ',
            with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'ed', 'cum', 'heat_risk', paste0('cum_lag', lag_num, '_genitourinary_5to17_ed_rr.png')), 
       height = 8, width = 12 )

# Plot cumulative  percent change of ED visit for all temperatures compared to the median observed temperature
plot_cum_hr(lag_num = lag_num,
            mod = mod_genitourinary_heat_risk, 
            outcome = 'ED visit',
            moa = 'pct_chg',
            temp_var = 'heat_risk', 
            estimates = estimates_genitourinary_heat_risk,
            subset = 'patients with genitourinary cause of ED visit ',
            with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'ed', 'cum', 'heat_risk', paste0('cum_lag', lag_num, '_genitourinary_5to17_ed_pct_chg.png')), 
       height = 8, width = 12 )

## Create plot in environment without histogram for temperature
plot_genitourinary_heat_risk <- plot_cum_hr(lag_num = lag_num,
                                            mod = mod_genitourinary_heat_risk, 
                                            outcome = 'ED visit',
                                            moa = 'pct_chg',
                                            temp_var = 'heat_risk', 
                                            estimates = estimates_genitourinary_heat_risk,
                                            subset = 'patients with genitourinary as cause of ED visit ',
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
  outcome = 'ED visit',
  moa = 'pct_chg',
  temp_var = 'tmean',
  group = c('Cardiovascular', 'Respiratory', 'Unintentional injury',
            'Substance use', 'Mental health', 'Genitourinary'),
  estimates = list(estimates_cardiovascular_tmean, estimates_respiratory_tmean, 
                   estimates_unintentinjury_tmean,
                   estimates_substance_tmean, estimates_mental_tmean, estimates_genitourinary_tmean),
  subset_var = 'Cause',
  with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'ed', 'cum', 'tmean', paste0('cum_lag', lag_num, '_allcause_5to17_ed_together.png')), 
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
    labs(title = 'Cardiovascular',
         y = "% change") +
    theme(plot.margin = margin(t = .5, b = .5),
          plot.title = element_text(size = 16),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 12),
          axis.text.x = element_blank(),
          axis.text.y = element_text(size = 12),
          axis.ticks.x = element_blank()),
  plot_genitourinary_tmean + 
    coord_cartesian(ylim = c(ymin, ymax),
                    xlim = c(2, 34)) +
    scale_y_continuous(breaks = seq(ymin_scale, ymax_scale, by = 100)) +
    labs(title = 'Genitourinary',
         y = "% change") +
    theme(plot.margin = margin(t = .5, b = .5),
          plot.title = element_text(size = 16),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 12),
          axis.text.x = element_blank(),
          axis.text.y = element_text(size = 12),
          axis.ticks.x = element_blank()),
  plot_mental_tmean + 
    coord_cartesian(ylim = c(ymin, ymax),
                    xlim = c(2, 34)) +
    scale_y_continuous(breaks = seq(ymin_scale, ymax_scale, by = 100)) +
    labs(title = 'Mental health',
         y = "% change") +
    theme(plot.margin = margin(t = .5, b = .5),
          plot.title = element_text(size = 16),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 12),
          axis.text.x = element_blank(),
          axis.text.y = element_text(size = 12),
          axis.ticks.x = element_blank()),
  NULL,
  plot_respiratory_tmean + 
    coord_cartesian(ylim = c(ymin, ymax),
                    xlim = c(2, 34)) +
    scale_y_continuous(breaks = seq(ymin_scale, ymax_scale, by = 100))  +
    labs(title = 'Respiratory',
         y = "% change") +
    theme(plot.margin = margin(t = .5, b = .5),
          plot.title = element_text(size = 16),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 12),
          axis.text.x = element_blank(),
          axis.text.y = element_text(size = 12),
          axis.ticks.x = element_blank()),
  NULL,
  plot_substance_tmean + 
    coord_cartesian(ylim = c(ymin, ymax),
                    xlim = c(2, 34)) +
    scale_y_continuous(breaks = seq(ymin_scale, ymax_scale, by = 100)) +
    labs(title = 'Substance use',
         y = "% change") +
    theme(plot.margin = margin(t = .5, b = .5),
          plot.title = element_text(size = 16),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 12),
          axis.text.x = element_blank(),
          axis.text.y = element_text(size = 12),
          axis.ticks.x = element_blank()),
  plot_unintentinjury_tmean + 
    coord_cartesian(ylim = c(ymin, ymax),
                    xlim = c(2, 34)) +
    scale_y_continuous(breaks = seq(ymin_scale, ymax_scale, by = 100)) +
    labs(title = 'Unintentional injury',
         y = "% change") +
    theme(plot.margin = margin(t = .5, b = .5),
          plot.title = element_text(size = 16),
          axis.title.y = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.text.x = element_text(size = 12),
          axis.title.x = element_text(size = 12)),
  nrow = 8,
  align = 'hv',
  axis = 'tblr'
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'ed', 'cum', 'tmean', paste0('cum_lag', lag_num, '_allcauses_5to17_ed.png')), 
       height = 12, width = 4.5 )

### HeatRisk ----

#### All on same figure ----
plot_cum_hr_together(
  lag_num = lag_num,
  mod = mod_unintentinjury_heat_risk,
  outcome = 'ED visit',
  moa = 'pct_chg',
  temp_var = 'heat_risk',
  group = c('Cardiovascular', 'Respiratory', 'Unintentional injury', 
            'Substance use', 'Mental health', 'Genitourinary'),
  estimates = list(estimates_cardiovascular_heat_risk, estimates_respiratory_heat_risk, 
                   estimates_unintentinjury_heat_risk,
                   estimates_substance_heat_risk, estimates_mental_heat_risk, 
                  estimates_genitourinary_heat_risk), subset_var = 'Cause',
  with_hist = TRUE
)

## Save plot with histogram for temperature
ggsave(filename = here::here('outputs', 'plots', 'ed', 'cum', 'heat_risk', paste0('cum_lag', lag_num, '_allcause_5to17_ed_together.png')), 
       height = 8, width = 12 )


################################################################################-
## 3: Clean up ----
################################################################################-

rm(data_source_name,
   selected_constraints_ed, cause_grid,
   estimates_cardiovascular_tmean, estimates_cardiovascular_heat_risk,
   estimates_respiratory_tmean, estimates_respiratory_heat_risk,
   estimates_unintentinjury_tmean, estimates_unintentinjury_heat_risk,
   estimates_substance_tmean, estimates_substance_heat_risk,
   estimates_mental_tmean, estimates_mental_heat_risk,
   estimates_genitourinary_tmean, estimates_genitourinary_heat_risk,
   mod_cardiovascular_tmean, mod_cardiovascular_heat_risk,
   mod_respiratory_tmean, mod_respiratory_heat_risk,
   mod_unintentinjury_tmean, mod_unintentinjury_heat_risk,
   mod_substance_tmean, mod_substance_heat_risk,
   mod_mental_tmean, mod_mental_heat_risk,
   mod_genitourinary_tmean, mod_genitourinary_heat_risk,
   plot_cardiovascular_tmean, plot_cardiovascular_heat_risk,
   plot_respiratory_tmean, plot_respiratory_heat_risk,
   plot_unintentinjury_tmean, plot_unintentinjury_heat_risk,
   plot_substance_tmean, plot_substance_heat_risk,
   plot_mental_tmean, plot_mental_heat_risk,
   plot_genitourinary_tmean, plot_genitourinary_heat_risk,
   ymin, ymax, ymin_scale, ymax_scale
)
