################################################################################-
# Fit models for supplement comparison ----
# Comparing HeatRisk and temperature for local heat surveillance: A time-stratified 
#  case-crossover study of emergency department visits in San Mateo County, 
#  California, 2017–2025 
# Juliana GE Bartels
# Updated: 6/24/2026
################################################################################-

# This file plots all exposure-response functions together to show 
# how they compare for a supplemental figure.

################################################################################-
## 0: Upload data ----
################################################################################-

# Load dataset of deaths and ED visits matched with temperature exposures
if(data_source == 'original') {
  dta_full <- read_csv(here::here(
    'data', 'prepared',
    'death_ed_climate_df.csv'
  )) %>%
    mutate(stratum = as.factor(stratum))
}

if(data_source == 'synth') {
  dta_full <- read_csv(here::here(
    'data', 'prepared',
    'death_ed_climate_df_synth.csv'
  )) %>%
    mutate(stratum = as.factor(stratum))
}

# Create grid of model constraints for mean temperature
models_grid <- expand_grid(
  er_constraint = c('a', 'b', 'c', 'd', 'e'),
  er_type = c('ns'),
  lr_type = c('ns'),
  temp_var = c('tmean')
) %>%
  rbind(., 
        expand_grid(
          er_constraint = c('a'),
          er_type = c('quad', 'cub', 'quar'),
          lr_type = c('ns'),
          temp_var = c('tmean'))
  )

################################################################################-
## 1a: Plot all model ERs on same figure - ED visits ----
################################################################################-

# Generate suffix for file calling and writing, based on data source
data_source_name <- case_when(data_source == 'original' ~ '',
                              data_source == 'synth' ~ '_synth')

lag_num <- 6

load_estimates_models_supp_fig(models_grid, 'ed', 'cum', data_source_name)

estimates <- list(
  a_ns_ns = est_ed_main_full_full_a_ns_ns, 
  b_ns_ns = est_ed_main_full_full_b_ns_ns,
  c_ns_ns = est_ed_main_full_full_c_ns_ns,
  d_ns_ns = est_ed_main_full_full_d_ns_ns,
  e_ns_ns = est_ed_main_full_full_e_ns_ns,
  a_quad_ns = est_ed_main_full_full_a_quad_ns,
  a_cub_ns = est_ed_main_full_full_a_cub_ns,
  a_quar_ns = est_ed_main_full_full_a_quar_ns)

# Calculate estimates by lag for percent change
all_estimates <- tibble()

for(i in 1:length(estimates)){
  estimates_bylag <- estimates[[i]] %>%
    filter(label == 'er_values') %>% 
    dplyr::select(counterfactual_temp, paste0('fit_rr_lag', lag_num), paste0('lci_rr_lag', lag_num), paste0('uci_rr_lag', lag_num)) %>%
    rename(fit_rr_lag = paste0('fit_rr_lag', lag_num),
           lci_rr_lag = paste0('lci_rr_lag', lag_num),
           uci_rr_lag = paste0('uci_rr_lag', lag_num)) %>%
    mutate(fit_rr_lag = (fit_rr_lag - 1) * 100,
           lci_rr_lag = (lci_rr_lag - 1) * 100,
           uci_rr_lag = (uci_rr_lag - 1) * 100,
           model = names(estimates)[i])
  
  all_estimates <- rbind(all_estimates, estimates_bylag)
}

# Calculate median, 5th percentile, and 95th percentile temp
median_temp <- median(dta_full$tmean)
perc_5_temp <- quantile(dta_full$tmean, 0.05)
perc_95_temp <- quantile(dta_full$tmean, 0.95)

# Calculate minimum temperature and maximum temperature
xmin_temp <- floor(min(dta_full$tmean))
xmax_temp <- ceiling(max(dta_full$tmean))

# Graph cumulative IRR
cum_pct_chg <- all_estimates %>%
  mutate(model = factor(model, levels = c('a_ns_ns', 'b_ns_ns', 'c_ns_ns', 'd_ns_ns', 'e_ns_ns', 'a_quad_ns', 'a_cub_ns', 'a_quar_ns'))) %>%
  ggplot(aes(x = counterfactual_temp, y = fit_rr_lag, group = model)) +
  geom_line(data = ~ subset(.x, model != 'a_quad_ns'),
                          aes(color = model), linewidth = 1) +
  geom_line(data = ~ subset(.x, model == 'a_quad_ns'),
            aes(color = model), linewidth = 1, linetype = 'dashed') +
  geom_hline(aes(yintercept = 0), color = "black", linetype = "dashed") +
  geom_vline(aes(xintercept = median_temp), color = "black", alpha = 0.8) +
  geom_vline(aes(xintercept = perc_5_temp), color = "black", linetype = "dotted") +
  geom_vline(aes(xintercept = perc_95_temp), color = "black", linetype = "dotted") +
  scale_color_manual(values = c("a_ns_ns" = "#325771",
                                "b_ns_ns" = "#4c918f",
                                "c_ns_ns" = "#f8bd4f",
                                "d_ns_ns" = "#fd6735",
                                "e_ns_ns" = "#e42538",
                                "a_quad_ns" = "black",
                                "a_cub_ns" = "#decfd0",
                                "a_quar_ns" = "#D0F0C0"
                                ),
                     labels = c('a_ns_ns' = 'Natural spline - 10th, 75th, 90th percentile',
                                'b_ns_ns' = 'Natural spline - 10th, 50th, 90th percentile',
                                'c_ns_ns' = 'Natural spline - 5th, 50th, 95th percentile',
                                'd_ns_ns' = 'Natural spline - 25th, 50th, 75th percentile',
                                'e_ns_ns' = 'Natural spline - 33th, 50th, 67th percentile',
                                'a_quad_ns' = 'Quadratic',
                                'a_cub_ns' = 'Cubic',
                                'a_quar_ns' = 'Quartic')) +
  scale_x_continuous(breaks = seq(xmin_temp, xmax_temp, by = 4)) +
  labs(x = expression("Mean daily temperature (" * degree * "C)"),
       y =  "Percent change",
       title = paste0("Cumulative percent change in ED visits compared to the median temperature\nafter ", 
                      lag_num, " days lag, among residents of San Mateo County, CA"),
       color = "Model exposure-response constraint"
  ) +
  theme_classic() +
  theme(axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.key.width = unit(1, "cm"))

ggsave(filename = here::here('outputs', 'plots', 'ed', 'cum', 'tmean', paste0('supp_cum_lag', lag_num, '_full_ed_pct_chg_all_ers', data_source_name, '.png')), 
       height = 8, width = 12 )

################################################################################-
## 2b: Plot all model ERs on same figure - deaths ----
################################################################################-

# Generate suffix for file calling and writing, based on data source
data_source_name <- case_when(data_source == 'original' ~ '',
                              data_source == 'synth' ~ '_synth')

lag_num <- 6

load_estimates_models_supp_fig(models_grid, 'death', 'cum', data_source_name)

estimates <- list(
  a_ns_ns = est_death_main_full_full_a_ns_ns, 
  b_ns_ns = est_death_main_full_full_b_ns_ns,
  c_ns_ns = est_death_main_full_full_c_ns_ns,
  d_ns_ns = est_death_main_full_full_d_ns_ns,
  e_ns_ns = est_death_main_full_full_e_ns_ns,
  a_quad_ns = est_death_main_full_full_a_quad_ns,
  a_cub_ns = est_death_main_full_full_a_cub_ns,
  a_quar_ns = est_death_main_full_full_a_quar_ns)

# Calculate estimates by lag for percent change
all_estimates <- tibble()

for(i in 1:length(estimates)){
  estimates_bylag <- estimates[[i]] %>%
    filter(label == 'er_values') %>% 
    dplyr::select(counterfactual_temp, paste0('fit_rr_lag', lag_num), paste0('lci_rr_lag', lag_num), paste0('uci_rr_lag', lag_num)) %>%
    rename(fit_rr_lag = paste0('fit_rr_lag', lag_num),
           lci_rr_lag = paste0('lci_rr_lag', lag_num),
           uci_rr_lag = paste0('uci_rr_lag', lag_num)) %>%
    mutate(fit_rr_lag = (fit_rr_lag - 1) * 100,
           lci_rr_lag = (lci_rr_lag - 1) * 100,
           uci_rr_lag = (uci_rr_lag - 1) * 100,
           model = names(estimates)[i])
  
  all_estimates <- rbind(all_estimates, estimates_bylag)
}

# Calculate median, 5th percentile, and 95th percentile temp
median_temp <- median(dta_full$tmean)
perc_5_temp <- quantile(dta_full$tmean, 0.05)
perc_95_temp <- quantile(dta_full$tmean, 0.95)

# Calculate minimum temperature and maximum temperature
xmin_temp <- floor(min(dta_full$tmean))
xmax_temp <- ceiling(max(dta_full$tmean))

# Graph cumulative IRR
cum_pct_chg <- all_estimates %>%
  mutate(model = factor(model, c('a_ns_ns', 'b_ns_ns', 'c_ns_ns', 'd_ns_ns', 'e_ns_ns', 'a_quad_ns', 'a_cub_ns', 'a_quar_ns'))) %>%
  ggplot(aes(x = counterfactual_temp, y = fit_rr_lag, group = model)) +
  geom_line(data = ~ subset(.x, model != 'a_quad_ns'),
            aes(color = model), linewidth = 1) +
  geom_line(data = ~ subset(.x, model == 'a_quad_ns'),
            aes(color = model), linewidth = 1, linetype = 'dashed') +
  geom_hline(aes(yintercept = 0), color = "black", linetype = "dashed") +
  geom_vline(aes(xintercept = median_temp), color = "black", alpha = 0.8) +
  geom_vline(aes(xintercept = perc_5_temp), color = "black", linetype = "dotted") +
  geom_vline(aes(xintercept = perc_95_temp), color = "black", linetype = "dotted") +
  scale_color_manual(values = c("a_ns_ns" = "#325771",
                                "b_ns_ns" = "#4c918f",
                                "c_ns_ns" = "#f8bd4f",
                                "d_ns_ns" = "#fd6735",
                                "e_ns_ns" = "#e42538",
                                "a_quad_ns" = "black",
                                "a_cub_ns" = "#decfd0",
                                "a_quar_ns" = "#D0F0C0"
  ),
  labels = c('a_ns_ns' = 'Natural spline - 10th, 75th, 90th percentile',
             'b_ns_ns' = 'Natural spline - 10th, 50th, 90th percentile',
             'c_ns_ns' = 'Natural spline - 5th, 50th, 95th percentile',
             'd_ns_ns' = 'Natural spline - 25th, 50th, 75th percentile',
             'e_ns_ns' = 'Natural spline - 33th, 50th, 67th percentile',
             'a_quad_ns' = 'Quadratic',
             'a_cub_ns' = 'Cubic',
             'a_quar_ns' = 'Quartic')) +
  scale_x_continuous(breaks = seq(xmin_temp, xmax_temp, by = 4)) +
  labs(x = expression("Mean daily temperature (" * degree * "C)"),
       y =  "Percent change",
       title = paste0("Cumulative percent change in deaths compared to the median temperature\nafter ", 
                      lag_num, " days lag, among residents of San Mateo County, CA"),
       color = "Model exposure-response constraint"
  ) +
  theme_classic() +
  theme(axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.key.width = unit(1, "cm"))

ggsave(filename = here::here('outputs', 'plots', 'death', 'cum', 'tmean', paste0('supp_cum_lag', lag_num, '_full_death_pct_chg_all_ers', data_source_name, '.png')), 
       height = 8, width = 12 )

################################################################################-
## 3: Clean up ----
################################################################################-

rm(dta_full, all_estimates, 
   est_death_main_full_full_a_ns_ns, est_death_main_full_full_b_ns_ns,
   est_death_main_full_full_c_ns_ns, est_death_main_full_full_d_ns_ns, 
   est_death_main_full_full_e_ns_ns, est_death_main_full_full_a_quad_ns, 
   est_death_main_full_full_a_cub_ns, est_death_main_full_full_a_quar_ns,
   est_ed_main_full_full_a_ns_ns, est_ed_main_full_full_b_ns_ns,
   est_ed_main_full_full_c_ns_ns, est_ed_main_full_full_d_ns_ns, 
   est_ed_main_full_full_e_ns_ns, est_ed_main_full_full_a_quad_ns, 
   est_ed_main_full_full_a_cub_ns, est_ed_main_full_full_a_quar_ns,
   cum_pct_chg,
   xmin_temp, xmax_temp, median_temp, perc_5_temp, perc_95_temp, lag_num, 
   estimates, data_source_name, models_grid, estimates_bylag)
