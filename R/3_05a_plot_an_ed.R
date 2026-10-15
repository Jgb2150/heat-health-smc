################################################################################-
# Plot: Attributable fraction and number of ED visits by age and region after 6 days lag ---
# Comparing HeatRisk and temperature for local heat surveillance: A time-stratified 
#  case-crossover study of emergency department visits in San Mateo County, 
#  California, 2017–2025
# Juliana GE Bartels
# Updated: 9/29/2026
################################################################################-

# This file plots the attributable fraction and number of ED visits by age and region 
# for cumulative 6 days lag. 

################################################################################-
## 0: Upload data ----
################################################################################-

# Generate suffix for file calling and writing, based on data source
data_source_name <- case_when(data_source == 'original' ~ '',
                              data_source == 'synth' ~ '_synth')

# Load selected constraints 
selected_constraints_ed <- read_csv(here::here('outputs', 'tables', 'selected_constraints_ed.csv') ) %>%
  filter(sensitivity == 'main')

# Load estimates and models for EMMs
an_calc_estimates <- read_csv(here::here('outputs', 'estimates', paste0('an_calc_estimates', data_source_name, '.csv'))) %>%
  filter(outcome == 'ed' & str_detect(model_name, 'main')) %>%
  filter(
    (temp_var == 'tmean' & str_detect(model_name, paste0(selected_constraints_ed[2, 4], '_', selected_constraints_ed[2, 5], '_', selected_constraints_ed[2,6])))|
      (temp_var == 'heat_risk' & str_detect(model_name, paste0(selected_constraints_ed[1, 4], '_', selected_constraints_ed[1, 5], '_', selected_constraints_ed[1,6])))
  )

################################################################################-
## 1: Plot attributable fraction of ED visits, cumulative 6 days lag (sex) ----
################################################################################-

### HeatRisk ----

# Bar graph with heat and extreme heat
an_calc_estimates %>%
  filter((subset_var == 'sex' | is.na(subset_var)) & temp_var == 'heat_risk') %>%
  mutate(subset = case_when(is.na(subset) ~ 'Overall',
                            TRUE ~ subset),
         subset = factor(subset, levels = c('Overall', 'f', 'm'))) %>%
  pivot_longer(
    cols = an_all_est:af_ext_cold_uci,
    names_to = c('type', 'category', 'measure'),
    names_pattern = '^([^_]+)_(.*)_([^_]+)$',
    values_to = 'estimate'
  ) %>%
  pivot_wider(
    names_from = measure,
    values_from = estimate
  ) %>% 
  filter(type == 'af' & (category == 'heat' | category == 'ext_heat')) %>%
  mutate(category = factor(category, levels = c('heat', 'ext_heat'))) %>% 
  ggplot() +
  geom_col(aes(x = subset, y = med, group = category, fill = category), position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(x = subset, ymin = lci, ymax = uci, group = category), position = position_dodge(width = 0.9), width = 0.25) +
  labs(x = 'Sex', y = 'Percent attributable (%)') +
  scale_x_discrete(labels = c(
    'Overall' = 'Overall',
    'f' = 'Female', 
    'm' = 'Male')) +
  scale_fill_manual(
    name = 'Exposure',
    values = c('heat' = '#f69632', 'ext_heat' = '#e22f33'),
    labels = c('heat' = 'Heat', 'ext_heat' = 'Extreme Heat')
  ) +
  geom_hline(aes(yintercept = 0), color = 'black') +
  geom_vline(xintercept = 1.5, linetype = 'dotted', color = 'black') +
  theme_classic() +
  theme(legend.position = 'inside',
        legend.position.inside = c(0.15, 0.9),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size = 20)
  )

ggsave(filename = here::here('outputs', 'plots', 'ed', 'an', 'heat_risk', paste0('af_lag6_heat_sex_ed_bar', data_source_name, '.png')), 
       height = 8, width = 12 )

### Mean temperature ----

an_calc_estimates %>%
  filter((subset_var == 'sex' | is.na(subset_var)) & temp_var == 'tmean') %>%
  mutate(subset = case_when(is.na(subset) ~ 'Overall',
                            TRUE ~ subset),
         subset = factor(subset, levels = c('Overall', 'f', 'm'))) %>%
  pivot_longer(
    cols = an_all_est:af_ext_cold_uci,
    names_to = c('type', 'category', 'measure'),
    names_pattern = '^([^_]+)_(.*)_([^_]+)$',
    values_to = 'estimate'
  ) %>%
  pivot_wider(
    names_from = measure,
    values_from = estimate
  ) %>% 
  filter(type == 'af' & (category == 'heat' | category == 'ext_heat')) %>%
  mutate(category = factor(category, levels = c('heat', 'ext_heat'))) %>%
  ggplot() +
  geom_col(aes(x = subset, y = med, group = category, fill = category), position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(x = subset, ymin = lci, ymax = uci, group = category), position = position_dodge(width = 0.9), width = 0.25) +
  labs(x = 'Sex', y = 'Percent attributable (%)') +
  scale_x_discrete(labels = c(
    'Overall' = 'Overall',
    'f' = 'Female', 
    'm' = 'Male')) +
  scale_fill_manual(
    name = 'Exposure',
    values = c('heat' = 'pink', 'ext_heat' = 'red'),
    labels = c('heat' = 'Heat', 'ext_heat' = 'Extreme Heat')
  ) +
  geom_hline(aes(yintercept = 0), color = 'black') +
  geom_vline(xintercept = 1.5, linetype = 'dotted', color = 'black') +
  theme_classic() +
  theme(legend.position = 'inside',
        legend.position.inside = c(0.1, 0.9),
        legend.title = element_text(size = 16),         
        legend.text = element_text(size = 16),
        axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size = 20)
  )

ggsave(filename = here::here('outputs', 'plots', 'ed', 'an', 'tmean', paste0('af_lag6_heat_sex_ed_bar', data_source_name, '.png')), 
       height = 8, width = 12 )

an_calc_estimates %>%
  filter((subset_var == 'sex' | is.na(subset_var)) & temp_var == 'tmean') %>%
  mutate(subset = case_when(is.na(subset) ~ 'Overall',
                            TRUE ~ subset),
         subset = factor(subset, levels = c('Overall', 'f', 'm'))) %>%
  pivot_longer(
    cols = an_all_est:af_ext_cold_uci,
    names_to = c('type', 'category', 'measure'),
    names_pattern = '^([^_]+)_(.*)_([^_]+)$',
    values_to = 'estimate'
  ) %>%
  pivot_wider(
    names_from = measure,
    values_from = estimate
  ) %>% 
  filter(type == 'af' & category != 'all') %>%
  mutate(category = factor(category, levels = c('ext_cold', 'cold', 'heat', 'ext_heat'))) %>%
  ggplot() +
  geom_col(aes(x = subset, y = med, group = category, fill = category), position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(x = subset, ymin = lci, ymax = uci, group = category), position = position_dodge(width = 0.9), width = 0.25) +
  labs(x = 'Sex', y = 'Percent attributable (%)') +
  scale_y_continuous(labels = scales::label_percent()) +
  scale_x_discrete(labels = c(
    'Overall' = 'Overall',
    'f' = 'Female', 
    'm' = 'Male')) +
  scale_fill_manual(
    name = 'Exposure',
    values = c('ext_cold' = 'blue', 'cold' = 'lightblue', 'heat' = 'pink', 'ext_heat' = 'red'),
    labels = c('ext_cold' = 'Extreme Cold', 'cold' = 'Cold', 'heat' = 'Heat', 'ext_heat' = 'Extreme Heat')
  ) +
  geom_hline(aes(yintercept = 0), color = 'black') +
  geom_vline(xintercept = 1.5, linetype = 'dotted', color = 'black') +
  theme_classic() +
  theme(legend.position = 'inside',
        legend.position.inside = c(0.1, 0.9),
        legend.title = element_text(size = 16),         
        legend.text = element_text(size = 16),
        axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size = 20)
  )

ggsave(filename = here::here('outputs', 'plots', 'ed', 'an', 'tmean', paste0('af_lag6_heatcold_sex_ed_bar', data_source_name, '.png')), 
       height = 8, width = 12 )

################################################################################-
## Run code below if data source is original
################################################################################-

if(data_source == 'original'){
  
  ################################################################################-
  ## 2: Plot attributable fraction of ED visits, cumulative 6 days lag (age group) ----
  ################################################################################-
  
  ### HeatRisk ----
  
  # Bar graph with heat and extreme heat - HeatRisk
  an_calc_estimates %>%
    filter((subset_var == 'age' | is.na(subset_var)) & temp_var == 'heat_risk') %>%
    mutate(subset = case_when(is.na(subset) ~ 'Overall',
                              TRUE ~ subset),
           subset = factor(subset, levels = c('Overall', 'under5', '5to17', '18to64', '65plus'))) %>%
    pivot_longer(
      cols = an_all_est:af_ext_cold_uci,
      names_to = c('type', 'category', 'measure'),
      names_pattern = '^([^_]+)_(.*)_([^_]+)$',
      values_to = 'estimate'
    ) %>%
    pivot_wider(
      names_from = measure,
      values_from = estimate
    ) %>%
    filter(type == 'af' & (category == 'heat' | category == 'ext_heat')) %>%
    mutate(category = factor(category, levels = c('heat', 'ext_heat'))) %>%
    ggplot() +
    geom_col(aes(x = subset, y = med, group = category, fill = category), position = position_dodge(width = 0.9)) +
    geom_errorbar(aes(x = subset, ymin = lci, ymax = uci, group = category), position = position_dodge(width = 0.9), width = 0.25) +
    geom_hline(aes(yintercept = 0), color = 'black') +
    labs(x = 'Age group', y = 'Percent attributable (%)') +
    scale_x_discrete(labels = c(
      'Overall' = 'Overall',
      'under5' = '< 5', 
      '5to17' = '5-17', 
      '18to64' = '18-64', 
      '65plus' = '\u2265 65')) +
    scale_fill_manual(
      name = 'Exposure',
      values = c('heat' = '#f69632', 'ext_heat' = '#e22f33'),
      labels = c('heat' = 'Heat', 'ext_heat' = 'Extreme Heat')
    ) +
    geom_vline(xintercept = 1.5, linetype = 'dotted', color = 'black') +
    theme_classic() +
    theme(legend.position = 'inside',
          legend.position.inside = c(0.1, 0.9),
          legend.title = element_text(size = 16),         
          legend.text = element_text(size = 16),
          axis.title.x = element_text(size = 20),
          axis.text.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          axis.text.y = element_text(size = 20)
    )
  
  ggsave(filename = here::here('outputs', 'plots', 'ed', 'an', 'heat_risk', paste0('af_lag6_heat_age_ed_bar', data_source_name, '.png')), 
         height = 8, width = 12 )
  
  ### Mean temperature ----
  
  an_calc_estimates %>%
    filter((subset_var == 'age' | is.na(subset_var)) & temp_var == 'tmean') %>%
    mutate(subset = case_when(is.na(subset) ~ 'Overall',
                              TRUE ~ subset),
           subset = factor(subset, levels = c('Overall', 'under5', '5to17', '18to64', '65plus'))) %>%
    pivot_longer(
      cols = an_all_est:af_ext_cold_uci,
      names_to = c('type', 'category', 'measure'),
      names_pattern = '^([^_]+)_(.*)_([^_]+)$',
      values_to = 'estimate'
    ) %>%
    pivot_wider(
      names_from = measure,
      values_from = estimate
    ) %>% 
    filter(type == 'af' & (category == 'heat' | category == 'ext_heat')) %>%
    mutate(category = factor(category, levels = c('heat', 'ext_heat'))) %>%
    ggplot() +
    geom_col(aes(x = subset, y = med, group = category, fill = category), position = position_dodge(width = 0.9)) +
    geom_errorbar(aes(x = subset, ymin = lci, ymax = uci, group = category), position = position_dodge(width = 0.9), width = 0.25) +
    labs(x = 'Age group', y = 'Percent attributable (%)') +
    scale_x_discrete(labels = c(
      'Overall' = 'Overall',
      'under5' = '< 5', 
      '5to17' = '5-17', 
      '18to64' = '18-64', 
      '65plus' = '\u2265 65')) +
    scale_fill_manual(
      name = 'Exposure',
      values = c('heat' = 'pink', 'ext_heat' = 'red'),
      labels = c('heat' = 'Heat', 'ext_heat' = 'Extreme Heat')
    ) +
    geom_hline(aes(yintercept = 0), color = 'black') +
    geom_vline(xintercept = 1.5, linetype = 'dotted', color = 'black') +
    theme_classic() +
    theme(legend.position = 'inside',
          legend.position.inside = c(0.1, 0.8),
          legend.title = element_text(size = 16),         
          legend.text = element_text(size = 16),
          axis.title.x = element_text(size = 20),
          axis.text.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          axis.text.y = element_text(size = 20)
    ) 
  
  ggsave(filename = here::here('outputs', 'plots', 'ed', 'an', 'tmean', paste0('af_lag6_heat_age_ed_bar', data_source_name, '.png')), 
         height = 8, width = 12 )
  
  an_calc_estimates %>%
    filter((subset_var == 'age' | is.na(subset_var)) & temp_var == 'tmean') %>%
    mutate(subset = case_when(is.na(subset) ~ 'Overall',
                              TRUE ~ subset),
           subset = factor(subset, levels = c('Overall', 'under5', '5to17', '18to64', '65plus'))) %>%
    pivot_longer(
      cols = an_all_est:af_ext_cold_uci,
      names_to = c('type', 'category', 'measure'),
      names_pattern = '^([^_]+)_(.*)_([^_]+)$',
      values_to = 'estimate'
    ) %>%
    pivot_wider(
      names_from = measure,
      values_from = estimate
    ) %>% 
    filter(type == 'af' & category != 'all') %>%
    mutate(category = factor(category, levels = c('ext_cold', 'cold', 'heat', 'ext_heat'))) %>%
    ggplot() +
    geom_col(aes(x = subset, y = med, group = category, fill = category), position = position_dodge(width = 0.9)) +
    geom_errorbar(aes(x = subset, ymin = lci, ymax = uci, group = category), position = position_dodge(width = 0.9), width = 0.25) +
    labs(x = 'Age group', y = 'Percent attributable (%)') +
    scale_x_discrete(labels = c(
      'Overall' = 'Overall',
      'under5' = '< 5', 
      '5to17' = '5-17', 
      '18to64' = '18-64', 
      '65plus' = '\u2265 65')) +
    scale_fill_manual(
      name = 'Exposure',
      values = c('ext_cold' = 'blue', 'cold' = 'lightblue', 'heat' = 'pink', 'ext_heat' = 'red'),
      labels = c('ext_cold' = 'Extreme Cold', 'cold' = 'Cold', 'heat' = 'Heat', 'ext_heat' = 'Extreme Heat')
    ) +
    geom_hline(aes(yintercept = 0), color = 'black') +
    geom_vline(xintercept = 1.5, linetype = 'dotted', color = 'black') +
    theme_classic() +
    theme(legend.position = 'inside',
          legend.position.inside = c(0.1, 0.3),
          legend.title = element_text(size = 16),         
          legend.text = element_text(size = 16),
          axis.title.x = element_text(size = 20),
          axis.text.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          axis.text.y = element_text(size = 20)
    )
  
  ggsave(filename = here::here('outputs', 'plots', 'ed', 'an', 'tmean', paste0('af_lag6_heatcold_age_ed_bar', data_source_name, '.png')), 
         height = 8, width = 12 )
  
  ################################################################################-
  ## 3: Plot attributable fraction of ED visits, cumulative 6 days lag (region) ----
  ################################################################################-
  
  ### HeatRisk ----
  
  # Bar graph with heat and extreme heat
  an_calc_estimates %>%
    filter((subset_var == 'region' | is.na(subset_var)) & temp_var == 'heat_risk') %>%
    mutate(subset = case_when(is.na(subset) ~ 'Overall',
                              TRUE ~ subset),
           subset = factor(subset, levels = c('Overall', 'north', 'mid', 'south', 'coastside'))) %>%
    pivot_longer(
      cols = an_all_est:af_ext_cold_uci,
      names_to = c('type', 'category', 'measure'),
      names_pattern = '^([^_]+)_(.*)_([^_]+)$',
      values_to = 'estimate'
    ) %>%
    pivot_wider(
      names_from = measure,
      values_from = estimate
    ) %>%
    filter(type == 'af' & (category == 'heat' | category == 'ext_heat')) %>%
    mutate(category = factor(category, levels = c('heat', 'ext_heat'))) %>%
    ggplot() +
    geom_col(aes(x = subset, y = med, group = category, fill = category), position = position_dodge(width = 0.9)) +
    geom_errorbar(aes(x = subset, ymin = lci, ymax = uci, group = category), position = position_dodge(width = 0.9), width = 0.25) +
    labs(x = 'Region', y = 'Percent attributable (%)') +
    scale_x_discrete(labels = c(
      'Overall' = 'Overall',
      'coastside' = 'Coastside', 
      'mid' = 'Mid-county', 
      'north' = 'North county', 
      'south' = 'South county')) +
    scale_fill_manual(
      name = 'Exposure',
      values = c('heat' = '#f69632', 'ext_heat' = '#e22f33'),
      labels = c('heat' = 'Heat', 'ext_heat' = 'Extreme Heat')
    ) +
    geom_hline(aes(yintercept = 0), color = 'black') +
    geom_vline(xintercept = 1.5, linetype = 'dotted', color = 'black') +
    theme_classic() +
    theme(legend.position = 'inside',
          legend.position.inside = c(0.15, 0.9),
          legend.title = element_text(size = 16),         
          legend.text = element_text(size = 16),
          axis.title.x = element_text(size = 20),
          axis.text.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          axis.text.y = element_text(size = 20)
    )
  
  ggsave(filename = here::here('outputs', 'plots', 'ed', 'an', 'heat_risk', paste0('af_lag6_heat_region_ed_bar', data_source_name, '.png')), 
         height = 8, width = 12 )
  
  ### Mean temperature ----
  
  an_calc_estimates %>%
    filter((subset_var == 'region' | is.na(subset_var)) & temp_var == 'tmean') %>%
    mutate(subset = case_when(is.na(subset) ~ 'Overall',
                              TRUE ~ subset),
           subset = factor(subset, levels = c('Overall', 'north', 'mid', 'south', 'coastside'))) %>%
    pivot_longer(
      cols = an_all_est:af_ext_cold_uci,
      names_to = c('type', 'category', 'measure'),
      names_pattern = '^([^_]+)_(.*)_([^_]+)$',
      values_to = 'estimate'
    ) %>%
    pivot_wider(
      names_from = measure,
      values_from = estimate
    ) %>% 
    filter(type == 'af' & (category == 'heat' | category == 'ext_heat')) %>%
    mutate(category = factor(category, levels = c('heat', 'ext_heat'))) %>%
    ggplot() +
    geom_col(aes(x = subset, y = med, group = category, fill = category), position = position_dodge(width = 0.9)) +
    geom_errorbar(aes(x = subset, ymin = lci, ymax = uci, group = category), position = position_dodge(width = 0.9), width = 0.25) +
    labs(x = 'Region', y = 'Percent attributable (%)') +
    scale_x_discrete(labels = c(
      'Overall' = 'Overall',
      'coastside' = 'Coastside', 
      'mid' = 'Mid-county', 
      'north' = 'North county', 
      'south' = 'South county')) +
    scale_fill_manual(
      name = 'Exposure',
      values = c('heat' = 'pink', 'ext_heat' = 'red'),
      labels = c('heat' = 'Heat', 'ext_heat' = 'Extreme Heat')
    ) +
    geom_hline(aes(yintercept = 0), color = 'black') +
    geom_vline(xintercept = 1.5, linetype = 'dotted', color = 'black') +
    theme_classic() +
    theme(legend.position = 'inside',
          legend.position.inside = c(0.1, 0.8),
          legend.title = element_text(size = 16),         
          legend.text = element_text(size = 16),
          axis.title.x = element_text(size = 20),
          axis.text.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          axis.text.y = element_text(size = 20)
    ) 
  
  ggsave(filename = here::here('outputs', 'plots', 'ed', 'an', 'tmean', paste0('af_lag6_heat_region_ed_bar', data_source_name, '.png')), 
         height = 8, width = 12 )
  
  an_calc_estimates %>%
    filter((subset_var == 'region' | is.na(subset_var)) & temp_var == 'tmean') %>%
    mutate(subset = case_when(is.na(subset) ~ 'Overall',
                              TRUE ~ subset),
           subset = factor(subset, levels = c('Overall', 'north', 'mid', 'south', 'coastside'))) %>%
    pivot_longer(
      cols = an_all_est:af_ext_cold_uci,
      names_to = c('type', 'category', 'measure'),
      names_pattern = '^([^_]+)_(.*)_([^_]+)$',
      values_to = 'estimate'
    ) %>%
    pivot_wider(
      names_from = measure,
      values_from = estimate
    ) %>% 
    filter(type == 'af' & category != 'all') %>%
    mutate(category = factor(category, levels = c('ext_cold', 'cold', 'heat', 'ext_heat'))) %>%
    ggplot() +
    geom_col(aes(x = subset, y = med, group = category, fill = category), position = position_dodge(width = 0.9)) +
    geom_errorbar(aes(x = subset, ymin = lci, ymax = uci, group = category), position = position_dodge(width = 0.9), width = 0.25) +
    labs(x = 'Region', y = 'Percent attributable (%)') +
    scale_x_discrete(labels = c(
      'Overall' = 'Overall',
      'coastside' = 'Coastside', 
      'mid' = 'Mid-county', 
      'north' = 'North county', 
      'south' = 'South county')) +
    scale_fill_manual(
      name = 'Exposure',
      values = c('ext_cold' = 'blue', 'cold' = 'lightblue', 'heat' = 'pink', 'ext_heat' = 'red'),
      labels = c('ext_cold' = 'Extreme Cold', 'cold' = 'Cold', 'heat' = 'Heat', 'ext_heat' = 'Extreme Heat')
    ) +
    geom_hline(aes(yintercept = 0), color = 'black') +
    geom_vline(xintercept = 1.5, linetype = 'dotted', color = 'black') +
    theme_classic() +
    theme(legend.position = 'inside',
          legend.position.inside = c(0.1, 0.8),
          legend.title = element_text(size = 16),         
          legend.text = element_text(size = 16),
          axis.title.x = element_text(size = 20),
          axis.text.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          axis.text.y = element_text(size = 20)
    ) 
  
  ggsave(filename = here::here('outputs', 'plots', 'ed', 'an', 'tmean', paste0('af_lag6_heatcold_region_ed_bar', data_source_name, '.png')), 
         height = 8, width = 12 )
  
  
  ################################################################################-
  ## 4: Plot attributable fraction of ED visits, cumulative 6 days lag (cause) ----
  ################################################################################-
  
  ### HeatRisk ----
  
  # Bar graph with heat and extreme heat
  an_calc_estimates %>%
    filter((subset_var == 'cause' | is.na(subset_var)) & temp_var == 'heat_risk') %>%
    mutate(subset = case_when(is.na(subset) ~ 'Overall',
                              TRUE ~ subset),
           subset = factor(subset, levels = c('Overall', 'cardiovascular', 
                                              'respiratory', 
                                              'stroke', 
                                              'unintentinjury', 
                                              'substance',
                                              'mental',
                                              'renal', 
                                              'genitourinary'))) %>%
    pivot_longer(
      cols = an_all_est:af_ext_cold_uci,
      names_to = c('type', 'category', 'measure'),
      names_pattern = '^([^_]+)_(.*)_([^_]+)$',
      values_to = 'estimate'
    ) %>%
    pivot_wider(
      names_from = measure,
      values_from = estimate
    ) %>%
    filter(type == 'af' & (category == 'heat' | category == 'ext_heat')) %>%
    mutate(category = factor(category, levels = c('heat', 'ext_heat'))) %>%
    ggplot() +
    geom_col(aes(x = subset, y = med, group = category, fill = category), position = position_dodge(width = 0.9)) +
    geom_errorbar(aes(x = subset, ymin = lci, ymax = uci, group = category), position = position_dodge(width = 0.9), width = 0.25) +
    labs(x = 'Cause of ED visit', y = 'Percent attributable (%)') +
    scale_x_discrete(labels = c(
      'Overall' = 'Overall',
      'cardiovascular' = 'Cardiovascular', 
      'respiratory' = 'Respiratory', 
      'stroke' = 'Stroke', 
      'unintentinjury' = 'Unintentional injury', 
      'substance' = 'Substance use',
      'mental' = 'Mental health',
      'renal' = 'Renal', 
      'genitourinary' = 'Genitourinary')) +
    scale_fill_manual(
      name = 'Exposure',
      values = c('heat' = '#f69632', 'ext_heat' = '#e22f33'),
      labels = c('heat' = 'Heat', 'ext_heat' = 'Extreme Heat')
    ) +
    geom_hline(aes(yintercept = 0), color = 'black') +
    geom_vline(xintercept = 1.5, linetype = 'dotted', color = 'black') +
    theme_classic() +
    theme(legend.position = 'inside',
          legend.position.inside = c(0.1, 0.9),
          legend.title = element_text(size = 16),         
          legend.text = element_text(size = 16),
          axis.title.x = element_text(size = 20),
          axis.text.x = element_text(size = 20, angle = 25, hjust = 1),
          axis.title.y = element_text(size = 20),
          axis.text.y = element_text(size = 20)
    ) 
  
  ggsave(filename = here::here('outputs', 'plots', 'ed', 'an', 'heat_risk', paste0('af_lag6_heat_cause_ed_bar', data_source_name, '.png')), 
         height = 9.5, width = 12 )
  
  ### Mean temperature ----
  
  an_calc_estimates %>%
    filter((subset_var == 'cause' | is.na(subset_var)) & temp_var == 'tmean') %>%
    mutate(subset = case_when(is.na(subset) ~ 'Overall',
                              TRUE ~ subset),
           subset = factor(subset, levels = c('Overall', 'cardiovascular', 
                                              'respiratory', 
                                              'stroke', 
                                              'unintentinjury', 
                                              'substance',
                                              'mental',
                                              'renal', 
                                              'genitourinary'))) %>%
    pivot_longer(
      cols = an_all_est:af_ext_cold_uci,
      names_to = c('type', 'category', 'measure'),
      names_pattern = '^([^_]+)_(.*)_([^_]+)$',
      values_to = 'estimate'
    ) %>%
    pivot_wider(
      names_from = measure,
      values_from = estimate
    ) %>% 
    filter(type == 'af' & (category == 'heat' | category == 'ext_heat')) %>%
    mutate(category = factor(category, levels = c('heat', 'ext_heat'))) %>%
    ggplot() +
    geom_col(aes(x = subset, y = med, group = category, fill = category), position = position_dodge(width = 0.9)) +
    geom_errorbar(aes(x = subset, ymin = lci, ymax = uci, group = category), position = position_dodge(width = 0.9), width = 0.25) +
    labs(x = 'Cause of ED visit', y = 'Percent attributable (%)') +
    scale_x_discrete(labels = c(
      'Overall' = 'Overall',
      'cardiovascular' = 'Cardiovascular', 
      'respiratory' = 'Respiratory', 
      'stroke' = 'Stroke', 
      'unintentinjury' = 'Unintentional injury', 
      'substance' = 'Substance use',
      'mental' = 'Mental health',
      'renal' = 'Renal', 
      'genitourinary' = 'Genitourinary')) +
    scale_fill_manual(
      name = 'Exposure',
      values = c('heat' = 'pink', 'ext_heat' = 'red'),
      labels = c('heat' = 'Heat', 'ext_heat' = 'Extreme Heat')
    ) +
    geom_hline(aes(yintercept = 0), color = 'black') +
    geom_vline(xintercept = 1.5, linetype = 'dotted', color = 'black') +
    theme_classic() +
    theme(legend.position = 'inside',
          legend.position.inside = c(0.1, 0.9),
          legend.title = element_text(size = 16),         
          legend.text = element_text(size = 16),
          axis.title.x = element_text(size = 20),
          axis.text.x = element_text(size = 20, angle = 25, hjust = 1),
          axis.title.y = element_text(size = 20),
          axis.text.y = element_text(size = 20)
    )
  
  ggsave(filename = here::here('outputs', 'plots', 'ed', 'an', 'tmean', paste0('af_lag6_heat_cause_ed_bar', data_source_name, '.png')), 
         height = 9.5, width = 12 )
  
  an_calc_estimates %>%
    filter((subset_var == 'cause' | is.na(subset_var)) & temp_var == 'tmean') %>%
    mutate(subset = case_when(is.na(subset) ~ 'Overall',
                              TRUE ~ subset),
           subset = factor(subset, levels = c('Overall', 'cardiovascular', 
                                              'respiratory', 
                                              'stroke', 
                                              'unintentinjury', 
                                              'substance',
                                              'mental',
                                              'renal', 
                                              'genitourinary'))) %>%
    pivot_longer(
      cols = an_all_est:af_ext_cold_uci,
      names_to = c('type', 'category', 'measure'),
      names_pattern = '^([^_]+)_(.*)_([^_]+)$',
      values_to = 'estimate'
    ) %>%
    pivot_wider(
      names_from = measure,
      values_from = estimate
    ) %>% 
    filter(type == 'af' & category != 'all') %>%
    mutate(category = factor(category, levels = c('ext_cold', 'cold', 'heat', 'ext_heat'))) %>%
    ggplot() +
    geom_col(aes(x = subset, y = med, group = category, fill = category), position = position_dodge(width = 0.9)) +
    geom_errorbar(aes(x = subset, ymin = lci, ymax = uci, group = category), position = position_dodge(width = 0.9), width = 0.25) +
    labs(x = 'Cause of ED visit', y = 'Percent attributable (%)') +    
    scale_x_discrete(labels = c(
      'Overall' = 'Overall',
      'cardiovascular' = 'Cardiovascular', 
      'respiratory' = 'Respiratory', 
      'stroke' = 'Stroke', 
      'unintentinjury' = 'Unintentional injury', 
      'substance' = 'Substance use',
      'mental' = 'Mental health',
      'renal' = 'Renal', 
      'genitourinary' = 'Genitourinary')) +
    scale_fill_manual(
      name = 'Exposure',
      values = c('ext_cold' = 'blue', 'cold' = 'lightblue', 'heat' = 'pink', 'ext_heat' = 'red'),
      labels = c('ext_cold' = 'Extreme Cold', 'cold' = 'Cold', 'heat' = 'Heat', 'ext_heat' = 'Extreme Heat')
    ) +
    geom_hline(aes(yintercept = 0), color = 'black') +
    geom_vline(xintercept = 1.5, linetype = 'dotted', color = 'black') +
    theme_classic() +
    theme(legend.position = 'inside',
          legend.position.inside = c(0.1, 0.9),
          legend.title = element_text(size = 16),         
          legend.text = element_text(size = 16),
          axis.title.x = element_text(size = 20),
          axis.text.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          axis.text.y = element_text(size = 20)
    ) 
  
  ggsave(filename = here::here('outputs', 'plots', 'ed', 'an', 'tmean', paste0('af_lag6_heatcold_cause_ed_bar', data_source_name, '.png')), 
         height = 8, width = 12 )
  
}

################################################################################-
## 5: Clean up ----
################################################################################-

rm(data_source_name, an_calc_estimates)

