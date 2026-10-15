################################################################################-
# Map: Attributable fraction and number of deaths by age and region ---
# Comparing HeatRisk and temperature for local heat surveillance: A time-stratified 
#  case-crossover study of emergency department visits in San Mateo County, 
#  California, 2017–2025
# Juliana GE Bartelss
# Updated: 3/6/2026
################################################################################-

# This file plots the attributable fraction and number of deaths by age and region 
# in a map.

################################################################################-
## 0: Upload data ----
################################################################################-

# Generate suffix for file calling and writing, based on data source
data_source_name <- case_when(data_source == 'original' ~ '',
                              data_source == 'synth' ~ '_synth')

# Load estimates and models for EMMs
an_calc_estimates <- read_csv(here::here('outputs', 'estimates', paste0('an_calc_estimates', data_source_name, '.csv')))

################################################################################-
## 1: Get geography for map ----
################################################################################-

smc_zips <- c('94018', '94019', '94020', '94021', '94037', 
              '94038', '94060', '94074', '94002', '94010', 
              '94065', '94070', '94401', '94402', '94403', 
              '94404', '94005', '94014', '94015', '94030', 
              '94044', '94066', '94080', '94128', '94025', 
              '94027', '94028', '94061', '94062', '94063', '94303')

smc_pop <- get_acs(
  geography = "zcta",
  variables = "B01003_001",
  year = 2019,
  state = "CA",
  geometry = TRUE)

smc_pop2 <- smc_pop %>%
  filter(GEOID %in% smc_zips) %>%
  mutate(region = case_when(GEOID %in% c('94018', '94019', '94020', '94021', '94037',
                                         '94038', '94060', '94074') ~ 'coastside',
                            GEOID %in% c('94002', '94010', '94065', '94070', '94401',
                                         '94402', '94403', '94404') ~ 'mid',
                            GEOID %in% c('94005', '94014', '94015', '94030', '94044',
                                         '94066', '94080', '94128') ~ 'north',
                            GEOID %in% c('94025', '94027', '94028', '94061', '94062',
                                         '94063', '94303') ~ 'south',
                            TRUE ~ NA)) %>%
  group_by(region) %>%
  summarize(geometry = st_union(geometry), .groups = 'drop')

################################################################################-
## 3: Join data to map ---
################################################################################-

an_region_estimates <- an_calc_estimates %>%
  filter(outcome == 'death' & subset_var == 'region' & temp_var == 'heat_risk') %>%
  mutate(subset = factor(subset, levels = c('coastside', 'mid', 'north', 'south'))) %>%
  dplyr::select(subset, an_heat_med, af_heat_med)

smc_region_an <- smc_pop2 %>%
  left_join(., an_region_estimates,
            by = c('region' = 'subset')) 

################################################################################-
## 4: Map attributable fraction and number of ED visits by region ----
################################################################################-

smc_region_af_map <- 
  smc_region_an %>%
  mutate(region = case_when(region == "coastside" ~ "Coastside",
                            region == "mid" ~ "Mid",
                            region == "north" ~ "North",
                            region == "south" ~ "South"
  )) %>%
  ggplot() +
  geom_sf(aes(fill = af_heat_med), 
          show.legend = c(fill = TRUE)) +
  scale_fill_viridis_c(option = "mako", begin = 0.5, direction = -1) +
  labs(fill = "Percent") +
  theme_void() +
  geom_sf_label(aes(label = region),
                size = 2.5,
                nudge_y = -0.015)

smc_region_an_map <- 
  smc_region_an %>%
  mutate(region = case_when(region == "coastside" ~ "Coastside",
                            region == "mid" ~ "Mid",
                            region == "north" ~ "North",
                            region == "south" ~ "South"
  )) %>%
  ggplot() +
  geom_sf(aes(fill = an_heat_med), 
          show.legend = c(fill = TRUE)) +
  scale_fill_viridis_c(option = "mako", begin = 0.5, direction = -1) +
  labs(fill = "Number") +
  theme_void() +
  geom_sf_label(aes(label = region),
                size = 2.5,
                nudge_y = -0.015)

plot_grid(
  smc_region_af_map,
  smc_region_an_map +
    theme(margin(r = 20)),
  labels = c("A. Attributable fraction of deaths", "B. Attributable number of deaths"),
  nrow = 1,
  hjust = -0.05,
  align = 'hv'
)

ggsave(filename = here::here('outputs', 'plots', 'death', 'an', 'heat_risk', 'afan_map_lag7_heat_death.png'), 
       height = 8, width = 12 )

################################################################################-
## 5: Clean up ----
################################################################################-

rm(an_calc_estimates,
   smc_zips, smc_pop, smc_pop2,
   an_region_estimates, smc_region_an,
   smc_region_af_map, smc_region_an_map)
