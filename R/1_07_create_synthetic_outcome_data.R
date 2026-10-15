################################################################################-
# Create synthetic data for mortality and ED visit outcomes ----
# Comparing HeatRisk and temperature for local heat surveillance: A time-stratified 
#  case-crossover study of emergency department visits in San Mateo County, 
#  California, 2017–2025
# Juliana GE Bartels
# Updated: 6/2/2026
################################################################################-

# This file creates synthetic data of outcomes (deaths, ED visits) to run overall and EMM sex analyses. 

################################################################################-
## 0: Load data ----
################################################################################-

# Load climate data 
climate_data <- read_csv(here::here('data', 'intermediate', 'climate_data.csv'))

# Population denominators by ZIP code
pop_zip <- read_csv(here::here('data', 'intermediate', 'pop_zip.csv'))

################################################################################-
## 1: Create synthetic dataframe of deaths and ED visit counts ----
################################################################################-

set.seed(402)

death_ed_df <- climate_data %>%
  mutate(year = as.double(format(date, '%Y')),
         death_count = floor(rgamma(nrow(.), shape = 1.05)),
         death_count_sex_f = ceiling(death_count * 0.5),
         death_count_sex_m = death_count - death_count_sex_f,
         ed_count = floor(rgamma(nrow(.), shape = 1.05, scale = 17)),
         ed_count_sex_f = ceiling(ed_count * 0.52),
         ed_count_sex_m = ed_count - ed_count_sex_f) %>%
  dplyr::select(-c(`...1`, po_name, ghcn, station_name, tmax, tmin, tmean, rh, heat_index, heat_risk_orig, heat_risk))

################################################################################-
## 2: Clean population denominator file ----
################################################################################-

pop_zip_reduced <- pop_zip %>%
  dplyr::select(zip_code, year, total_pop_all, total_pop_m, total_pop_f)

################################################################################-
## 3: Join climate/deaths/ed file with population denominators ----
################################################################################-

death_ed_pop_df <- death_ed_df %>%
  left_join(pop_zip_reduced, by = c('zip_code', 'year'))

################################################################################-
## 4: Export data ----
################################################################################-

death_ed_pop_df %>%
  write.csv(here::here('data', 'intermediate', 'death_ed_zip_pop_synth.csv'))

################################################################################-
## 5: Clean up ----
################################################################################-

rm(climate_data, pop_zip, death_ed_df, pop_zip_reduced, death_ed_pop_df)
