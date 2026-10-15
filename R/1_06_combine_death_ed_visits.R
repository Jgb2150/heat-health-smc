################################################################################-
# Combine all-cause deaths and all-cause mortality data, with population denominators ----
# Comparing HeatRisk and temperature for local heat surveillance: A time-stratified 
#  case-crossover study of emergency department visits in San Mateo County, 
#  California, 2017–2025
# Juliana GE Bartels
# Updated: 6/3/2026
################################################################################-

# This file combines dataframes of daily mortality and all-cause ED visit counts 
# and population denominators. The output is a dataframe with all daily death and 
# ED visit counts and population denominators by ZIP code.

################################################################################-
## 0: Load data ----
################################################################################-

# Death counts by ZIP code
death_zip <- read_csv(here::here('data', 'intermediate', 'death_zip.csv'))

# ED visit counts by ZIP code
ed_zip <- read_csv(here::here('data', 'intermediate', 'ed_zip.csv'))

# Population denominators by ZIP code
pop_zip <- read_csv(here::here('data', 'intermediate', 'pop_zip.csv'))

################################################################################-
## 1: Combine death and ED visit data ----
################################################################################-

death_ed_zip <- death_zip %>%
  full_join(ed_zip, by = c('date', 'zip_code'))

death_ed_zip <- death_ed_zip %>%
  mutate(year = as.double(format(date, '%Y')))

death_ed_zip_pop <- death_ed_zip %>%
  left_join(pop_zip, by = c('zip_code', 'year'))

################################################################################-
## 2: Export death and ED visit data ----
################################################################################-

death_ed_zip_pop %>%
  write_csv(here::here('data', 'intermediate', 'death_ed_zip_pop.csv'
  ))

################################################################################-
## 3: Clean up ----
################################################################################-

rm(death_zip, ed_zip, pop_zip, death_ed_zip, death_ed_zip_pop)

