################################################################################-
# Assign climate exposure variables to death and ED visit dataframe ----
# Comparing HeatRisk and temperature for local heat surveillance: A time-stratified 
#  case-crossover study of emergency department visits in San Mateo County, 
#  California, 2017–2025 
# Juliana GE Bartels
# Updated: 6/3/2026
################################################################################-

# This file combines climate data and daily outcome count data, and assigns 21 days
# of lagged exposures for each ZIP code-day. The output of this file is the main 
# dataframe used for all analyses. 

################################################################################-
## 0: Upload data ----
################################################################################-

# Load climate data
climate_data <- read_csv(here::here(
  "data", "intermediate",
  "climate_data.csv"
))

# Load death and ED visit data 
if(data_source == 'original') {
  death_ed_zip_pop <- read_csv(here::here(
    "data", "intermediate", "death_ed_zip_pop.csv"
  ))
}

if(data_source == 'synth') {
  death_ed_zip_pop <- read_csv(here::here(
    'data', 'intermediate', 'death_ed_zip_pop_synth.csv'
  )) %>%
    dplyr::select(-`...1`)
}

# Load holidays
holidays <- read_csv(here::here('data', 'raw', 'holidays.csv'))

################################################################################-
## 1: Assign lagged exposures to death data
################################################################################-

# Make copy of climate data
lagged_exp_df <- climate_data %>%
  group_by(zip_code)

# Assign lagged exposures (21 days)
for(i in 1:21) {
  lagged_exp_df <- assign_lag_exposure(lagged_exp_df, i)
}

# Join climate data to death and ED visit data
if(data_source == 'original') {
  death_ed_climate_df <- lagged_exp_df %>%
    left_join(death_ed_zip_pop, by = c("date", "zip_code")) %>%
    mutate(dow = wday(date),
           doy = yday(date),
           year = year(date),
           month = month(date),
           across(death_count:ed_count_restricthosp, ~ replace_na(.x, 0))
    )
}

if(data_source == 'synth') {
  death_ed_climate_df <- lagged_exp_df %>%
    left_join(death_ed_zip_pop, by = c("date", "zip_code")) %>%
    mutate(dow = wday(date),
           doy = yday(date),
           year = year(date),
           month = month(date),
           across(death_count:ed_count_sex_m, ~ replace_na(.x, 0))
    )
}

################################################################################-
## 2: Add holiday variable to dataframe ----
################################################################################-

holidays2 <- holidays %>%
  mutate(date = as.Date(date, '%m/%d/20%y'))

death_ed_climate_df <- death_ed_climate_df %>%
  left_join(., holidays2, by = c('date'),
            relationship = 'many-to-many') %>%
  mutate(holiday = case_when(is.na(holiday) ~ 0,
                             TRUE ~ holiday)) 

################################################################################-
## 2: Create stratum variable ----
################################################################################-

# Create stratum variable using zip, year, month, day of week
# Generate location-time-stratified strata
death_ed_climate_df$month <-  as.factor(death_ed_climate_df$month)
death_ed_climate_df$year  <- as.factor(death_ed_climate_df$year)
death_ed_climate_df$dow   <- as.factor(death_ed_climate_df$dow)
death_ed_climate_df$zip_code <- as.factor(death_ed_climate_df$zip_code)
death_ed_climate_df$stratum <- as.factor(with(death_ed_climate_df, as.factor(year:month:dow:zip_code)))

################################################################################-
## 3: Export data ----
################################################################################-

if(data_source == 'original') {
  death_ed_climate_df %>%
    write_csv(here::here(
      "data", "prepared",
      "death_ed_climate_df.csv"
    ))
}

if(data_source == 'synth') {
  death_ed_climate_df %>%
    write_csv(here::here(
      "data", "prepared",
      "death_ed_climate_df_synth.csv"
    ))
}

################################################################################-
## 4: Clean up ----
################################################################################-

rm(climate_data, death_ed_zip_pop, lagged_exp_df, death_ed_climate_df, holidays, holidays2)
