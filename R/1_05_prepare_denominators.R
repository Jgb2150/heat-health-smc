################################################################################-
# Pull population denominator data ----
# Comparing HeatRisk and temperature for local heat surveillance: A time-stratified 
#  case-crossover study of emergency department visits in San Mateo County, 
#  California, 2017–2025 
# Juliana GE Bartels
# Updated: 6/30/2026
################################################################################-

# This file pulls and cleans annual population denominator data for each ZIP code  
# in SMC, for the overall population and by sub-group. 

################################################################################-
## 0: Load population denominators from Azure ----
################################################################################-

# Connect to Azure
# This requires a personal password
con <- azure_connect(creds_file = here::here('creds', 'azure_epi-warehouse_creds.yml'))

# Load population denominators data from 2017 to 2025
pop_df <- tbl(con, in_schema("dbo", "acs_b01001_age_sex_pop_estimates")) %>%
  filter(year %in% 2017:2025 & survey == "acs5" & geography == "zcta") %>%
  collect()

################################################################################-
## 1: Clean population denominators ----
################################################################################-

pop_df_clean <- pop_df %>%
  clean_names() %>%
  dplyr::select(-c(name, variable, download_dt, survey, geography, race_cat, moe)) %>%
  rename(zip_code = geoid)

################################################################################-
## 2: Calculate totals for each demographic subgroup ----
################################################################################-

# Population by sex
pop_df_all <- pop_df_clean %>% 
  pivot_wider(names_from = sex,
              names_prefix = "total_pop_",
              values_from = c(estimate)
              ) %>%
  mutate(total_pop_all = case_when(
    is.na(total_pop_all) ~ total_pop_Male + total_pop_Female,
    TRUE ~ total_pop_all
  ))

# Population under 5 years old
pop_df_under5 <- pop_df_all %>% 
  filter(age_group == "Under 5 years") %>%
  dplyr::select(-c(total_pop_Male, total_pop_Female, age_group)) %>%
  rename(total_pop_under5 = total_pop_all)

# Ppopulation aged 5 to 17
pop_df_5to17 <- pop_df_all %>%
  filter(age_group %in% c("5 to 9 years", "10 to 14 years", "15 to 17 years")) %>%
  group_by(zip_code, year) %>%
  summarize(total_pop_5to17 = sum(total_pop_all))

# Population aged 18 to 64
pop_df_18to64 <- pop_df_all %>%
  filter(age_group %in% c("18 and 19 years", "20 years", "21 years", "22 to 24 years", "25 to 29 years",
                          "30 to 34 years", "35 to 39 years", "40 to 44 years", "45 to 49 years",
                          "50 to 54 years", "55 to 59 years", "60 and 61 years", "62 to 64 years" )) %>%
  group_by(zip_code, year) %>%
  summarize(total_pop_18to64 = sum(total_pop_all))

# Population aged 65 and over
pop_df_65plus <- pop_df_all %>%
  filter(age_group %in% c("65 and 66 years", "67 to 69 years", "70 to 74 years",   
                          "75 to 79 years", "80 to 84 years", "85 years and over")) %>%
  group_by(zip_code, year) %>%
  summarize(total_pop_65plus = sum(total_pop_all))

# Total population
pop_df_all <- pop_df_all %>%
  filter(age_group %in% c("all")) %>%
  dplyr::select(-c(age_group))

# Combine dataframes
total_pop_list <- list(pop_df_all, pop_df_under5, pop_df_5to17, pop_df_18to64, pop_df_65plus)

total_pop_zip <- reduce(total_pop_list, full_join, by = c('zip_code', 'year'))

total_pop_zip <- total_pop_zip %>%
  rename(total_pop_m = total_pop_Male,
         total_pop_f = total_pop_Female)

################################################################################-
## 3: Calculate population by SMC region ----
################################################################################-

# Calculate total population by region
pop_region <- pop_df_clean %>%
  filter(age_group == "all" & sex == "all") %>%
  mutate(
    region = case_when(
      zip_code %in% c(94018, 94019, 94020, 94021, 94037, 
                      94038, 94060, 94074) ~ 'coastside',
      zip_code %in% c(94002, 94010, 94065, 94070, 94401,
                      94402, 94403, 94404) ~ 'mid',
      zip_code %in% c(94005, 94014, 94015, 94030, 94044,
                      94066, 94080, 94128) ~ 'north',
      zip_code %in% c(94025, 94027, 94028, 94061, 94062,
                      94063, 94303) ~ 'south',
      TRUE ~ NA
    )) %>% 
  group_by(year) %>%
  mutate(
    total_pop_coastside  = case_when(
      region == 'coastside' ~ sum(estimate[region == "coastside"],  na.rm = TRUE),
      TRUE ~ 0),
    total_pop_mid = case_when(
      region == 'mid' ~ sum(estimate[region == "mid"],  na.rm = TRUE),
      TRUE ~ 0),
    total_pop_north = case_when(
      region == 'north' ~ sum(estimate[region == "north"],  na.rm = TRUE),
      TRUE ~ 0),
    total_pop_south = case_when(
      region == 'south' ~ sum(estimate[region == "south"],  na.rm = TRUE),
      TRUE ~ 0)
    ) %>% 
  dplyr::select(-sex, -age_group, -region, -estimate) %>%
  ungroup()

total_pop_zip_region <- total_pop_zip %>%
  full_join(., pop_region, by = c('zip_code', 'year'))

################################################################################-
## 3: Calculate population by ZIP code ----
################################################################################-

pop_zip <- pop_df_clean %>%
  filter(age_group == "all" & sex == "all") %>%
  group_by(year, zip_code) %>%
  summarize(total_pop_zip = sum(estimate)) %>%
  ungroup()

pop_zip <- pop_zip %>%
  mutate(zip_code2 = zip_code) %>%
  pivot_wider(
    names_prefix = "total_pop_",
    names_from = zip_code,
    values_from = total_pop_zip
  ) %>%
  rename(zip_code = zip_code2) %>%
  mutate(across(everything(), ~replace_na(., 0)))

total_pop_full <- total_pop_zip_region %>%
  full_join(., pop_zip, by = c('zip_code', 'year'))

################################################################################-
## 3: Create dataframe for 2025 ----
################################################################################-

# As of May 2026, ACS has not yet released population estimates for 2025 
# We are going to impute values for 2025 using 2024 estimates

total_pop_full_2025 <- total_pop_full %>%
  filter(year == "2024") %>%
  mutate(year = "2025")

# Join 2025 dataframe to total_pop_full

total_pop_full2 <- total_pop_full %>%
  rbind(., total_pop_full_2025)
  
################################################################################-
## 4: Export population denominators ----
################################################################################-

total_pop_full2 %>%
  write_csv(here::here('data', 'intermediate', 'pop_zip.csv'))

################################################################################-
## 5: Clean up ----
################################################################################-

rm(pop_df_all, pop_df_under5, pop_df_5to17, pop_df_18to64, pop_df_65plus,
   total_pop_list, pop_zip, pop_df, pop_df_clean, pop_region, total_pop_zip, total_pop_zip_region, 
   total_pop_full, total_pop_full_2025, total_pop_full2, rhfiles, tmaxfiles, tminfiles)
