################################################################################-
# Table 1: Demographic characteristics of San Mateo County, 2017-2024 ---
# Comparing HeatRisk and temperature for local heat surveillance: A time-stratified 
#  case-crossover study of emergency department visits in San Mateo County, 
#  California, 2017–2025
# Juliana GE Bartels
# Updated: 3/6/2026
################################################################################-

# This file creates summarized dataframes of daily death counts and ED visits to
# create a Table 1 for the manuscript. 

################################################################################-
## 0: Upload data ----
################################################################################-

## Load dataset of ED visits and deaths matched with temperature exposures ----
dta_full <- read_csv(here::here(
  "data", "prepared",
  "death_ed_climate_df.csv"
)) %>%
  mutate(stratum = as.factor(stratum))

################################################################################-
## 1: Create ED visits Table 1 ----
################################################################################-

ed_totals <- dta_full %>%
  group_by(heat_risk) %>%
  summarize(total_count = sum(ed_count, na.rm = TRUE),
            sex_f_count = sum(ed_count_sex_f, na.rm = TRUE),
            sex_m_count = sum(ed_count_sex_m, na.rm = TRUE),
            age_under5_count = sum(ed_count_age_under5, na.rm = TRUE),
            age_5to17_count = sum(ed_count_age_5to17, na.rm = TRUE),
            age_18to64_count = sum(ed_count_age_18to64, na.rm = TRUE),
            age_65plus_count = sum(ed_count_age_65plus, na.rm = TRUE),
            region_coastside_count = sum(ed_count_region_coastside, na.rm = TRUE),
            region_mid_count = sum(ed_count_region_mid, na.rm = TRUE),
            region_north_count = sum(ed_count_region_north, na.rm = TRUE),
            region_south_count = sum(ed_count_region_south, na.rm = TRUE),
            cause_cardiovascular_count = sum(ed_count_cause_cardiovascular, na.rm = TRUE),
            cause_respiratory_count = sum(ed_count_cause_respiratory, na.rm = TRUE),
            cause_stroke_count = sum(ed_count_cause_stroke, na.rm = TRUE),
            cause_intentinjury_count = sum(ed_count_cause_intentinjury, na.rm = TRUE),
            cause_unintentinjury_count = sum(ed_count_cause_unintentinjury, na.rm = TRUE),
            cause_heat_count = sum(ed_count_cause_heat, na.rm = TRUE),
            cause_substance_count = sum(ed_count_cause_substance, na.rm = TRUE),
            cause_mental_count = sum(ed_count_cause_mental, na.rm = TRUE),
            cause_renal_count = sum(ed_count_cause_renal, na.rm = TRUE),
            cause_genitourinary_count = sum(ed_count_cause_genitourinary, na.rm = TRUE)
  )

ed_counts <- ed_totals %>%
  rbind(., colSums(.)) %>%
  mutate(heat_risk = replace(heat_risk, 6, "Overall"),
         sex_f_pct = round((sex_f_count/total_count * 100), 1),
         sex_m_pct = round((sex_m_count/total_count * 100), 1),
         age_under5_pct = round((age_under5_count/total_count * 100), 1),
         age_5to17_pct = round((age_5to17_count/total_count * 100), 1),
         age_18to64_pct = round((age_18to64_count/total_count * 100), 1),
         age_65plus_pct = round((age_65plus_count/total_count * 100), 1),
         region_coastside_pct = round((region_coastside_count/total_count * 100), 1),
         region_mid_pct = round((region_mid_count/total_count * 100), 1),
         region_north_pct = round((region_north_count/total_count * 100), 1),
         region_south_pct = round((region_south_count/total_count * 100), 1),
         cause_cardiovascular_pct = round((cause_cardiovascular_count/total_count * 100), 1),
         cause_respiratory_pct = round((cause_respiratory_count/total_count * 100), 1),
         cause_stroke_pct = round((cause_stroke_count/total_count * 100), 1),
         cause_intentinjury_pct = round((cause_intentinjury_count/total_count * 100), 1),
         cause_unintentinjury_pct = round((cause_unintentinjury_count/total_count * 100), 1),
         cause_heat_pct = round((cause_heat_count/total_count * 100), 1),
         cause_substance_pct = round((cause_substance_count/total_count * 100), 1),
         cause_mental_pct = round((cause_mental_count/total_count * 100), 1),
         cause_renal_pct = round((cause_renal_count/total_count * 100), 1),
         cause_genitourinary_pct = round((cause_genitourinary_count/total_count * 100), 1)
         ) %>%
  pivot_longer(
    cols = -heat_risk,
  ) %>% 
  pivot_wider(
    names_from = heat_risk,
    values_from = value
  ) 

ed_counts %>%
  write_csv(here::here('outputs', 'tables', 'table1_ed.csv'))

################################################################################-
## 2: Create deaths Table 1 ----
################################################################################-

death_totals <- dta_full %>%
  group_by(heat_risk) %>%
  summarize(total_count = sum(death_count, na.rm = TRUE),
            sex_f_count = sum(death_count_sex_f, na.rm = TRUE),
            sex_m_count = sum(death_count_sex_m, na.rm = TRUE),
            age_under5_count = sum(death_count_age_under5, na.rm = TRUE),
            age_5to17_count = sum(death_count_age_5to17, na.rm = TRUE),
            age_18to64_count = sum(death_count_age_18to64, na.rm = TRUE),
            age_65plus_count = sum(death_count_age_65plus, na.rm = TRUE),
            region_coastside_count = sum(death_count_region_coastside, na.rm = TRUE),
            region_mid_count = sum(death_count_region_mid, na.rm = TRUE),
            region_north_count = sum(death_count_region_north, na.rm = TRUE),
            region_south_count = sum(death_count_region_south, na.rm = TRUE),
            cause_cardiovascular_count = sum(death_count_cause_cardiovascular, na.rm = TRUE),
            cause_respiratory_count = sum(death_count_cause_respiratory, na.rm = TRUE),
            cause_stroke_count = sum(death_count_cause_stroke, na.rm = TRUE),
            cause_intentinjury_count = sum(death_count_cause_intentinjury, na.rm = TRUE),
            cause_unintentinjury_count = sum(death_count_cause_unintentinjury, na.rm = TRUE),
            cause_heat_count = sum(death_count_cause_heat, na.rm = TRUE),
            cause_substance_count = sum(death_count_cause_substance, na.rm = TRUE),
            cause_mental_count = sum(death_count_cause_mental, na.rm = TRUE),
            cause_renal_count = sum(death_count_cause_renal, na.rm = TRUE),
            cause_genitourinary_count = sum(death_count_cause_genitourinary, na.rm = TRUE)
  )

death_counts <- death_totals %>%
  rbind(., colSums(.)) %>%
  mutate(heat_risk = replace(heat_risk, 6, "Overall"),
         sex_f_pct = round((sex_f_count/total_count * 100), 1),
         sex_m_pct = round((sex_m_count/total_count * 100), 1),
         age_under5_pct = round((age_under5_count/total_count * 100), 1),
         age_5to17_pct = round((age_5to17_count/total_count * 100), 1),
         age_18to64_pct = round((age_18to64_count/total_count * 100), 1),
         age_65plus_pct = round((age_65plus_count/total_count * 100), 1),
         region_coastside_pct = round((region_coastside_count/total_count * 100), 1),
         region_mid_pct = round((region_mid_count/total_count * 100), 1),
         region_north_pct = round((region_north_count/total_count * 100), 1),
         region_south_pct = round((region_south_count/total_count * 100), 1),
         cause_cardiovascular_pct = round((cause_cardiovascular_count/total_count * 100), 1),
         cause_respiratory_pct = round((cause_respiratory_count/total_count * 100), 1),
         cause_stroke_pct = round((cause_stroke_count/total_count * 100), 1),
         cause_intentinjury_pct = round((cause_intentinjury_count/total_count * 100), 1),
         cause_unintentinjury_pct = round((cause_unintentinjury_count/total_count * 100), 1),
         cause_heat_pct = round((cause_heat_count/total_count * 100), 1),
         cause_substance_pct = round((cause_substance_count/total_count * 100), 1),
         cause_mental_pct = round((cause_mental_count/total_count * 100), 1),
         cause_renal_pct = round((cause_renal_count/total_count * 100), 1),
         cause_genitourinary_pct = round((cause_genitourinary_count/total_count * 100), 1)
  ) %>%
  pivot_longer(
    cols = -heat_risk,
  ) %>% 
  pivot_wider(
    names_from = heat_risk,
    values_from = value
  ) 

death_counts %>%
  write_csv(here::here('outputs', 'tables', 'table1_death.csv'))

################################################################################-
## 3: Clean up ----
################################################################################-

rm(dta_full, ed_totals, ed_counts, death_totals, death_counts)
