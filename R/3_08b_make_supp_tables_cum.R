################################################################################-
# Make supplementary tables of cumulative percent change estimates for mean temperature and HeatRisk ----
# Comparing HeatRisk and temperature for local heat surveillance: A time-stratified 
#  case-crossover study of emergency department visits in San Mateo County, 
#  California, 2017–2025
# Juliana GE Bartels
# Updated: 9/18/2026
################################################################################-

# This file makes tables of estimates for the supplement.

################################################################################-
## 0: Upload data ----
################################################################################-

# Load cumulative ED estimates for mean temperature
files <- list.files(
  path = here::here('outputs', 'estimates', 'ed', 'cum', 'tmean'),
  pattern = "\\.csv$",
  full.names = TRUE
)

for (f in files) {
  df_name <- file_path_sans_ext(basename(f))
  assign(df_name, read_csv(f), envir = .GlobalEnv)
}

# Load cumulative ED estimates for HeatRisk
files <- list.files(
  path = here::here('outputs', 'estimates', 'ed', 'cum', 'heat_risk'),
  pattern = "\\.csv$",
  full.names = TRUE
)

for (f in files) {
  df_name <- file_path_sans_ext(basename(f))
  assign(df_name, read_csv(f), envir = .GlobalEnv)
}

# Load cumulative death estimates for mean temperature
files <- list.files(
  path = here::here('outputs', 'estimates', 'death', 'cum', 'tmean'),
  pattern = "\\.csv$",
  full.names = TRUE
)

for (f in files) {
  df_name <- file_path_sans_ext(basename(f))
  assign(df_name, read_csv(f), envir = .GlobalEnv)
}

# Load cumulative death estimates for HeatRisk
files <- list.files(
  path = here::here('outputs', 'estimates', 'death', 'cum', 'heat_risk'),
  pattern = "\\.csv$",
  full.names = TRUE
)

for (f in files) {
  df_name <- file_path_sans_ext(basename(f))
  assign(df_name, read_csv(f), envir = .GlobalEnv)
}

################################################################################-
## 1a: Make supplemental table of ED mean temperature ----
################################################################################-

estimates_tmean_ed <- list(
  full = est_cum_ed_main_full_full_a_quad_ns_full_tmean,
  
  age_under5 = est_cum_ed_main_age_under5_a_quad_ns_full_tmean,
  age_5to17 = est_cum_ed_main_age_5to17_a_quad_ns_full_tmean,
  age_18to64 = est_cum_ed_main_age_18to64_a_quad_ns_full_tmean,
  age_65plus = est_cum_ed_main_age_65plus_a_quad_ns_full_tmean,
  
  region_north = est_cum_ed_main_region_north_a_quad_ns_full_tmean,
  region_mid = est_cum_ed_main_region_mid_a_quad_ns_full_tmean,
  region_south = est_cum_ed_main_region_south_a_quad_ns_full_tmean,
  region_coastside = est_cum_ed_main_region_coastside_a_quad_ns_full_tmean,
  
  sex_f = est_cum_ed_main_sex_f_a_quad_ns_full_tmean,
  sex_m = est_cum_ed_main_sex_m_a_quad_ns_full_tmean,
  
  cause_cardiovascular = est_cum_ed_main_cause_cardiovascular_a_quad_ns_full_tmean,
  cause_genitourinary = est_cum_ed_main_cause_genitourinary_a_quad_ns_full_tmean,
  cause_mental = est_cum_ed_main_cause_mental_a_quad_ns_full_tmean,
  cause_renal = est_cum_ed_main_cause_renal_a_quad_ns_full_tmean,
  cause_respiratory = est_cum_ed_main_cause_respiratory_a_quad_ns_full_tmean,
  cause_stroke = est_cum_ed_main_cause_stroke_a_quad_ns_full_tmean,
  cause_substance = est_cum_ed_main_cause_substance_a_quad_ns_full_tmean,
  cause_unintentinjury = est_cum_ed_main_cause_unintentinjury_a_quad_ns_full_tmean,
  
  causeunder5_genitourinary = est_cum_ed_main_causeunder5_genitourinary_a_quad_ns_full_tmean,
  causeunder5_respiratory = est_cum_ed_main_causeunder5_respiratory_a_quad_ns_full_tmean,
  causeunder5_unintentinjury = est_cum_ed_main_causeunder5_unintentinjury_a_quad_ns_full_tmean,
  
  cause5to17_cardiovascular = est_cum_ed_main_cause5to17_cardiovascular_a_quad_ns_full_tmean,
  cause5to17_genitourinary = est_cum_ed_main_cause5to17_genitourinary_a_quad_ns_full_tmean,
  cause5to17_mental = est_cum_ed_main_cause5to17_mental_a_quad_ns_full_tmean,
  cause5to17_respiratory = est_cum_ed_main_cause5to17_respiratory_a_quad_ns_full_tmean,
  cause5to17_substance = est_cum_ed_main_cause5to17_substance_a_quad_ns_full_tmean,
  cause5to17_unintentinjury = est_cum_ed_main_cause5to17_unintentinjury_a_quad_ns_full_tmean,
  
  cause18to64_cardiovascular = est_cum_ed_main_cause18to64_cardiovascular_a_quad_ns_full_tmean,
  cause18to64_genitourinary = est_cum_ed_main_cause18to64_genitourinary_a_quad_ns_full_tmean,
  cause18to64_mental = est_cum_ed_main_cause18to64_mental_a_quad_ns_full_tmean,
  cause18to64_renal = est_cum_ed_main_cause18to64_renal_a_quad_ns_full_tmean,
  cause18to64_respiratory = est_cum_ed_main_cause18to64_respiratory_a_quad_ns_full_tmean,
  cause18to64_stroke = est_cum_ed_main_cause18to64_stroke_a_quad_ns_full_tmean,
  cause18to64_substance = est_cum_ed_main_cause18to64_substance_a_quad_ns_full_tmean,
  cause18to64_unintentinjury = est_cum_ed_main_cause18to64_unintentinjury_a_quad_ns_full_tmean,
  
  cause65plus_cardiovascular = est_cum_ed_main_cause65plus_cardiovascular_a_quad_ns_full_tmean,
  cause65plus_genitourinary = est_cum_ed_main_cause65plus_genitourinary_a_quad_ns_full_tmean,
  cause65plus_mental = est_cum_ed_main_cause65plus_mental_a_quad_ns_full_tmean,
  cause65plus_renal = est_cum_ed_main_cause65plus_renal_a_quad_ns_full_tmean,
  cause65plus_respiratory = est_cum_ed_main_cause65plus_respiratory_a_quad_ns_full_tmean,
  cause65plus_stroke = est_cum_ed_main_cause65plus_stroke_a_quad_ns_full_tmean,
  cause65plus_substance = est_cum_ed_main_cause65plus_substance_a_quad_ns_full_tmean,
  cause65plus_unintentinjury = est_cum_ed_main_cause65plus_unintentinjury_a_quad_ns_full_tmean
  )

format_supp_table_cum_tmean(estimates_tmean_ed) %>% 
  write_csv(., here::here('outputs', 'tables', 'supp_table_tmean_ed.csv'))

################################################################################-
## 1b: Make supplemental table of ED HeatRisk ----
################################################################################-

estimates_heat_risk_ed <- list(
  full = est_cum_ed_main_full_full_a_strata_ns_full_heat_risk,
  
  age_under5 = est_cum_ed_main_age_under5_a_strata_ns_full_heat_risk,
  age_5to17 = est_cum_ed_main_age_5to17_a_strata_ns_full_heat_risk,
  age_18to64 = est_cum_ed_main_age_18to64_a_strata_ns_full_heat_risk,
  age_65plus = est_cum_ed_main_age_65plus_a_strata_ns_full_heat_risk,
  
  region_north = est_cum_ed_main_region_north_a_strata_ns_full_heat_risk,
  region_mid = est_cum_ed_main_region_mid_a_strata_ns_full_heat_risk,
  region_south = est_cum_ed_main_region_south_a_strata_ns_full_heat_risk,
  region_coastside = est_cum_ed_main_region_coastside_a_strata_ns_full_heat_risk,
  
  sex_f = est_cum_ed_main_sex_f_a_strata_ns_full_heat_risk,
  sex_m = est_cum_ed_main_sex_m_a_strata_ns_full_heat_risk,
  
  cause_cardiovascular = est_cum_ed_main_cause_cardiovascular_a_strata_ns_full_heat_risk,
  cause_genitourinary = est_cum_ed_main_cause_genitourinary_a_strata_ns_full_heat_risk,
  cause_mental = est_cum_ed_main_cause_mental_a_strata_ns_full_heat_risk,
  cause_renal = est_cum_ed_main_cause_renal_a_strata_ns_full_heat_risk,
  cause_respiratory = est_cum_ed_main_cause_respiratory_a_strata_ns_full_heat_risk,
  cause_stroke = est_cum_ed_main_cause_stroke_a_strata_ns_full_heat_risk,
  cause_substance = est_cum_ed_main_cause_substance_a_strata_ns_full_heat_risk,
  cause_unintentinjury = est_cum_ed_main_cause_unintentinjury_a_strata_ns_full_heat_risk,
  
  causeunder5_genitourinary = est_cum_ed_main_causeunder5_genitourinary_a_strata_ns_full_heat_risk,
  causeunder5_respiratory = est_cum_ed_main_causeunder5_respiratory_a_strata_ns_full_heat_risk,
  causeunder5_unintentinjury = est_cum_ed_main_causeunder5_unintentinjury_a_strata_ns_full_heat_risk,
  
  cause5to17_cardiovascular = est_cum_ed_main_cause5to17_cardiovascular_a_strata_ns_full_heat_risk,
  cause5to17_genitourinary = est_cum_ed_main_cause5to17_genitourinary_a_strata_ns_full_heat_risk,
  cause5to17_mental = est_cum_ed_main_cause5to17_mental_a_strata_ns_full_heat_risk,
  cause5to17_respiratory = est_cum_ed_main_cause5to17_respiratory_a_strata_ns_full_heat_risk,
  cause5to17_substance = est_cum_ed_main_cause5to17_substance_a_strata_ns_full_heat_risk,
  cause5to17_unintentinjury = est_cum_ed_main_cause5to17_unintentinjury_a_strata_ns_full_heat_risk,
  
  cause18to64_cardiovascular = est_cum_ed_main_cause18to64_cardiovascular_a_strata_ns_full_heat_risk,
  cause18to64_genitourinary = est_cum_ed_main_cause18to64_genitourinary_a_strata_ns_full_heat_risk,
  cause18to64_mental = est_cum_ed_main_cause18to64_mental_a_strata_ns_full_heat_risk,
  cause18to64_renal = est_cum_ed_main_cause18to64_renal_a_strata_ns_full_heat_risk,
  cause18to64_respiratory = est_cum_ed_main_cause18to64_respiratory_a_strata_ns_full_heat_risk,
  cause18to64_stroke = est_cum_ed_main_cause18to64_stroke_a_strata_ns_full_heat_risk,
  cause18to64_substance = est_cum_ed_main_cause18to64_substance_a_strata_ns_full_heat_risk,
  cause18to64_unintentinjury = est_cum_ed_main_cause18to64_unintentinjury_a_strata_ns_full_heat_risk,
  
  cause65plus_cardiovascular = est_cum_ed_main_cause65plus_cardiovascular_a_strata_ns_full_heat_risk,
  cause65plus_genitourinary = est_cum_ed_main_cause65plus_genitourinary_a_strata_ns_full_heat_risk,
  cause65plus_mental = est_cum_ed_main_cause65plus_mental_a_strata_ns_full_heat_risk,
  cause65plus_renal = est_cum_ed_main_cause65plus_renal_a_strata_ns_full_heat_risk,
  cause65plus_respiratory = est_cum_ed_main_cause65plus_respiratory_a_strata_ns_full_heat_risk,
  cause65plus_stroke = est_cum_ed_main_cause65plus_stroke_a_strata_ns_full_heat_risk,
  cause65plus_substance = est_cum_ed_main_cause65plus_substance_a_strata_ns_full_heat_risk,
  cause65plus_unintentinjury = est_cum_ed_main_cause65plus_unintentinjury_a_strata_ns_full_heat_risk
)

format_supp_table_cum_hr(estimates_heat_risk_ed) %>%
  write_csv(., here::here('outputs', 'tables', 'supp_table_heat_risk_ed.csv'))

################################################################################-
## 2a: Make supplemental table of death mean temperature ----
################################################################################-

estimates_tmean_death <- list(
  full = est_cum_death_main_full_full_a_quad_ns_full_tmean,
  
  age_under5 = est_cum_death_main_age_under5_a_quad_ns_full_tmean,
  age_18to64 = est_cum_death_main_age_18to64_a_quad_ns_full_tmean,
  age_65plus = est_cum_death_main_age_65plus_a_quad_ns_full_tmean,
  
  region_north = est_cum_death_main_region_north_a_quad_ns_full_tmean,
  region_mid = est_cum_death_main_region_mid_a_quad_ns_full_tmean,
  region_south = est_cum_death_main_region_south_a_quad_ns_full_tmean,
  region_coastside = est_cum_death_main_region_coastside_a_quad_ns_full_tmean,
  
  sex_f = est_cum_death_main_sex_f_a_quad_ns_full_tmean,
  sex_m = est_cum_death_main_sex_m_a_quad_ns_full_tmean,
  
  cause_cardiovascular = est_cum_death_main_cause_cardiovascular_a_quad_ns_full_tmean,
  cause_genitourinary = est_cum_death_main_cause_genitourinary_a_quad_ns_full_tmean,
  cause_intentinjury = est_cum_death_main_cause_intentinjury_a_quad_ns_full_tmean,
  cause_renal = est_cum_death_main_cause_renal_a_quad_ns_full_tmean,
  cause_respiratory = est_cum_death_main_cause_respiratory_a_quad_ns_full_tmean,
  cause_stroke = est_cum_death_main_cause_stroke_a_quad_ns_full_tmean,
  cause_substance = est_cum_death_main_cause_substance_a_quad_ns_full_tmean,
  cause_unintentinjury = est_cum_death_main_cause_unintentinjury_a_quad_ns_full_tmean
)

format_supp_table_cum_tmean(estimates_tmean_death) %>% 
  write_csv(., here::here('outputs', 'tables', 'supp_table_tmean_death.csv'))

################################################################################-
## 2b: Make supplemental table of death HeatRisk ----
################################################################################-

estimates_heat_risk_death <- list(
  full = est_cum_death_main_full_full_a_strata_ns_full_heat_risk,
  
  age_under5 = est_cum_death_main_age_under5_a_strata_ns_full_heat_risk,
  age_18to64 = est_cum_death_main_age_18to64_a_strata_ns_full_heat_risk,
  age_65plus = est_cum_death_main_age_65plus_a_strata_ns_full_heat_risk,
  
  region_north = est_cum_death_main_region_north_a_strata_ns_full_heat_risk,
  region_mid = est_cum_death_main_region_mid_a_strata_ns_full_heat_risk,
  region_south = est_cum_death_main_region_south_a_strata_ns_full_heat_risk,
  region_coastside = est_cum_death_main_region_coastside_a_strata_ns_full_heat_risk,
  
  sex_f = est_cum_death_main_sex_f_a_strata_ns_full_heat_risk,
  sex_m = est_cum_death_main_sex_m_a_strata_ns_full_heat_risk,
  
  cause_cardiovascular = est_cum_death_main_cause_cardiovascular_a_strata_ns_full_heat_risk,
  cause_genitourinary = est_cum_death_main_cause_genitourinary_a_strata_ns_full_heat_risk,
  cause_renal = est_cum_death_main_cause_renal_a_strata_ns_full_heat_risk,
  cause_respiratory = est_cum_death_main_cause_respiratory_a_strata_ns_full_heat_risk,
  cause_stroke = est_cum_death_main_cause_stroke_a_strata_ns_full_heat_risk,
  cause_substance = est_cum_death_main_cause_substance_a_strata_ns_full_heat_risk,
  cause_unintentinjury = est_cum_death_main_cause_unintentinjury_a_strata_ns_full_heat_risk
)

format_supp_table_cum_hr(estimates_heat_risk_death) %>%
  write_csv(., here::here('outputs', 'tables', 'supp_table_heat_risk_death.csv'))

################################################################################-
## 3a: Make supplemental table of no COVID model ED mean temperature ----
################################################################################-

estimates_tmean_ed_nocov <- list(
  full = est_cum_ed_nocov_full_full_a_quad_ns_full_tmean)

format_supp_table_cum_tmean(estimates_tmean_ed_nocov) %>%
  write_csv(., here::here('outputs', 'tables', 'supp_table_tmean_ed_nocov.csv'))

################################################################################-
## 3b: Make supplemental table of no COVID model ED HeatRisk ----
################################################################################-

estimates_heat_risk_ed_nocov <- list(
  full = est_cum_ed_nocov_full_full_a_strata_ns_full_heat_risk)

format_supp_table_cum_hr(estimates_heat_risk_ed_nocov) %>%
  write_csv(., here::here('outputs', 'tables', 'supp_table_heat_risk_ed_nocov.csv'))

################################################################################-
## 3c: Make supplemental table of no COVID model ED mean temperature ----
################################################################################-

estimates_tmean_ed_restricthosp <- list(
  full = est_cum_ed_restricthosp_full_full_a_quad_ns_full_tmean)

format_supp_table_cum_tmean(estimates_tmean_ed_restricthosp) %>%
  write_csv(., here::here('outputs', 'tables', 'supp_table_tmean_ed_restricthosp.csv'))

################################################################################-
## 3d: Make supplemental table of no COVID model ED HeatRisk ----
################################################################################-

estimates_heat_risk_ed_restricthosp <- list(
  full = est_cum_ed_restricthosp_full_full_a_strata_ns_full_heat_risk)

format_supp_table_cum_hr(estimates_heat_risk_ed_restricthosp) %>%
  write_csv(., here::here('outputs', 'tables', 'supp_table_heat_risk_ed_restricthosp.csv'))

################################################################################-
## 4a: Make supplemental table of no COVID model death mean temperature ----
################################################################################-

estimates_tmean_death_nocov <- list(
  full = est_cum_death_nocov_full_full_a_quad_ns_full_tmean)

format_supp_table_cum_tmean(estimates_tmean_death_nocov) %>%
  write_csv(., here::here('outputs', 'tables', 'supp_table_tmean_death_nocov.csv'))

################################################################################-
## 4b: Make supplemental table of no COVID model death HeatRisk ----
################################################################################-

estimates_heat_risk_death_nocov <- list(
  full = est_cum_death_nocov_full_full_a_strata_ns_full_heat_risk)

format_supp_table_cum_hr(estimates_heat_risk_death_nocov) %>%
  write_csv(., here::here('outputs', 'tables', 'supp_table_heat_risk_death_nocov.csv'))

################################################################################-
## 5: Clean up ----
################################################################################-

rm(list = ls())

# reload functions
source(here::here('R', '0_00_set_up.R'))
