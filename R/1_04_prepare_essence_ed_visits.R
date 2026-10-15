################################################################################-
# Pull all-cause ED visit ESSENCE data, 2017-2025 ----
# Comparing HeatRisk and temperature for local heat surveillance: A time-stratified 
#  case-crossover study of emergency department visits in San Mateo County, 
#  California, 2017–2025
# Juliana GE Bartels and Sayema Badar
# Updated: 6/30/2026
################################################################################-

# This file cleans and combines ED visit data from ESSENCE syndromic surveillance 
# for all SMC residents from 2017 to 2025. The output is a dataframe with daily 
# counts of all-cause ED visits by ZIP code, overall and by sub-group. 

################################################################################-
## 0: Load ESSENCE all-cause ED data ----
################################################################################-

# Data were pulled manually from ESSENCE syndromic surveillance platform for San Mateo County

load(here::here('data', 'raw', 'ed_visits', 'allcause_edvisits_2017.rda'))
load(here::here('data', 'raw', 'ed_visits', 'allcause_edvisits_2018.rda'))
load(here::here('data', 'raw', 'ed_visits', 'allcause_edvisits_2019.rda'))
load(here::here('data', 'raw', 'ed_visits', 'allcause_edvisits_2020.rda'))
load(here::here('data', 'raw', 'ed_visits', 'allcause_edvisits_2021.rda'))
load(here::here('data', 'raw', 'ed_visits', 'allcause_edvisits_2022.rda'))
load(here::here('data', 'raw', 'ed_visits', 'allcause_edvisits_2023.rda'))
load(here::here('data', 'raw', 'ed_visits', 'allcause_edvisits_2024.rda'))
load(here::here('data', 'raw', 'ed_visits', 'allcause_edvisits_2025.rda'))

# Connect to Azure
# This requires a personal password
con <- azure_connect(creds_file = here::here('creds', 'azure_epi-warehouse_creds.yml'))

# Load table of ICD 10 code categories 
icd10_ref <- tbl(con, in_schema("dbo", "icd10_categories")) %>%
  collect()

################################################################################-
## 1: Combine all years of ESSENCE syndromic surveillance data ----
################################################################################-

# Create list of ED visit annual files
ed_files <- list(allcause_edvisits_2017, allcause_edvisits_2018,
                 allcause_edvisits_2019, allcause_edvisits_2020, 
                 allcause_edvisits_2021, allcause_edvisits_2022, 
                 allcause_edvisits_2023, allcause_edvisits_2024,
                 allcause_edvisits_2025)

# Clean each ED visit annual file
ed_zip <- lapply(ed_files, function(file){
  file %>%
    clean_names()  %>%
    
      # De-duplicate visits
      # Create new hosp variable with abbreviated hospital names
      mutate(hosp = case_when(hospital_name == "CA_San_Mateo-Kfh Redwood City" ~ "krwc",
                              hospital_name == "CA_San_Mateo-Kfh South San Francisco" ~ "kssf",
                              hospital_name == "CA_San_Mateo-Mills-Peninsula Medical Center" ~ "mp",
                              hospital_name == "CA_San_Mateo-San Mateo Medical Center" ~ "smmc",
                              hospital_name == "CA_San_Mateo-Sequoia Hospital" ~ "seq",
                              hospital_name == "CA_San_Mateo-Seton Medical Center" ~ "sdc",
                              hospital_name == "CA_San_Mateo-Stanford Hospital And Clinics" ~ "stan"),
             visit_id = as.character(visit_id),
             # Number missing visit_id values as a separate variable so they are not affected by de-duplication
             visit_id2 = case_when(
               visit_id == "none" | is.na(visit_id) ~ paste0("na_", as.character(row_number())),
               TRUE ~ visit_id)) %>% 
      # De-duplicate to keep the earlier of the duplicate records
      group_by(hosp, visit_id2) %>% 
      arrange(c_visit_date_time) %>% 
      slice(1) %>% 
      ungroup() %>%
    
    # Select variables
    dplyr::select(date, zip_code, age, sex, hosp, dd_parsed) %>%
    
    # Add primary cause of ED visit
    mutate(
      # Isolate first ICD code (we will treat this as primary cause of visit(cov))
      primary_cause = str_extract(dd_parsed, "(?<=;)[^;]+(?=;)"),
      # Remove semicolons, periods, extra spaces and limit ICD10 to 4 characters
      primary_cause_clean =  str_sub(str_replace_all(primary_cause, "\\.|\\s", ""), 1, 4),
      # Right pad with zeroes so all the codes are the same units
      primary_cause_clean = str_pad(primary_cause_clean, 4, side = "right", pad = "0"), 
      # Split the code into characters and numbers
      primary_cause_clean_char = str_sub(primary_cause_clean, 1, 1),
      primary_cause_clean_num = as.numeric(str_sub(primary_cause_clean, 2, 4))
    ) %>% 
    left_join(icd10_ref,
              by = join_by(
                primary_cause_clean_char == icd_char,
                between(primary_cause_clean_num, start_num, end_num)
              ),
              relationship = "many-to-many"
    ) %>%
    mutate(ed_cat = case_when(
      primary_cause_clean_char %in% c("S", "T", "W") ~ "Unintentional injury",
      primary_cause_clean_char %in% c("Z") ~ "Factors influencing health status")) %>%
    
    # Create new variables for ED visits by category
    mutate(ed_vis = 1,
           # Sex
           female = ifelse(sex == "F", 1, 0),
           male = ifelse(sex == "M", 1, 0),
           
           # Age group
           age_under5 = ifelse(age < 5, 1, 0),
           age_5to17 = ifelse(age >= 5 & age < 18, 1, 0),
           age_18to64 = ifelse(age >= 18 & age < 65, 1, 0),
           age_65plus = ifelse(age >= 65, 1, 0),
           
           # Region
           coastside = ifelse(zip_code %in% c(94018, 94019, 94020, 94021, 94037, 
                                              94038, 94060, 94074), 1, 0),
           midcounty = ifelse(zip_code %in% c(94002, 94010, 94065, 94070, 94401,
                                              94402, 94403, 94404), 1, 0),
           northcounty = ifelse(zip_code %in% c(94005, 94014, 94015, 94030, 94044,
                                                94066, 94080, 94128), 1, 0),
           southcounty = ifelse(zip_code %in% c(94025, 94027, 94028, 94061, 94062,
                                                94063, 94303), 1, 0),
           
           # Cause of ED visit
           cause_cardiovascular = ifelse(smc_color == "Heart disease", 1, 0),
           cause_respiratory = ifelse(who_dx == "Respiratory infections" | who_dx == "Respiratory diseases", 1, 0),
           cause_stroke = ifelse(smc_cat == "Stroke", 1, 0),
           cause_intentinjury = ifelse(smc_cat == "Intentional injuries" | smc_cat == "Intentional self-harm (suicide)", 1, 0),
           cause_unintentinjury = ifelse(smc_cat == "Unintentional injuries" | ed_cat == "Unintentional injury", 1, 0),
           cause_heat = ifelse(smc_detail == "Heat-related", 1, 0),
           cause_substance = ifelse(smc_cat == "Alcohol use and alcohol use disorders" | smc_cat == "Drug use and drug use disorders", 1, 0),
           cause_mental = ifelse(ccb_cat == "Mental Health disorders", 1, 0),
           cause_renal = ifelse(smc_detail == "Other chronic kidney disease (MS)", 1, 0),
           cause_genitourinary = ifelse(smc_cat == "Genitourinary diseases", 1, 0),
           
           # Facility
           hosp_krwc = ifelse(hosp == 'krwc', 1, 0),
           hosp_kssf = ifelse(hosp == 'kssf', 1, 0),
           hosp_mp = ifelse(hosp == 'mp', 1, 0),
           hosp_smmc = ifelse(hosp == 'smmc', 1, 0),
           hosp_seq = ifelse(hosp == 'seq', 1, 0),
           hosp_sdc = ifelse(hosp == 'sdc', 1, 0),
           hosp_stan = ifelse(hosp == 'stan', 1, 0),
           
           # Cause within age under 5
           causeunder5_cardiovascular = ifelse(age < 5 & cause_cardiovascular == 1, 1, 0),
           causeunder5_respiratory = ifelse(age < 5 & cause_respiratory == 1, 1, 0),
           causeunder5_stroke = ifelse(age < 5 & cause_stroke == 1, 1, 0),
           causeunder5_intentinjury = ifelse(age < 5 & cause_intentinjury == 1, 1, 0),
           causeunder5_unintentinjury = ifelse(age < 5 & cause_unintentinjury == 1, 1, 0),
           causeunder5_heat = ifelse(age < 5 & cause_heat == 1, 1, 0),
           causeunder5_substance = ifelse(age < 5 & cause_substance == 1, 1, 0),
           causeunder5_mental = ifelse(age < 5 & cause_mental == 1, 1, 0),
           causeunder5_renal = ifelse(age < 5 & cause_renal == 1, 1, 0),
           causeunder5_genitourinary = ifelse(age < 5 & cause_genitourinary == 1, 1, 0),
           
           # Cause within age 5 to 17
           cause5to17_cardiovascular = ifelse((age >= 5 & age < 18) & cause_cardiovascular == 1, 1, 0),
           cause5to17_respiratory = ifelse((age >= 5 & age < 18) & cause_respiratory == 1, 1, 0),
           cause5to17_stroke = ifelse((age >= 5 & age < 18) & cause_stroke == 1, 1, 0),
           cause5to17_intentinjury = ifelse((age >= 5 & age < 18) & cause_intentinjury == 1, 1, 0),
           cause5to17_unintentinjury = ifelse((age >= 5 & age < 18) & cause_unintentinjury == 1, 1, 0),
           cause5to17_heat = ifelse((age >= 5 & age < 18) & cause_heat == 1, 1, 0),
           cause5to17_substance = ifelse((age >= 5 & age < 18) & cause_substance == 1, 1, 0),
           cause5to17_mental = ifelse((age >= 5 & age < 18) & cause_mental == 1, 1, 0),
           cause5to17_renal = ifelse((age >= 5 & age < 18) & cause_renal == 1, 1, 0),
           cause5to17_genitourinary = ifelse((age >= 5 & age < 18) & cause_genitourinary == 1, 1, 0),
           
           # Cause within age 18 to 64
           cause18to64_cardiovascular = ifelse((age >= 18 & age < 65) & cause_cardiovascular == 1, 1, 0),
           cause18to64_respiratory = ifelse((age >= 18 & age < 65) & cause_respiratory == 1, 1, 0),
           cause18to64_stroke = ifelse((age >= 18 & age < 65) & cause_stroke == 1, 1, 0),
           cause18to64_intentinjury = ifelse((age >= 18 & age < 65) & cause_intentinjury == 1, 1, 0),
           cause18to64_unintentinjury = ifelse((age >= 18 & age < 65) & cause_unintentinjury == 1, 1, 0),
           cause18to64_heat = ifelse((age >= 18 & age < 65) & cause_heat == 1, 1, 0),
           cause18to64_substance = ifelse((age >= 18 & age < 65) & cause_substance == 1, 1, 0),
           cause18to64_mental = ifelse((age >= 18 & age < 65) & cause_mental == 1, 1, 0),
           cause18to64_renal = ifelse((age >= 18 & age < 65) & cause_renal == 1, 1, 0),
           cause18to64_genitourinary = ifelse((age >= 18 & age < 65) & cause_genitourinary == 1, 1, 0),
           
           # Cause within age over 65
           cause65plus_cardiovascular = ifelse((age > 65) & cause_cardiovascular == 1, 1, 0),
           cause65plus_respiratory = ifelse((age > 65) & cause_respiratory == 1, 1, 0),
           cause65plus_stroke = ifelse((age > 65) & cause_stroke == 1, 1, 0),
           cause65plus_intentinjury = ifelse((age > 65) & cause_intentinjury == 1, 1, 0),
           cause65plus_unintentinjury = ifelse((age > 65) & cause_unintentinjury == 1, 1, 0),
           cause65plus_heat = ifelse((age > 65) & cause_heat == 1, 1, 0),
           cause65plus_substance = ifelse((age > 65) & cause_substance == 1, 1, 0),
           cause65plus_mental = ifelse((age > 65) & cause_mental == 1, 1, 0),
           cause65plus_renal = ifelse((age > 65) & cause_renal == 1, 1, 0),
           cause65plus_genitourinary = ifelse((age > 65) & cause_genitourinary == 1, 1, 0),
           
           # Restricted hospitals
           ed_restricthosp = ifelse(hosp %in% c('krwc', 'kssf', 'smmc', 'stan'), 1, 0)
           
    ) %>%
    group_by(date, zip_code) %>%
    summarize(ed_count = sum(ed_vis),
              # Sex
              ed_count_sex_f = sum(female),
              ed_count_sex_m = sum(male),
              
              # Age group
              ed_count_age_under5 = sum(age_under5),
              ed_count_age_5to17 = sum(age_5to17),
              ed_count_age_18to64 = sum(age_18to64),
              ed_count_age_65plus = sum(age_65plus),
              
              # Region
              ed_count_region_coastside = sum(coastside),
              ed_count_region_mid = sum(midcounty),
              ed_count_region_north = sum(northcounty),
              ed_count_region_south = sum(southcounty),
              
              # Cause of ED visit
              ed_count_cause_cardiovascular = sum(cause_cardiovascular, na.rm = TRUE),
              ed_count_cause_respiratory = sum(cause_respiratory, na.rm = TRUE),
              ed_count_cause_stroke = sum(cause_stroke, na.rm = TRUE),
              ed_count_cause_intentinjury = sum(cause_intentinjury, na.rm = TRUE),
              ed_count_cause_unintentinjury = sum(cause_unintentinjury, na.rm = TRUE),
              ed_count_cause_heat = sum(cause_heat, na.rm = TRUE),
              ed_count_cause_substance = sum(cause_substance, na.rm = TRUE),
              ed_count_cause_mental = sum(cause_mental, na.rm = TRUE),
              ed_count_cause_renal = sum(cause_renal, na.rm = TRUE),
              ed_count_cause_genitourinary = sum(cause_genitourinary, na.rm = TRUE),
              
              # Facility
              ed_count_hosp_krwc = sum(hosp_krwc, na.rm = TRUE),
              ed_count_hosp_kssf = sum(hosp_kssf, na.rm = TRUE),
              ed_count_hosp_mp = sum(hosp_mp, na.rm = TRUE),
              ed_count_hosp_smmc = sum(hosp_smmc, na.rm = TRUE),
              ed_count_hosp_seq = sum(hosp_seq, na.rm = TRUE),
              ed_count_hosp_sdc = sum(hosp_sdc, na.rm = TRUE),
              ed_count_hosp_stan = sum(hosp_stan, na.rm = TRUE),
              
              # Cause of ED visit under 5
              ed_count_causeunder5_cardiovascular = sum(causeunder5_cardiovascular, na.rm = TRUE),
              ed_count_causeunder5_respiratory = sum(causeunder5_respiratory, na.rm = TRUE),
              ed_count_causeunder5_stroke = sum(causeunder5_stroke, na.rm = TRUE),
              ed_count_causeunder5_intentinjury = sum(causeunder5_intentinjury, na.rm = TRUE),
              ed_count_causeunder5_unintentinjury = sum(causeunder5_unintentinjury, na.rm = TRUE),
              ed_count_causeunder5_heat = sum(causeunder5_heat, na.rm = TRUE),
              ed_count_causeunder5_substance = sum(causeunder5_substance, na.rm = TRUE),
              ed_count_causeunder5_mental = sum(causeunder5_mental, na.rm = TRUE),
              ed_count_causeunder5_renal = sum(causeunder5_renal, na.rm = TRUE),
              ed_count_causeunder5_genitourinary = sum(causeunder5_genitourinary, na.rm = TRUE),
              
              # Cause of ED visit 5 to 17
              ed_count_cause5to17_cardiovascular = sum(cause5to17_cardiovascular, na.rm = TRUE),
              ed_count_cause5to17_respiratory = sum(cause5to17_respiratory, na.rm = TRUE),
              ed_count_cause5to17_stroke = sum(cause5to17_stroke, na.rm = TRUE),
              ed_count_cause5to17_intentinjury = sum(cause5to17_intentinjury, na.rm = TRUE),
              ed_count_cause5to17_unintentinjury = sum(cause5to17_unintentinjury, na.rm = TRUE),
              ed_count_cause5to17_heat = sum(cause5to17_heat, na.rm = TRUE),
              ed_count_cause5to17_substance = sum(cause5to17_substance, na.rm = TRUE),
              ed_count_cause5to17_mental = sum(cause5to17_mental, na.rm = TRUE),
              ed_count_cause5to17_renal = sum(cause5to17_renal, na.rm = TRUE),
              ed_count_cause5to17_genitourinary = sum(cause5to17_genitourinary, na.rm = TRUE),
              
              # Cause of ED visit 18 to 64
              ed_count_cause18to64_cardiovascular = sum(cause18to64_cardiovascular, na.rm = TRUE),
              ed_count_cause18to64_respiratory = sum(cause18to64_respiratory, na.rm = TRUE),
              ed_count_cause18to64_stroke = sum(cause18to64_stroke, na.rm = TRUE),
              ed_count_cause18to64_intentinjury = sum(cause18to64_intentinjury, na.rm = TRUE),
              ed_count_cause18to64_unintentinjury = sum(cause18to64_unintentinjury, na.rm = TRUE),
              ed_count_cause18to64_heat = sum(cause18to64_heat, na.rm = TRUE),
              ed_count_cause18to64_substance = sum(cause18to64_substance, na.rm = TRUE),
              ed_count_cause18to64_mental = sum(cause18to64_mental, na.rm = TRUE),
              ed_count_cause18to64_renal = sum(cause18to64_renal, na.rm = TRUE),
              ed_count_cause18to64_genitourinary = sum(cause18to64_genitourinary, na.rm = TRUE),
              
              # Cause of ED visit over 65
              ed_count_cause65plus_cardiovascular = sum(cause65plus_cardiovascular, na.rm = TRUE),
              ed_count_cause65plus_respiratory = sum(cause65plus_respiratory, na.rm = TRUE),
              ed_count_cause65plus_stroke = sum(cause65plus_stroke, na.rm = TRUE),
              ed_count_cause65plus_intentinjury = sum(cause65plus_intentinjury, na.rm = TRUE),
              ed_count_cause65plus_unintentinjury = sum(cause65plus_unintentinjury, na.rm = TRUE),
              ed_count_cause65plus_heat = sum(cause65plus_heat, na.rm = TRUE),
              ed_count_cause65plus_substance = sum(cause65plus_substance, na.rm = TRUE),
              ed_count_cause65plus_mental = sum(cause65plus_mental, na.rm = TRUE),
              ed_count_cause65plus_renal = sum(cause65plus_renal, na.rm = TRUE),
              ed_count_cause65plus_genitourinary = sum(cause65plus_genitourinary, na.rm = TRUE),
              
              # Restricted hospitals
              ed_count_restricthosp = sum(ed_restricthosp, na.rm = TRUE)
              
    ) %>%
    mutate(date = mdy(date),
           zip_code = as.numeric(zip_code)
           )
})

# Create dataframe for all ED visits in all years
ed_zip <- bind_rows(ed_zip)

################################################################################-
## 3: Export prepared ed data by zip code ----
################################################################################-

ed_zip %>%
  write_csv(here::here(
    "data", "intermediate", "ed_zip.csv")
  )

################################################################################-
## 4: Clean up ----
################################################################################-

rm(ed_zip, ed_files, 
   allcause_edvisits_2017, allcause_edvisits_2018,
   allcause_edvisits_2019, allcause_edvisits_2020, 
   allcause_edvisits_2021, allcause_edvisits_2022, 
   allcause_edvisits_2023, allcause_edvisits_2024, 
   allcause_edvisits_2025,
   icd10_ref)

