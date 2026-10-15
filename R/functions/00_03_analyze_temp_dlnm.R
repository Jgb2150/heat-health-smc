################################################################################-
# Function to analyze association between temperature and ED visits ----
# Comparing HeatRisk and temperature for local heat surveillance: A time-stratified 
#  case-crossover study of emergency department visits in San Mateo County, 
#  California, 2017–2025
# Juliana GE Bartels
# Updated: 5/22/2026
################################################################################-
## 0: Define model parameters ----
################################################################################-

# Model parameters:
## sensitivity:   'main' = main model
##                'nocov' = without years 2020 and 2021
##                'holiday' = with variable for holidays
##
## subset_var:    'full' = full dataset
##                'sex' = by gender
##                'age' = by age
##                'region' = by region
##                'cause' = by cause of ED visit
##                'hosp' = hospital facility
##
## subset:        'full' = full dataset
##                'f' = female
##                'm' = male
##                'under5' = under age 5
##                '5to17' = under age 65
##                '18 to 64' = age 15 to 65
##                '65plus' = age over 65
##                'coastside' = Coastside region
##                'mid' = Mid-county region
##                'north' = North county region
##                'south' = South county region
##                'cardiovascular' = cardiovascular
##                'respiratory' = respiratory
##                'stroke' = stroke
##                'intentinjury' = intentional injury
##                'unintentinjury' = unintentional injury
##                'heat' = heat-related
##                'substance' = substance use
##                'mental' = mental health
##                'renal' = renal
##                'genitourinary' = genitourinary
##                'krwc' = Kaiser Permanente Redwood City
##                'kssf' = Kaiser Permanente South San Francisco
##                'mp' = Mills Peninsula
##                'smmc' = San Mateo Medical Cetner
##                'seq' = Sequoia 
##                'sdc' = Seton
##                'stan' = Stanford Medical Center
##
## outcome:       'ed' = ED visit
##                'death' = death
##
## er_constraint: {'a', 'b', 'c', 'd', 'e'} = letter constraint for exposure-response dimension placement of knots
##                x = where no letter constraint is needed
##
## er_type:       'ns' = natural spline exposure response 
##                'quad' = quadratic exposure response
##                'cub' = cubic exposure response
##                'quar' = quartic exposure response
##                'strata' = strata exposure-response
##
## lr_type:       'ns' = natural spline lag-response
##
## temp_analysis: 'full' = analysis full year dataframe (mean temp)
##
## save_model:    'save_qaic' = save the QAIC in the QAIC table
##                'save_model' = save the model in the models folder and effect estimates in the estimates folder
## 
## dta:           'dta_full'
##
## temp_var:      'tmean' = mean daily temperature
##                'heat_risk' = HeatRisk score
##
## pop_denom:     'total_pop_all' = full dataset
##                'total_pop_f' = female
##                'total_pop_m' = male
##                'total_pop_under5' = under age 5
##                'total_pop_5to17' = age 5 to 17
##                'total_pop_18tot64' = age 18 to 64
##                'total_pop_65plus' = age over 65
##                'total_pop_coastside' = coastside region
##                'total_pop_mid' = mid-county region
##                'total_pop_north' = north county region
##                'total_pop_south' = south county region
##
## outcome_var:   ('ed_count_', subset_var, '_', subset)
##
## data_source:   'original' = original, private data
##                'synth' = synthetic data

################################################################################-
## 1: Define function ----
################################################################################-

# Analyze temperature dlnm function ----
analyze_temp_dlnm <- function(sensitivity, 
                              subset_var,
                              subset,
                              outcome,
                              er_constraint, 
                              er_type, 
                              lr_type, 
                              temp_analysis,
                              save_model, 
                              dta, 
                              temp_var,
                              pop_denom,
                              outcome_var,
                              data_source
                              ) {
  
  # 1. Name model ----
  model_name <- paste(outcome, sensitivity, subset_var, subset, er_constraint,
                      er_type, lr_type, temp_analysis, temp_var,
                      sep = '_')
  
  # 2. Run model ----
  mod <- run_model(sensitivity, subset, outcome_var, pop_denom, dta, temp_var, temp_analysis,
            er_constraint, er_type, lr_type)
  
  # 3. Save model QAIC ----
  if(save_model == 'save_qaic') {
    save_qaic(mod, 
              sensitivity, 
              subset_var, 
              subset, 
              er_constraint,
              er_type, 
              lr_type, 
              temp_analysis, 
              temp_var,
              outcome,
              data_source)
  } 
  
  # 4. Save model ----
  if(save_model == 'save_model') {
    save_model(mod, 
               mod$cb.temp, 
               model_name, 
               temp_analysis, 
               temp_var,
               data_source)
  } 
  
}


