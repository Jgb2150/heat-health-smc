################################################################################-
# Function to calculate and save QAIC for main ED visit model ----
# Comparing HeatRisk and temperature for local heat surveillance: A time-stratified 
#  case-crossover study of emergency department visits in San Mateo County, 
#  California, 2017–2025 
# Juliana GE Bartels
# Updated: 6/2/2026
################################################################################-
## 0: Define model parameters ----
################################################################################-

# Model parameters 

## mod:           'mod' = model output
##
## sensitivity:   'main' = main model
##                'nocov' = without years 2020 and 2021
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
##                'non-accidental' = non-accidental mortality/morbidity
##                'cardiovascular' = cardiovascular
##                'respiratory' = respiratory
##                'stroke' = stroke
##                'injury' = injury
##                'krwc' = Kaiser Permanente Redwood City
##                'kssf' = Kaiser Permanente South San Francisco
##                'mp' = Mills Peninsula
##                'smmc' = San Mateo Medical Cetner
##                'seq' = Sequoia 
##                'sdc' = Seton
##                'stan' = Stanford Medical Center
##
## er_constraint: {'a', 'b', 'c', 'd', 'e'} = letter constraint for exposure-response dimension placement of knots
##
## er_type:       'ns' = natural spline exposure response 
##                'quad' = quadratic exposure response
##                'cub' = cubic exposure response
##                'quar' = quartic exposure response
##                'strata' = strata exposure-response
##
## lr_type:       'cons' = constant lag-response
##                'ns' = natural spline lag-response with lr_constraint df
##
## temp_analysis: 'full' = analysis full year dataframe (mean temp)
##
## temp_var:      'tmean' = mean daily temperature
##                'heat_risk' = HeatRisk score
##
## outcome:       'ed' = ED visit
##                'death' = death
## 
## data_source:   'original' = original, private data
##                'synth' = synthetic data

################################################################################-
## 1: Define function ----
################################################################################-

save_qaic <- function(mod, 
                      sensitivity, 
                      subset_var, 
                      subset, 
                      er_constraint, 
                      er_type, 
                      lr_type, 
                      temp_analysis, 
                      temp_var, 
                      outcome,
                      data_source) {
  
  # Generate suffix for file calling and writing, based on data source
  data_source_name <- case_when(data_source == 'original' ~ '',
                                data_source == 'synth' ~ '_synth')
  
  # Upload template QAIC table
  qaic_table <- read_csv(here::here('outputs', 'tables',
                                         paste0('model_QAIC_', outcome, data_source_name, '.csv')),
                                 col_types = 'ccccccccdT')
  
  # Calculate QAIC using AIC you would get if the distribution were Poisson, 
  # then account for overdispersion (this is from MuMIn package, via Rowland)
  x.quasipoisson <- function(...) {
    res <- quasipoisson(...)
    res$aic <- poisson(...)$aic
    res
  }
  mod1 <- update(mod, family = x.quasipoisson)
  qaic_mod <- QAIC(mod1, chat = summary(mod1)$dispersion)
  
  qaic_table[1+nrow(qaic_table),] <- list(sensitivity, subset_var, subset, 
                                                      er_constraint, 
                                                      er_type, lr_type, temp_analysis, temp_var, 
                                                      qaic_mod,
                                                      Sys.time()
  )
  
  qaic_table %>% 
    group_by(sensitivity, subset_var, subset, er_constraint, er_type, lr_type, temp_analysis, temp_var) %>% 
    arrange(desc(run_date)) %>% 
    slice(1) %>% 
    filter(!is.na(sensitivity)) %>%
    write_csv(here::here('outputs', 'tables',
                          paste0('model_QAIC_', outcome, data_source_name, '.csv')))

}
