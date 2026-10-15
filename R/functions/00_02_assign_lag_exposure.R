################################################################################-
# Function to assign lagged temperature exposure
# Comparing HeatRisk and temperature for local heat surveillance: A time-stratified 
#  case-crossover study of emergency department visits in San Mateo County, 
#  California, 2017–2025
# Juliana GE Bartels
# Updated: 6/16/2026
################################################################################-
## 0: Define model parameters ----
################################################################################-

# Model parameters:
## exposure_df:   'lagged_exp_df' = dataframe of exposures

################################################################################-
## 1: Define function ----
################################################################################-

assign_lag_exposure <- function(exposure_df, i) {

  # Create dataframe of lagged temperature and heat index exposures
  exposure_df <- exposure_df %>%
    mutate(
        !!paste0("tmax_", i) := lag(tmax, n = i),
        !!paste0("tmin_", i) := lag(tmin, n = i),
        !!paste0("tmean_", i) := lag(tmean, n = i),
        !!paste0("heat_risk_", i) := lag(heat_risk, n = i)
      )
    
}
  
