################################################################################-
# Function to set crossbasis for mortality analysis, heat risk ----
# Comparing HeatRisk and temperature for local heat surveillance: A time-stratified 
#  case-crossover study of emergency department visits in San Mateo County, 
#  California, 2017–2025
# Juliana GE Bartels
# Updated: 3/3/2026
################################################################################-
## 0: Define model parameters ----
################################################################################-

## temp_analysis: 'full' = analysis of full year of data
##
## dta:           'dta_full'
##
## temp_var:      'heat_risk' = HeatRisk score
##                

################################################################################-
## 1: Define function ----
################################################################################-

define_cross_basis_hr <- function(temp_analysis, 
                                  dta, 
                                  temp_var) {

  ### Set number of lags
  numlag <- 7
  maxlag <- numlag - 1
  
  # 1a. Cross-basis ----
  
  if(temp_analysis == 'full') {
    cb.temp_strata_ns <- crossbasis(
      factor(dta[[temp_var]]),
      group = dta$zip_code,
      lag = c(0, maxlag),
      argvar = list(fun = 'strata', breaks = c(1, 2, 3)), 
      arglag = list(fun = 'ns', knots = logknots(maxlag, nk = 3))
    )
  }
  
  # 2. Return crossbasis for analysis
  return(list(
    cb.temp_strata_ns = cb.temp_strata_ns
    )
  )

}
    