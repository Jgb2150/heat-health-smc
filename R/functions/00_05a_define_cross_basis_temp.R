################################################################################-
# Function to set crossbasis for mean temperature analysis ----
# Comparing HeatRisk and temperature for local heat surveillance: A time-stratified 
#  case-crossover study of emergency department visits in San Mateo County, 
#  California, 2017–2025
# Juliana GE Bartels
# Updated: 2/11/2026
################################################################################-
## 0: Define model parameters ----
################################################################################-

# Model parameters:

## temp_analysis: 'full' = analysis of full year of data
##
## er_constraint: {'a', 'b', 'c', 'd', 'e'} = letter constraint for exposure-response dimension placement of knots
##
## er_type:       'ns' = natural spline exposure response
##                'quad' = quadratic exposure response
##                'cub' = cubic exposure response
##                'quar' = quartic exposure response
##
## lr_type:       'ns' = natural spline lag-response 
##
## dta:           'dta_full'
##
## temp_var:      'tmean' = mean daily temperature
##

################################################################################-
## 1: Define function ----
################################################################################-

define_cross_basis_temp <- function(temp_analysis, 
                                    er_constraint, 
                                    er_type, 
                                    lr_type, 
                                    dta, 
                                    temp_var) {

  ### Set number of lags
  numlag <- 7
  maxlag <- numlag - 1
  
  # 1a. Cross-basis for natural spline in both exposure-response and lag-response ----
  
  if(temp_analysis == 'full' & er_constraint == 'a') {
    cb.temp_ns <- crossbasis(
      dta[[temp_var]],
      group = dta$zip_code,
      lag = c(0, maxlag),
      argvar = list(fun = 'ns', knots = quantile(dta[[temp_var]], c(0.1, 0.75, 0.9), na.rm = TRUE), Bound = range(dta[[temp_var]], na.rm = T)), 
      arglag = list(fun = 'ns', knots = logknots(maxlag, nk = 3))
    )
  }
  
  if(temp_analysis == 'full' & er_constraint == 'b') {
    cb.temp_ns <- crossbasis(
      dta[[temp_var]],
      group = dta$zip_code,
      lag = c(0, maxlag),
      argvar = list(fun = 'ns', knots = quantile(dta[[temp_var]], c(0.1, 0.5, 0.9), na.rm = TRUE), Bound = range(dta[[temp_var]], na.rm = T)), 
      arglag = list(fun = 'ns', knots = logknots(maxlag, nk = 3))
    )
  }
  
  if(temp_analysis == 'full' & er_constraint == 'c') {
    cb.temp_ns <- crossbasis(
      dta[[temp_var]],
      group = dta$zip_code,
      lag = c(0, maxlag),
      argvar = list(fun = 'ns', knots = quantile(dta[[temp_var]], c(0.05, 0.5, 0.95), na.rm = TRUE), Bound = range(dta[[temp_var]], na.rm = T)), # try setting knots instead of degrees of freedom
      arglag = list(fun = 'ns', knots = logknots(maxlag, nk = 3))
    )
  }
  
  if(temp_analysis == 'full' & er_constraint == 'd') {
    cb.temp_ns <- crossbasis(
      dta[[temp_var]],
      group = dta$zip_code,
      lag = c(0, maxlag),
      argvar = list(fun = 'ns', knots = quantile(dta[[temp_var]], c(0.25, 0.5, 0.75), na.rm = TRUE), Bound = range(dta[[temp_var]], na.rm = T)), # try setting knots instead of degrees of freedom
      arglag = list(fun = 'ns', knots = logknots(maxlag, nk = 3))
    )
  }
  
  if(temp_analysis == 'full' & er_constraint == 'e') {
    cb.temp_ns <- crossbasis(
      dta[[temp_var]],
      group = dta$zip_code,
      lag = c(0, maxlag),
      argvar = list(fun = 'ns', knots = quantile(dta[[temp_var]], c(0.33, 0.5, 0.67), na.rm = TRUE), Bound = range(dta[[temp_var]], na.rm = T)), # try setting knots instead of degrees of freedom
      arglag = list(fun = 'ns', knots = logknots(maxlag, nk = 3))
    )
  }
  
  # 2. Cross-basis for quadratic exposure-response and natural spline in lag-response ----
  
  if(temp_analysis == 'full') {
    cb.temp_quad_ns <- crossbasis(
      dta[[temp_var]],
      group = dta$zip_code,
      lag = c(0, maxlag), 
      argvar = list(fun = 'poly', degree = 2),
      arglag = list(fun = 'ns', knots = logknots(maxlag, nk = 3))
    )
  }
  
  # 3. Cross-basis for cubic exposure-response and natural spline in lag-response ----
  
  if(temp_analysis == 'full') {
    cb.temp_cub_ns <- crossbasis(
      dta[[temp_var]],
      group = dta$zip_code,
      lag = c(0, maxlag), 
      argvar = list(fun = 'poly', degree = 3),
      arglag = list(fun = 'ns', knots = logknots(maxlag, nk = 3))
    )
  }
  
  # 4. Cross-basis for quartic exposure-response and natural spline in lag-response ----
  
  if(temp_analysis == 'full') {
    cb.temp_quar_ns <- crossbasis(
      dta[[temp_var]],
      group = dta$zip_code,
      lag = c(0, maxlag), 
      argvar = list(fun = 'poly', degree = 4),
      arglag = list(fun = 'ns', knots = logknots(maxlag, nk = 3))
    )
  }
  
  # 2. Return crossbasis for analysis
  return(list(
    cb.temp_ns = cb.temp_ns,
    cb.temp_quad_ns = cb.temp_quad_ns,
    cb.temp_cub_ns = cb.temp_cub_ns,
    cb.temp_quar_ns = cb.temp_quar_ns
    )
  )

}
    