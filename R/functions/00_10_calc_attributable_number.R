################################################################################-
# Function to calculate attributable number of deaths or ED visits ----
# Comparing HeatRisk and temperature for local heat surveillance: A time-stratified 
#  case-crossover study of emergency department visits in San Mateo County, 
#  California, 2017–2025 
# Juliana GE Bartels
# Updated: 9/29/2026
################################################################################-
## 0: Define model parameters ----
################################################################################-

# Model parameters

## temp_var:      'tmean' = mean daily temperature
##                'heat_risk' = HeatRisk score
##
## cb.temp:       'mod$cb.temp' = cross basis stored in model output
##
## mod:           'mod' = model output
##
## temp_analysis: 'full' = analysis full year dataframe (mean temp)
##
## model_name:    model name saved as 'outcome_sensitivity_subset_var_subset_er_constraint_er_type_lr_type_temp_analysis_temp_var
##

################################################################################-
## 1: Define function ----
################################################################################-

calc_attributable_number <- function(temp_var, 
                                     cb.temp, 
                                     mod, 
                                     temp_analysis, 
                                     model_name) {
  
  # Cross reduce for cumulative exposure-response ----
  
  if(temp_var == 'tmean') {
    cen_val <- median(mod$data[[temp_var]])
  }
  
  if(temp_var == 'heat_risk') {
    cen_val <- 0
  }
  
  cb.temp <- mod$cb.temp
  
  cr <- crossreduce(basis = cb.temp,
                    model = mod,
                    cen = cen_val,
                    lag = 6, # setting the lag to 6 days (days 0-6)
                    type = 'overall'
                    )
  
  # Calculate minimum mortality temperature (or morbidity) of cross-reduction
  mmt <-  cr$predvar[which.min(cr$RRfit)]
  
  # Extract coefficients and covariance matrix
  coef <- coef(cr)  
  vcov <- vcov(cr)
  
  if(temp_var == 'tmean') {
    
    # For natural spline models
    if(attr(mod$cb.temp, "argvar")$fun == 'ns') {
      knots <- attr(mod$cb.temp, "argvar")$knots
      
      bvar <- onebasis(mod$data[[temp_var]], 
                   fun="ns", 
                   knots = knots,
                   Bound=attr(mod$cb.temp, "range"))
  
      # re-center on cen_val
      cenvec <- onebasis(cen_val, 
                         fun ="ns", 
                         knots = knots,
                         Bound=attr(mod$cb.temp, "range"))
    
      bvarcen <- scale(bvar, center=cenvec, scale=F)
    }
    
    # For polynomial models
    if(attr(mod$cb.temp, "argvar")$fun == 'poly') {

      bvar <- onebasis(mod$data[[temp_var]], 
                       fun="poly", 
                       degree = attr(mod$cb.temp,"argvar")$degree,
                       scale = attr(mod$cb.temp,"argvar")$scale
                       )
      
      # re-center on cen_val
      cenvec <- onebasis(cen_val, 
                         fun="poly", 
                         degree = attr(mod$cb.temp,"argvar")$degree,
                         scale = attr(mod$cb.temp,"argvar")$scale
                         )
      
      bvarcen <- scale(bvar, center=cenvec, scale=F)
    }
  }
  
  if(temp_var == 'heat_risk') {
    bvar <- onebasis(mod$data[[temp_var]], 
                     fun="strata",
                     breaks = c(1, 2, 3))
    
    # re-center on cen_val
    cenvec <- onebasis(cen_val, 
                       fun ="strata",
                       breaks = c(1, 2, 3))
    
    bvarcen <- scale(bvar, center=cenvec, scale=F)
  }
  
  # Calculate attributable number of deaths
  outcome_var <- all.vars(formula(mod))[1]
  an <- as.data.frame(mod$data[[outcome_var]] * (1-exp(-bvarcen %*% coef)))
  names(an)[1] <- paste0("an")
  
  an_comb <- mod$data %>% 
    dplyr::select(date, year, month, zip_code, all_of(temp_var), heat_risk, stratum, all_of(outcome_var)) %>% 
    bind_cols(an)
  
  # Create 1000 samples of model coefficients from a multivariate normal distribution
  N.samples <- 1000
  set.seed(2026)
  coefsim <- mvrnorm(N.samples, coef, vcov)
  
  # Estimate the attributable number using each of the 1000 sampled coefficients
  
  ## All "non-optimal" temperatures
  an_all <- sapply(seq(N.samples), function(i) {
    an <- mod$data[[outcome_var]] * (1-exp(-bvarcen %*% coefsim[i,]))
    sum(an, na.rm = TRUE)
  })
  
  ## All temperatures greater than the centering value (heat)
  
  if(temp_var == 'tmean') {
    cen_val <- median(mod$data[[temp_var]])

    an_heat <- sapply(seq(N.samples), function(i) {
      an <- mod$data[[outcome_var]] * (1-exp(-bvarcen %*% coefsim[i,]))
      sum(an[mod$data[[temp_var]] > cen_val], na.rm = TRUE)
    })
  }
  
  if(temp_var == 'heat_risk') {
    cen_val <- 1
    
    an_heat <- sapply(seq(N.samples), function(i) {
      an <- mod$data[[outcome_var]] * (1-exp(-bvarcen %*% coefsim[i,]))
      sum(an[mod$data[[temp_var]] > cen_val], na.rm = TRUE)
    })
  }
  
  ## All temperatures less than the centering value (cold)
  if(temp_var == 'tmean') {
    cen_val <- median(mod$data[[temp_var]])
    
    an_cold <- sapply(seq(N.samples), function(i) {
      an <- mod$data[[outcome_var]] * (1-exp(-bvarcen %*% coefsim[i,]))
      sum(an[mod$data[[temp_var]] < cen_val], na.rm = TRUE)
    })
  }
  
  if(temp_var == 'heat_risk') {
    an_cold <- rep(NA, 1000)
  }
  
  ## All temperatures greater than the extreme heat centering value (ext_heat)
  if(temp_var == 'tmean') {
    
    # setting extreme heat centering value as 95th percentile
    ext_heat_cen_val <- quantile(mod$data[[temp_var]], 0.95)
  
    an_ext_heat <- sapply(seq(N.samples), function(i) {
      an <- mod$data[[outcome_var]] * (1-exp(-bvarcen %*% coefsim[i,]))
      sum(an[mod$data[[temp_var]] > ext_heat_cen_val], na.rm = TRUE)
    })
  }
  
  if(temp_var == 'heat_risk') {
    ext_heat_cen_val <- 2
    
    an_ext_heat <- sapply(seq(N.samples), function(i) {
      an <- mod$data[[outcome_var]] * (1-exp(-bvarcen %*% coefsim[i,]))
      sum(an[mod$data[[temp_var]] > ext_heat_cen_val], na.rm = TRUE)
    })
  }
  
  ## All temperatures less than the extreme cold centering value (ext_cold)
  
  if(temp_var == 'tmean'){
    
    # setting extreme cold centering value as 5th percentile
    ext_cold_cen_val <- quantile(mod$data[[temp_var]], 0.05)
    
    an_ext_cold <- sapply(seq(N.samples), function(i) {
      an <- mod$data[[outcome_var]] * (1-exp(-bvarcen %*% coefsim[i,]))
      sum(an[mod$data[[temp_var]] < ext_cold_cen_val], na.rm = TRUE)
    })
  }
  
  if(temp_var == 'heat_risk'){
    ext_cold_cen_val <- NA
    an_ext_cold <- rep(NA, 1000)
  }
  
  # Create table of final results
  an_calc <- tibble(
         model_name = model_name, 
         outcome_var = outcome_var,
         temp_analysis = temp_analysis,
         temp_var = temp_var,
         cen = case_when(temp_var == 'tmean' ~ 'median', 
                         temp_var == 'heat_risk' ~ "0"),
         mmt = mmt,
         median = median(mod$data[[temp_var]], na.rm = TRUE),
         perc_95 = quantile(mod$data[[temp_var]], 0.95, na.rm = TRUE),
         perc_05 = quantile(mod$data[[temp_var]], 0.05, na.rm = TRUE),
         num_cases = sum(an_comb[[outcome_var]], na.rm = TRUE),
         
         an_all_est = sum(an_comb$an, na.rm = TRUE),
         an_all_lci = quantile(unlist(an_all), c(0.025), na.rm = TRUE),
         an_all_med = quantile(unlist(an_all), c(0.5), na.rm = TRUE),
         an_all_uci = quantile(unlist(an_all), c(0.975), na.rm = TRUE),
         af_all_est = (an_all_est/sum(an_comb[[outcome_var]], na.rm = TRUE)*100),
         af_all_lci = (quantile(unlist(an_all)/sum(an_comb[[outcome_var]], na.rm = TRUE), 0.025)*100),
         af_all_med = (quantile(unlist(an_all)/sum(an_comb[[outcome_var]], na.rm = TRUE), 0.5)*100),
         af_all_uci = (quantile(unlist(an_all)/sum(an_comb[[outcome_var]], na.rm = TRUE), 0.975)*100),
         
         an_heat_est = sum(an_comb$an[mod$data[[temp_var]] > cen_val], na.rm = TRUE),
         an_heat_lci = quantile(unlist(an_heat), c(0.025), na.rm = TRUE),
         an_heat_med = quantile(unlist(an_heat), c(0.5), na.rm = TRUE),
         an_heat_uci = quantile(unlist(an_heat), c(0.975), na.rm = TRUE),
         af_heat_est = (an_heat_est/sum(an_comb[[outcome_var]], na.rm = TRUE)*100),
         af_heat_lci = (quantile(unlist(an_heat)/sum(an_comb[[outcome_var]], na.rm = TRUE), 0.025, na.rm = TRUE)*100),
         af_heat_med = (quantile(unlist(an_heat)/sum(an_comb[[outcome_var]], na.rm = TRUE), 0.5, na.rm = TRUE)*100),
         af_heat_uci = (quantile(unlist(an_heat)/sum(an_comb[[outcome_var]], na.rm = TRUE), 0.975, na.rm = TRUE)*100),
         
         an_cold_est = sum(an_comb$an[mod$data[[temp_var]] < cen_val], na.rm = TRUE),
         an_cold_lci = quantile(unlist(an_cold), c(0.025), na.rm = TRUE),
         an_cold_med = quantile(unlist(an_cold), c(0.5), na.rm = TRUE),
         an_cold_uci = quantile(unlist(an_cold), c(0.975), na.rm = TRUE),
         af_cold_est = (an_cold_est/sum(an_comb[[outcome_var]], na.rm = TRUE)*100),
         af_cold_lci = (quantile(unlist(an_cold)/sum(an_comb[[outcome_var]], na.rm = TRUE), 0.025, na.rm = TRUE)*100),
         af_cold_med = (quantile(unlist(an_cold)/sum(an_comb[[outcome_var]], na.rm = TRUE), 0.5, na.rm = TRUE)*100),
         af_cold_uci = (quantile(unlist(an_cold)/sum(an_comb[[outcome_var]], na.rm = TRUE), 0.975, na.rm = TRUE)*100),
         
         an_ext_heat_est = sum(an_comb$an[mod$data[[temp_var]] > ext_heat_cen_val], na.rm = TRUE),
         an_ext_heat_lci = quantile(unlist(an_ext_heat), c(0.025), na.rm = TRUE),
         an_ext_heat_med = quantile(unlist(an_ext_heat), c(0.5), na.rm = TRUE),
         an_ext_heat_uci = quantile(unlist(an_ext_heat), c(0.975), na.rm = TRUE),
         af_ext_heat_est = (an_ext_heat_est/sum(an_comb[[outcome_var]], na.rm = TRUE)*100),
         af_ext_heat_lci = (quantile(unlist(an_ext_heat)/sum(an_comb[[outcome_var]], na.rm = TRUE), 0.025, na.rm = TRUE)*100),
         af_ext_heat_med = (quantile(unlist(an_ext_heat)/sum(an_comb[[outcome_var]], na.rm = TRUE), 0.5, na.rm = TRUE)*100),
         af_ext_heat_uci = (quantile(unlist(an_ext_heat)/sum(an_comb[[outcome_var]], na.rm = TRUE), 0.975, na.rm = TRUE)*100),
         
         an_ext_cold_est = sum(an_comb$an[mod$data[[temp_var]] < ext_cold_cen_val], na.rm = TRUE),
         an_ext_cold_lci = quantile(unlist(an_ext_cold), c(0.025), na.rm = TRUE),
         an_ext_cold_med = quantile(unlist(an_ext_cold), c(0.5), na.rm = TRUE),
         an_ext_cold_uci = quantile(unlist(an_ext_cold), c(0.975), na.rm = TRUE),
         af_ext_cold_est = (an_ext_cold_est/sum(an_comb[[outcome_var]], na.rm = TRUE)*100),
         af_ext_cold_lci = (quantile(unlist(an_ext_cold)/sum(an_comb[[outcome_var]], na.rm = TRUE), 0.025, na.rm = TRUE)*100),
         af_ext_cold_med = (quantile(unlist(an_ext_cold)/sum(an_comb[[outcome_var]], na.rm = TRUE), 0.5, na.rm = TRUE)*100),
         af_ext_cold_uci = (quantile(unlist(an_ext_cold)/sum(an_comb[[outcome_var]], na.rm = TRUE), 0.975, na.rm = TRUE)*100),
         
         run_date = Sys.time()
         
         ) %>%
        separate(outcome_var, 
                 into = c('outcome', 'count', 'subset_var', 'subset'), 
                 sep = '_', 
                 remove = FALSE) # %>%
    # mutate(across(where(is.numeric), ~ round(.x, 1)))
  
  return(an_calc)
}


