################################################################################-
# Function to run models ----
# Comparing HeatRisk and temperature for local heat surveillance: A time-stratified 
#  case-crossover study of emergency department visits in San Mateo County, 
#  California, 2017–2025 
# Juliana GE Bartels
# Updated: 6/16/2026
################################################################################-
## 0: Define model parameters ----
################################################################################-

# Model parameters:

## sensitivity:   'main' = main model
##                'nocov' = without years 2020 and 2021
##                'restricthosp' = without Mills-Peninsula, Seton, or Sequoia hospitals
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
## outcome_var:   ('ed_count_', subset_var, '_', subset)
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
##                'total_pop_north' = north-county region
##                'total_pop_south' = south-county region
##
## dta:           'dta_full'
##
## temp_var:      'tmean' = mean daily temperature
##                'heat_risk' = HeatRisk score
##
## temp_analysis: 'full' = analysis of full year of data
##
## er_constraint: {'a', 'b', 'c', 'd', 'e'} = letter constraint for exposure-response dimension placement of knots
##
## er_type:       'ns' = natural spline exposure response 
##                'quad' = quadratic exposure response
##                'cub' = cubic exposure response
##                'quar' = quartic exposure response
##                'strata' = strata exposure-response
##
## lr_type:       'ns' = natural spline lag-response
##

################################################################################-
## 1: Define function ----
################################################################################-

run_model <- function(sensitivity, 
                      subset, 
                      outcome_var, 
                      pop_denom, 
                      dta, 
                      temp_var, 
                      temp_analysis,
                      er_constraint, 
                      er_type, 
                      lr_type) {
  
  # Main model ----
  ## a. Natural spline in exposure-response and lag (tmean) ----
  if(sensitivity == 'main' & er_type == 'ns' & lr_type == 'ns' & temp_var == 'tmean') {

    if(subset  == "seq") {
      # Filter data for where pop_denom is greater than 0
      dta <- dta %>% filter(.data[[pop_denom]] > 0) %>% 
        # AND filter for the first year that hospital starts reporting
        filter(year >= min(year[.data[[outcome_var]] > 0], na.rm = TRUE))
    } else if (subset  == "sdc") {
      dta <- dta %>% filter(.data[[pop_denom]] > 0) %>% 
        # AND filter for the first year that hospital starts reporting
        filter(year > 2021)
    } else{
      # Filter data for where pop_denom is greater than 0
      dta <- dta %>% filter(.data[[pop_denom]] > 0)
    }
    
    # Redefine crossbasis
    cb_list <- define_cross_basis_temp(temp_analysis, er_constraint, er_type, lr_type,
                                       dta, temp_var)
    
    # Calculate spline term for time with approximated 8 degrees of freedom per year (after Gasparrini, 2021)
    dftrend <- round(as.numeric(diff(range(dta$date))/365.25 * 8))
    btrend <- ns(dta$date, knots=equalknots(dta$date, dftrend-1))
    
    cb.temp <- cb_list$cb.temp_ns
    
    formula <- as.formula(paste(outcome_var, "~ cb.temp + ns(rhmean, 3) + btrend + factor(holiday)"))
    
    mod <- gnm(formula,
               offset = log(dta[[pop_denom]]), 
               family = quasipoisson,
               data = dta,
               eliminate = stratum)
    
    # Store cross basis in model
    mod$cb.temp <- cb.temp
  }
  
  ## b. Quadratic in exposure-response and lag (tmean) ----
  if(sensitivity == 'main' & er_type == 'quad' & lr_type == 'ns' & temp_var == 'tmean') {
    
    if(subset  == "seq") {
      # Filter data for where pop_denom is greater than 0
      dta <- dta %>% filter(.data[[pop_denom]] > 0) %>% 
        # AND filter for the first year that hospital starts reporting
        filter(year >= min(year[.data[[outcome_var]] > 0], na.rm = TRUE))
    } else if (subset  == "sdc") {
      dta <- dta %>% filter(.data[[pop_denom]] > 0) %>% 
        # AND filter for the first year that hospital starts reporting
        filter(year > 2021)
    } else{
      # Filter data for where pop_denom is greater than 0
      dta <- dta %>% filter(.data[[pop_denom]] > 0)
    }
    
    # Redefine crossbasis
    cb_list <- define_cross_basis_temp(temp_analysis, er_constraint, er_type, lr_type,
                                       dta, temp_var)
    
    # Calculate spline term for time with approximated 8 degrees of freedom per year (after Gasparrini, 2021)
    dftrend <- round(as.numeric(diff(range(dta$date))/365.25 * 8))
    btrend <- ns(dta$date, knots=equalknots(dta$date, dftrend-1))
    
    cb.temp <- cb_list$cb.temp_quad_ns
    
    formula <- as.formula(paste(outcome_var, "~ cb.temp + ns(rhmean, 3) + btrend + factor(holiday)"))
    
    mod <- gnm(formula,
               offset = log(dta[[pop_denom]]), 
               family = quasipoisson,
               data = dta,
               eliminate = stratum)
    
    # Store cross basis in model
    mod$cb.temp <- cb.temp
  }
  
  ## c. Cubic in exposure-response and lag (tmean) ----
  if(sensitivity == 'main' & er_type == 'cub' & lr_type == 'ns' & temp_var == 'tmean') {
    
    if(subset  == "seq") {
      # Filter data for where pop_denom is greater than 0
      dta <- dta %>% filter(.data[[pop_denom]] > 0) %>% 
        # AND filter for the first year that hospital starts reporting
        filter(year >= min(year[.data[[outcome_var]] > 0], na.rm = TRUE))
    } else if (subset  == "sdc") {
      dta <- dta %>% filter(.data[[pop_denom]] > 0) %>% 
        # AND filter for the first year that hospital starts reporting
        filter(year > 2021)
    } else{
      # Filter data for where pop_denom is greater than 0
      dta <- dta %>% filter(.data[[pop_denom]] > 0)
    }
    
    # Redefine crossbasis
    cb_list <- define_cross_basis_temp(temp_analysis, er_constraint, er_type, lr_type,
                                       dta, temp_var)
    
    # Calculate spline term for time with approximated 8 degrees of freedom per year (after Gasparrini, 2021)
    dftrend <- round(as.numeric(diff(range(dta$date))/365.25 * 8))
    btrend <- ns(dta$date, knots=equalknots(dta$date, dftrend-1))
    
    cb.temp <- cb_list$cb.temp_cub_ns
    
    formula <- as.formula(paste(outcome_var, "~ cb.temp + ns(rhmean, 3) + btrend + factor(holiday)"))
    
    mod <- gnm(formula,
               offset = log(dta[[pop_denom]]), 
               family = quasipoisson,
               data = dta,
               eliminate = stratum)
    
    # Store cross basis in model
    mod$cb.temp <- cb.temp
  }
  
  ## d. Quartic in exposure-response and lag (tmean) ----
  if(sensitivity == 'main' & er_type == 'quar' & lr_type == 'ns' & temp_var == 'tmean') {
    
    if(subset  == "seq") {
      # Filter data for where pop_denom is greater than 0
      dta <- dta %>% filter(.data[[pop_denom]] > 0) %>% 
        # AND filter for the first year that hospital starts reporting
        filter(year >= min(year[.data[[outcome_var]] > 0], na.rm = TRUE))
    } else if (subset  == "sdc") {
      dta <- dta %>% filter(.data[[pop_denom]] > 0) %>% 
        # AND filter for the first year that hospital starts reporting
        filter(year > 2021)
    } else{
      # Filter data for where pop_denom is greater than 0
      dta <- dta %>% filter(.data[[pop_denom]] > 0)
    }
    
    # Redefine crossbasis
    cb_list <- define_cross_basis_temp(temp_analysis, er_constraint, er_type, lr_type,
                                       dta, temp_var)
    
    # Calculate spline term for time with approximated 8 degrees of freedom per year (after Gasparrini, 2021)
    dftrend <- round(as.numeric(diff(range(dta$date))/365.25 * 8))
    btrend <- ns(dta$date, knots=equalknots(dta$date, dftrend-1))
    
    cb.temp <- cb_list$cb.temp_quar_ns
    
    formula <- as.formula(paste(outcome_var, "~ cb.temp + ns(rhmean, 3) + btrend + factor(holiday)"))
    
    mod <- gnm(formula,
               offset = log(dta[[pop_denom]]), 
               family = quasipoisson,
               data = dta,
               eliminate = stratum)
    
    # Store cross basis in model
    mod$cb.temp <- cb.temp
  }
  
  ## e. Strata in exposure-response and natural spline in lag (heat_risk) ----
  if(sensitivity == 'main' & er_type == 'strata' & lr_type == 'ns' & temp_var == 'heat_risk') {
    
    if(subset == "seq") {
      # Filter data for where pop_denom is greater than 0
      dta <- dta %>% filter(.data[[pop_denom]] > 0) %>% 
        # AND filter for the first year that hospital starts reporting
        filter(year >= min(year[.data[[outcome_var]] > 0], na.rm = TRUE))
    } else if (subset  == "sdc") {
      dta <- dta %>% filter(.data[[pop_denom]] > 0) %>% 
        # AND filter for the first year that hospital starts reporting
        filter(year > 2021)
    } else{
      # Filter data for where pop_denom is greater than 0
      dta <- dta %>% filter(.data[[pop_denom]] > 0)
    }
    
    # Redefine crossbasis
    cb_list <- define_cross_basis_hr(temp_analysis, dta, temp_var)
    
    # Calculate spline term for time with approximated 8 degrees of freedom per year (after Gasparrini, 2021)
    dftrend <- round(as.numeric(diff(range(dta$date))/365.25 * 8))
    btrend <- ns(dta$date, knots=equalknots(dta$date, dftrend-1))
    
    cb.temp <- cb_list$cb.temp_strata_ns
    formula <- as.formula(paste(outcome_var, "~ cb.temp + btrend + factor(holiday)"))
    
    mod <- gnm(formula,
               offset = log(dta[[pop_denom]]), 
               family = quasipoisson,
               data = dta,
               eliminate = stratum)
    
    # Store cross basis in model
    mod$cb.temp <- cb.temp
  }
  
  # nocov sensitivity model: without 2020 and 2021 ----
  
  ## a. Natural spline in exposure-response and lag (tmean) ----
  if(sensitivity == 'nocov' & er_type == 'ns' & lr_type == 'ns' & temp_var == 'tmean') {
    
    if(subset == "seq") {
      # Filter data for where pop_denom is greater than 0
      dta <- dta %>% 
        filter(!year %in% c(2020, 2021)) %>%
        filter(.data[[pop_denom]] > 0) %>% 
        # AND filter for the first year that hospital starts reporting
        filter(year >= min(year[.data[[outcome_var]] > 0], na.rm = TRUE))
    } else if (subset == "sdc") {
      dta <- dta %>% 
        filter(!year %in% c(2020, 2021)) %>%
        filter(.data[[pop_denom]] > 0) %>% 
        # AND filter for the first year that hospital starts reporting
        filter(year > 2021)
    } else {
      # Filter data for where pop_denom is greater than 0
      dta <- dta %>% 
        filter(!year %in% c(2020, 2021)) %>%
        filter(.data[[pop_denom]] > 0)
    }
    
    # Redefine crossbasis
    cb_list <- define_cross_basis_temp(temp_analysis, er_constraint, er_type, lr_type,
                                       dta, temp_var)
    
    # Calculate spline term for time with approximated 8 degrees of freedom per year (after Gasparrini, 2021)
    dftrend <- round(as.numeric(diff(range(dta$date))/365.25 * 8))
    btrend <- ns(dta$date, knots=equalknots(dta$date, dftrend-1))
    
    cb.temp <- cb_list$cb.temp_ns
    
    formula <- as.formula(paste(outcome_var, "~ cb.temp + ns(rhmean, 3) + btrend + factor(holiday)"))
    
    mod <- gnm(formula,
               offset = log(dta[[pop_denom]]), 
               family = quasipoisson,
               data = dta,
               eliminate = stratum)
    
    # Store cross basis in model
    mod$cb.temp <- cb.temp
  }
  
  ## b. Quadratic in exposure-response and lag (tmean) ----
  if(sensitivity == 'nocov' & er_type == 'quad' & lr_type == 'ns' & temp_var == 'tmean') {
    
    if(subset == "seq") {
      # Filter data for where pop_denom is greater than 0
      dta <- dta %>% 
        filter(!year %in% c(2020, 2021)) %>%
        filter(.data[[pop_denom]] > 0) %>% 
        # AND filter for the first year that hospital starts reporting
        filter(year >= min(year[.data[[outcome_var]] > 0], na.rm = TRUE))
    } else if (subset == "sdc") {
      dta <- dta %>% 
        filter(!year %in% c(2020, 2021)) %>%
        filter(.data[[pop_denom]] > 0) %>% 
        # AND filter for the first year that hospital starts reporting
        filter(year > 2021)
    } else {
      # Filter data for where pop_denom is greater than 0
      dta <- dta %>% 
        filter(!year %in% c(2020, 2021)) %>%
        filter(.data[[pop_denom]] > 0)
    }
    
    # Redefine crossbasis
    cb_list <- define_cross_basis_temp(temp_analysis, er_constraint, er_type, lr_type,
                                       dta, temp_var)
    
    # Calculate spline term for time with approximated 8 degrees of freedom per year (after Gasparrini, 2021)
    dftrend <- round(as.numeric(diff(range(dta$date))/365.25 * 8))
    btrend <- ns(dta$date, knots=equalknots(dta$date, dftrend-1))
    
    cb.temp <- cb_list$cb.temp_quad_ns
    
    formula <- as.formula(paste(outcome_var, "~ cb.temp + ns(rhmean, 3) + btrend + factor(holiday)"))
    
    mod <- gnm(formula,
               offset = log(dta[[pop_denom]]), 
               family = quasipoisson,
               data = dta,
               eliminate = stratum)
    
    # Store cross basis in model
    mod$cb.temp <- cb.temp
  }
  
  ## c. Cubic in exposure-response and lag (tmean) ----
  if(sensitivity == 'nocov' & er_type == 'cub' & lr_type == 'ns' & temp_var == 'tmean') {
    
    if(subset == "seq") {
      # Filter data for where pop_denom is greater than 0
      dta <- dta %>% 
        filter(!year %in% c(2020, 2021)) %>%
        filter(.data[[pop_denom]] > 0) %>% 
        # AND filter for the first year that hospital starts reporting
        filter(year >= min(year[.data[[outcome_var]] > 0], na.rm = TRUE))
    } else if (subset == "sdc") {
      dta <- dta %>% 
        filter(!year %in% c(2020, 2021)) %>%
        filter(.data[[pop_denom]] > 0) %>% 
        # AND filter for the first year that hospital starts reporting
        filter(year > 2021)
    } else {
      # Filter data for where pop_denom is greater than 0
      dta <- dta %>% 
        filter(!year %in% c(2020, 2021)) %>%
        filter(.data[[pop_denom]] > 0)
    }
    
    # Redefine crossbasis
    cb_list <- define_cross_basis_temp(temp_analysis, er_constraint, er_type, lr_type,
                                       dta, temp_var)
    
    # Calculate spline term for time with approximated 8 degrees of freedom per year (after Gasparrini, 2021)
    dftrend <- round(as.numeric(diff(range(dta$date))/365.25 * 8))
    btrend <- ns(dta$date, knots=equalknots(dta$date, dftrend-1))
    
    cb.temp <- cb_list$cb.temp_cub_ns
    
    formula <- as.formula(paste(outcome_var, "~ cb.temp + ns(rhmean, 3) + btrend + factor(holiday)"))
    
    mod <- gnm(formula,
               offset = log(dta[[pop_denom]]), 
               family = quasipoisson,
               data = dta,
               eliminate = stratum)
    
    # Store cross basis in model
    mod$cb.temp <- cb.temp
  }
  
  ## d. Quartic in exposure-response and lag (tmean) ----
  if(sensitivity == 'nocov' & er_type == 'quar' & lr_type == 'ns' & temp_var == 'tmean') {
    
    if(subset == "seq") {
      # Filter data for where pop_denom is greater than 0
      dta <- dta %>% 
        filter(!year %in% c(2020, 2021)) %>%
        filter(.data[[pop_denom]] > 0) %>% 
        # AND filter for the first year that hospital starts reporting
        filter(year >= min(year[.data[[outcome_var]] > 0], na.rm = TRUE))
    } else if (subset == "sdc") {
      dta <- dta %>% 
        filter(!year %in% c(2020, 2021)) %>%
        filter(.data[[pop_denom]] > 0) %>% 
        # AND filter for the first year that hospital starts reporting
        filter(year > 2021)
    } else {
      # Filter data for where pop_denom is greater than 0
      dta <- dta %>% 
        filter(!year %in% c(2020, 2021)) %>%
        filter(.data[[pop_denom]] > 0)
    }
    
    # Redefine crossbasis
    cb_list <- define_cross_basis_temp(temp_analysis, er_constraint, er_type, lr_type,
                                       dta, temp_var)
    
    # Calculate spline term for time with approximated 8 degrees of freedom per year (after Gasparrini, 2021)
    dftrend <- round(as.numeric(diff(range(dta$date))/365.25 * 8))
    btrend <- ns(dta$date, knots=equalknots(dta$date, dftrend-1))
    
    cb.temp <- cb_list$cb.temp_quar_ns
    
    formula <- as.formula(paste(outcome_var, "~ cb.temp + ns(rhmean, 3) + btrend + factor(holiday)"))
    
    mod <- gnm(formula,
               offset = log(dta[[pop_denom]]), 
               family = quasipoisson,
               data = dta,
               eliminate = stratum)
    
    # Store cross basis in model
    mod$cb.temp <- cb.temp
  }
  
  ## e. Strata in exposure-response and natural spline in lag (heat_risk) ----
  if(sensitivity == 'nocov' & er_type == 'strata' & lr_type == 'ns' & temp_var == 'heat_risk') {
    
    if(subset == "seq") {
      # Filter data for where pop_denom is greater than 0
      dta <- dta %>% 
        filter(!year %in% c(2020, 2021)) %>%
        filter(.data[[pop_denom]] > 0) %>% 
        # AND filter for the first year that hospital starts reporting
        filter(year >= min(year[.data[[outcome_var]] > 0], na.rm = TRUE))
    } else if (subset == "sdc") {
      dta <- dta %>% 
        filter(!year %in% c(2020, 2021)) %>%
        filter(.data[[pop_denom]] > 0) %>% 
        # AND filter for the first year that hospital starts reporting
        filter(year > 2021)
    } else {
      # Filter data for where pop_denom is greater than 0
      dta <- dta %>% 
        filter(!year %in% c(2020, 2021)) %>%
        filter(.data[[pop_denom]] > 0)
    }
    
    # Redefine crossbasis
    cb_list <- define_cross_basis_hr(temp_analysis, dta, temp_var)
    
    # Calculate spline term for time with approximated 8 degrees of freedom per year (after Gasparrini, 2021)
    dftrend <- round(as.numeric(diff(range(dta$date))/365.25 * 8))
    btrend <- ns(dta$date, knots=equalknots(dta$date, dftrend-1))
    
    cb.temp <- cb_list$cb.temp_strata_ns
    formula <- as.formula(paste(outcome_var, "~ cb.temp + btrend + factor(holiday)"))
    
    mod <- gnm(formula,
               offset = log(dta[[pop_denom]]), 
               family = quasipoisson,
               data = dta,
               eliminate = stratum)
    
    # Store cross basis in model
    mod$cb.temp <- cb.temp
  }
  
  
  
  # Restrict hospitals model ----
  ## a. Natural spline in exposure-response and lag (tmean) ----
  if(sensitivity == 'restricthosp' & er_type == 'ns' & lr_type == 'ns' & temp_var == 'tmean') {
    
    # Filter data for where pop_denom is greater than 0
    dta <- dta %>% filter(.data[[pop_denom]] > 0)
    
    # Redefine crossbasis
    cb_list <- define_cross_basis_temp(temp_analysis, er_constraint, er_type, lr_type,
                                       dta, temp_var)
    
    # Calculate spline term for time with approximated 8 degrees of freedom per year (after Gasparrini, 2021)
    dftrend <- round(as.numeric(diff(range(dta$date))/365.25 * 8))
    btrend <- ns(dta$date, knots=equalknots(dta$date, dftrend-1))
    
    cb.temp <- cb_list$cb.temp_ns
    
    formula <- as.formula(paste(outcome_var, "~ cb.temp + ns(rhmean, 3) + btrend + factor(holiday)"))
    
    mod <- gnm(formula,
               offset = log(dta[[pop_denom]]), 
               family = quasipoisson,
               data = dta,
               eliminate = stratum)
    
    # Store cross basis in model
    mod$cb.temp <- cb.temp
  }
  
  ## b. Quadratic in exposure-response and lag (tmean) ----
  if(sensitivity == 'restricthosp' & er_type == 'quad' & lr_type == 'ns' & temp_var == 'tmean') {
    
    # Filter data for where pop_denom is greater than 0
    dta <- dta %>% filter(.data[[pop_denom]] > 0)
    
    # Redefine crossbasis
    cb_list <- define_cross_basis_temp(temp_analysis, er_constraint, er_type, lr_type,
                                       dta, temp_var)
    
    # Calculate spline term for time with approximated 8 degrees of freedom per year (after Gasparrini, 2021)
    dftrend <- round(as.numeric(diff(range(dta$date))/365.25 * 8))
    btrend <- ns(dta$date, knots=equalknots(dta$date, dftrend-1))
    
    cb.temp <- cb_list$cb.temp_quad_ns
    
    formula <- as.formula(paste(outcome_var, "~ cb.temp + ns(rhmean, 3) + btrend + factor(holiday)"))
    
    mod <- gnm(formula,
               offset = log(dta[[pop_denom]]), 
               family = quasipoisson,
               data = dta,
               eliminate = stratum)
    
    # Store cross basis in model
    mod$cb.temp <- cb.temp
  }
  
  ## c. Cubic in exposure-response and lag (tmean) ----
  if(sensitivity == 'restricthosp' & er_type == 'cub' & lr_type == 'ns' & temp_var == 'tmean') {
    
    # Filter data for where pop_denom is greater than 0
    dta <- dta %>% filter(.data[[pop_denom]] > 0)
    
    # Redefine crossbasis
    cb_list <- define_cross_basis_temp(temp_analysis, er_constraint, er_type, lr_type,
                                       dta, temp_var)
    
    # Calculate spline term for time with approximated 8 degrees of freedom per year (after Gasparrini, 2021)
    dftrend <- round(as.numeric(diff(range(dta$date))/365.25 * 8))
    btrend <- ns(dta$date, knots=equalknots(dta$date, dftrend-1))
    
    cb.temp <- cb_list$cb.temp_cub_ns
    
    formula <- as.formula(paste(outcome_var, "~ cb.temp + ns(rhmean, 3) + btrend + factor(holiday)"))
    
    mod <- gnm(formula,
               offset = log(dta[[pop_denom]]), 
               family = quasipoisson,
               data = dta,
               eliminate = stratum)
    
    # Store cross basis in model
    mod$cb.temp <- cb.temp
  }
  
  ## d. Quartic in exposure-response and lag (tmean) ----
  if(sensitivity == 'restricthosp' & er_type == 'quar' & lr_type == 'ns' & temp_var == 'tmean') {
    
    # Filter data for where pop_denom is greater than 0
    dta <- dta %>% filter(.data[[pop_denom]] > 0)
    
    # Redefine crossbasis
    cb_list <- define_cross_basis_temp(temp_analysis, er_constraint, er_type, lr_type,
                                       dta, temp_var)
    
    # Calculate spline term for time with approximated 8 degrees of freedom per year (after Gasparrini, 2021)
    dftrend <- round(as.numeric(diff(range(dta$date))/365.25 * 8))
    btrend <- ns(dta$date, knots=equalknots(dta$date, dftrend-1))
    
    cb.temp <- cb_list$cb.temp_quar_ns
    
    formula <- as.formula(paste(outcome_var, "~ cb.temp + ns(rhmean, 3) + btrend + factor(holiday)"))
    
    mod <- gnm(formula,
               offset = log(dta[[pop_denom]]), 
               family = quasipoisson,
               data = dta,
               eliminate = stratum)
    
    # Store cross basis in model
    mod$cb.temp <- cb.temp
  }
  
  ## e. Strata in exposure-response and natural spline in lag (heat_risk) ----
  if(sensitivity == 'restricthosp' & er_type == 'strata' & lr_type == 'ns' & temp_var == 'heat_risk') {
    
    # Filter data for where pop_denom is greater than 0
    dta <- dta %>% filter(.data[[pop_denom]] > 0)
    
    # Redefine crossbasis
    cb_list <- define_cross_basis_hr(temp_analysis, dta, temp_var)
    
    # Calculate spline term for time with approximated 8 degrees of freedom per year (after Gasparrini, 2021)
    dftrend <- round(as.numeric(diff(range(dta$date))/365.25 * 8))
    btrend <- ns(dta$date, knots=equalknots(dta$date, dftrend-1))
    
    cb.temp <- cb_list$cb.temp_strata_ns
    formula <- as.formula(paste(outcome_var, "~ cb.temp + btrend + factor(holiday)"))
    
    mod <- gnm(formula,
               offset = log(dta[[pop_denom]]), 
               family = quasipoisson,
               data = dta,
               eliminate = stratum)
    
    # Store cross basis in model
    mod$cb.temp <- cb.temp
  }
  
  
  return(mod)
  
}
