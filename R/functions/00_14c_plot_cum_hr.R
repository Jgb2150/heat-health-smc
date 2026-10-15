################################################################################-
# Function to plot cumulative deaths and ED visits, mean temperature ----
# Comparing HeatRisk and temperature for local heat surveillance: A time-stratified 
#  case-crossover study of emergency department visits in San Mateo County, 
#  California, 2017–2025 
# Juliana GE Bartels
# Updated: 8/26/2026
################################################################################-
## 0: Define model parameters ----
################################################################################-

# Model parameters:

## lag_num:               number of lags at which to get cumulative estimates
##
## mod:                   model for estimates
##
## outcome:               'death'
##                        'ED visit'
##
## moa:                   'rr' = relative risk
##                        'pct_chg' = percent change
##
## temp_var:              'tmean' = mean temperature
##                        'heat_risk' = HeatRisk score
##
## estimates:             'estimates_heat_risk' = dataframe of estimates of IRR
##
##                        'estimates_under5_heat_risk' = dataframe of estimates of IRR for under 5
##                        'estimates_5to17_heat_risk' = dataframe of estimates of IRR for 5 to 17
##                        'estimates_18to64_heat_risk' = dataframe of estimates of IRR for 18 to 64
##                        'estimates_65plus_heat_risk' = dataframe of estimates of IRR for over 65
##                        'estimates_80plus_heat_risk' = dataframe of estimates of IRR for over 80
##
##                        'estimates_north_heat_risk' = dataframe of estimates of IRR for North county
##                        'estimates_mid_heat_risk' = dataframe of estimates of IRR for Mid-county
##                        'estimates_south_heat_risk' = dataframe of estimates of IRR for South county
##                        'estimates_coastside_heat_risk' = dataframe of estimates of IRR for Coastside
##
##                        'estimates_f_heat_risk' = dataframe of estimates of IRR for females
##                        'estimates_male_heat_risk' = dataframe of estimates of IRR for males
##
##                        'estimates_cardiovascular_heat_risk' = dataframe of estimates of IRR for cardiovascular
##                        'estimates_respiratory_heat_risk' = dataframe of estimates of IRR for respiratory
##                        'estimates_intentinjury_heat_risk' = dataframe of estimates of IRR for intentional injury
##                        'estimates_unintentinjury_heat_risk' = dataframe of estimates of IRR for unintentional injury
##                        'estimates_stroke_heat_risk' = dataframe of estimates of IRR for stroke
##                        'estimates_substance_heat_risk' = dataframe of estimates of IRR for substance use
##                        'estimates_mental_heat_risk' = dataframe of estimates of IRR for mental health
##                        'estimates_renal_heat_risk' = dataframe of estimates of IRR for renal
##                        'estimates_genitourinary_heat_risk' = dataframe of estimates of IRR for genitourinary
##
##                        'estimates_krwc_heat_risk' = dataframe of estimates of IRR for Kaiser Redwood City
##                        'estimates_kssf_heat_risk' = dataframe of estimates of IRR for Kaiser South San Francisco
##                        'estimates_mp_heat_risk' = dataframe of estimates of IRR for Mills-Peninsula
##                        'estimates_smmc_heat_risk' = dataframe of estimates of IRR for San Mateo Medical Center
##                        'estimates_seq_heat_risk' = dataframe of estimates of IRR for Sequoia
##                        'estimates_sdc_heat_risk' = dataframe of estimates of IRR for Seton
##                        'estimates_stan_heat_risk' = dataframe of estimates of IRR for Stanford
##
## subset:                'age' = age groups (under 5, 5 to 17, 18 to 64, 65 plus)
##                        'region' = regions in SMC (coastside, mid, north, south)
##                        'sex' = sex of patients (female, male)
##                        'cause' = cause of death or visit (non-accidental, cardiovascular, respiratory, injury, stroke)
##              
## with_hist:             TRUE or FALSE
##

################################################################################-
## 1: Define function ----
################################################################################-

plot_cum_hr <- function(lag_num,
                        mod,
                        outcome,
                        moa,
                        temp_var, 
                        estimates,
                        subset,
                        with_hist) {
  
  if(moa == 'rr') {
    estimates_bylag <- estimates %>%
      dplyr::select(counterfactual_temp, paste0('fit_rr_lag', lag_num), paste0('lci_rr_lag', lag_num), paste0('uci_rr_lag', lag_num)) %>%
      rename(fit_rr_lag = paste0('fit_rr_lag', lag_num),
             lci_rr_lag = paste0('lci_rr_lag', lag_num),
             uci_rr_lag = paste0('uci_rr_lag', lag_num))
    }
  
  if(moa == 'pct_chg'){
    estimates_bylag <- estimates %>%
      dplyr::select(counterfactual_temp, paste0('fit_rr_lag', lag_num), paste0('lci_rr_lag', lag_num), paste0('uci_rr_lag', lag_num)) %>%
      rename(fit_rr_lag = paste0('fit_rr_lag', lag_num),
             lci_rr_lag = paste0('lci_rr_lag', lag_num),
             uci_rr_lag = paste0('uci_rr_lag', lag_num)) %>%
      mutate(fit_rr_lag = (fit_rr_lag - 1) * 100,
             lci_rr_lag = (lci_rr_lag - 1) * 100,
             uci_rr_lag = (uci_rr_lag - 1) * 100)
  }
  
  max_rr <- max(estimates_bylag$fit_rr_lag, na.rm = TRUE)
  max_uci <- max(estimates_bylag$uci_rr_lag, na.rm = TRUE)
  
  if(moa == 'rr'){
    max_rr_ref <- 4
    max_uci_ref <- 6
  }
  
  if(moa == 'pct_chg'){
    max_rr_ref <- 400
    max_uci_ref <- 600
  }
  
  cum_rr <- estimates_bylag %>%
    ggplot(aes(x = factor(counterfactual_temp), y = fit_rr_lag)) +
    geom_point(size = 5, color = "black") +
    geom_errorbar(aes(ymin = lci_rr_lag, ymax = uci_rr_lag), 
                width = 0) +
    geom_hline(aes(yintercept = switch(moa,
                                       rr = 1,
                                       pct_chg = 0)),
                   color = "black", linetype = "dashed") +
    labs(x = "HeatRisk level", 
         y = switch(
           moa,
           rr = "IRR",
           pct_chg = "Percent change (%)"
         ),
         title = paste0("Cumulative ", switch(moa, 
                                              rr= "relative risk of ", 
                                              pct_chg = "percent change in "),
                        outcome, " for HeatRisk levels compared to 0 over ", lag_num, " days lag,\namong ", 
                        subset, "of San Mateo County, CA")
         ) +
    scale_x_discrete(labels = c("Little to none", 
                                "Minor", 
                                "Moderate",
                                "Major/Extreme")) +
    theme_classic() +
    theme(legend.position = 'none',
          axis.title.x = element_text(size = 20),
          axis.text.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          axis.text.y = element_text(size = 20))
  
  cum_rr +
    if (max_rr > max_rr_ref | max_uci > max_uci_ref) {
      coord_cartesian(ylim = c(0, ceiling(max_rr + 2)))
    }
  
  if(with_hist == TRUE) {
    temp_hist <- mod$data %>%
      filter(!is.na(.data[[temp_var]])) %>%
      ggplot() +
      geom_bar(fill = "lightgrey",
               aes(x = factor(.data[[temp_var]]),
                   y = (after_stat(count / sum(count)))*100)) +
      geom_text(
        aes(x = factor(.data[[temp_var]]),
            y = after_stat(count / sum(count)) * 100,
            label = paste0(round((after_stat(count / sum(count))* 100),1), "%")
        ),
        stat = "count",
        vjust = -0.3,
        size = 4
      ) +
      labs(x = "HeatRisk", y = "% ZIP-code days") +
      theme_classic() +
      scale_x_discrete(limits = c("0", "1", "2", "3"),
                       labels = c("Little to none", 
                                  "Minor", 
                                  "Moderate",
                                  "Major/Extreme")) +
      theme(axis.title.x = element_text(size = 20),
            axis.title.y = element_text(size = 20),
            axis.text.x = element_text(size = 20),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            legend.position = 'none')
    
    cum_rr_hist <- plot_grid(
      cum_rr +
        theme(axis.title.x = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_text(size = 20),
              plot.margin = margin(t = 20, b = 10)) +
        if (max_rr > max_rr_ref | max_uci > max_uci_ref) {
          coord_cartesian(ylim = c(0, ceiling(max_rr + 2)))
        },
      temp_hist +
        theme(plot.margin = margin(t = 10)) +
        coord_cartesian(clip = "off"),
      nrow = 2,
      rel_heights = c(3, 1),
      align = "v",
      axis = "lr"
    )
    
    return(cum_rr_hist)
  }
  
  if(with_hist == FALSE) {
    return(cum_rr +
             if (max_rr > max_rr_ref | max_uci > max_uci_ref) {
               coord_cartesian(ylim = c(0, ceiling(max_rr + 2)))
             }
           )
  }
}
