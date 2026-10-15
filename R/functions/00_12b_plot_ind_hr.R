################################################################################-
# Function to plot cumulative deaths and ED visits, mean temperature ----
# Comparing HeatRisk and temperature for local heat surveillance: A time-stratified 
#  case-crossover study of emergency department visits in San Mateo County, 
#  California, 2017–2025
# Juliana GE Bartels
# Updated: 8/26/2026
################################################################################-
## 0: Define function ----
################################################################################-

# Model parameters

## lag_num:       number of lags
##
## mod:           'mod' = model output
## 
## outcome:       'death'
##                'ED visit'
##
## moa:           'rr' = relative risk
##                'pct_chg' = percent change
##
## hr_level:      '2' = Moderate HeatRisk level
##                '3' = Major/Extreme HeatRisk level
##
##
## estimates:     'estimates_heat_risk' = dataframe of estimates of IRR
##
##                'estimates_under5_heat_risk' = dataframe of estimates of IRR for under 5
##                'estimates_5to17_heat_risk' = dataframe of estimates of IRR for 5 to 17
##                'estimates_18to64_heat_risk' = dataframe of estimates of IRR for 18 to 64
##                'estimates_65plus_heat_risk' = dataframe of estimates of IRR for over 65
##                'estimates_80plus_heat_risk' = dataframe of estimates of IRR for over 80
##
##                'estimates_north_heat_risk' = dataframe of estimates of IRR for North county
##                'estimates_mid_heat_risk' = dataframe of estimates of IRR for Mid-county
##                'estimates_south_heat_risk' = dataframe of estimates of IRR for South county
##                'estimates_coastside_heat_risk' = dataframe of estimates of IRR for Coastside
##
##                'estimates_f_heat_risk' = dataframe of estimates of IRR for females
##                'estimates_male_heat_risk' = dataframe of estimates of IRR for males
##
##                'estimates_cardiovascular_heat_risk' = dataframe of estimates of IRR for cardiovascular
##                'estimates_respiratory_heat_risk' = dataframe of estimates of IRR for respiratory
##                'estimates_intentinjury_heat_risk' = dataframe of estimates of IRR for intentional injury
##                'estimates_unintentinjury_heat_risk' = dataframe of estimates of IRR for unintentional injury
##                'estimates_stroke_heat_risk' = dataframe of estimates of IRR for stroke
##                'estimates_substance_heat_risk' = dataframe of estimates of IRR for substance use
##                'estimates_mental_heat_risk' = dataframe of estimates of IRR for mental health
##                'estimates_renal_heat_risk' = dataframe of estimates of IRR for renal
##                'estimates_genitourinary_heat_risk' = dataframe of estimates of IRR for genitourinary
##
##                'estimates_krwc_heat_risk' = dataframe of estimates of IRR for Kaiser Redwood City
##                'estimates_kssf_heat_risk' = dataframe of estimates of IRR for Kaiser South San Francisco
##                'estimates_mp_heat_risk' = dataframe of estimates of IRR for Mills-Peninsula
##                'estimates_smmc_heat_risk' = dataframe of estimates of IRR for San Mateo Medical Center
##                'estimates_seq_heat_risk' = dataframe of estimates of IRR for Sequoia
##                'estimates_sdc_heat_risk' = dataframe of estimates of IRR for Seton
##                'estimates_stan_heat_risk' = dataframe of estimates of IRR for Stanford
##
## subset:        input for plot title
##

################################################################################-
## 1: Define function ----
################################################################################-

plot_ind_hr <- function(lag_num,
                        mod, 
                        outcome,
                        moa,
                        hr_level, 
                        estimates, 
                        subset) {
  
  # reformat for only comparison of HR X to HR0, all individual lags
  if (moa == 'rr') {
   estimates_hr <- estimates %>%
    filter(counterfactual_temp == hr_level) %>% 
    dplyr::select(-c(fit_rr_refT, lci_rr_refT, uci_rr_refT)) %>%
    pivot_longer(cols = starts_with(c("fit_", "lci_", "uci_")),
                 names_to = c("type", "lag"),
                 names_pattern = "(.*)_rr_lag(.*)",
                 values_to = "rr"
    ) %>%
    pivot_wider(names_from = type,
                values_from = rr) %>%
    mutate(lag = as.numeric(lag))
  }
  
  if (moa == 'pct_chg') {
    estimates_hr <- estimates %>%
      filter(counterfactual_temp == hr_level) %>% 
      dplyr::select(-c(fit_rr_refT, lci_rr_refT, uci_rr_refT)) %>%
      pivot_longer(cols = starts_with(c("fit_", "lci_", "uci_")),
                   names_to = c("type", "lag"),
                   names_pattern = "(.*)_rr_lag(.*)",
                   values_to = "rr"
      ) %>%
      pivot_wider(names_from = type,
                  values_from = rr) %>%
      mutate(lag = as.numeric(lag)) %>%
      mutate(fit = (fit - 1) * 100,
             lci = (lci - 1) * 100,
             uci = (uci - 1) * 100)
  }
  
  max_rr <- max(estimates_hr$fit, na.rm = TRUE)
  
  # Plot percent change in mortality rate comparing HR X to HR0 for lag days 0-20 
  ind_hr <- estimates_hr %>%
    filter(lag <= lag_num) %>%
    ggplot(aes(x = lag, y = fit)) +
    geom_ribbon(aes(ymin = lci, ymax = uci), fill = "lightblue",  alpha = 0.4) +
    geom_point(size = 3, color = "black") +
    geom_line(color = "black", linewidth = 0.75) +
    geom_hline(aes(yintercept = switch(moa,
                                       rr = 1,
                                       pct_chg = 0)),
               color = "black", linetype = "dashed") +
    theme_classic() +
    labs(x = 'Lagged days',
         y = switch(
           moa,
           rr = "IRR",
           pct_chg = "Percent change (%)"),
         title = paste0(switch(moa, 
                               rr= "IRR of ", 
                               pct_chg = "Percent change in "),
                        outcome, ' for Heat Risk ', 
                        hr_level, ' compared to 0,\namong ', 
                        subset, 'of San Mateo County, CA')
    ) +
    theme(
      legend.position = 'none',
      axis.title.x = element_text(size = 20),
      axis.text.x = element_text(size = 20),
      axis.title.y = element_text(size = 20),
      axis.text.y = element_text(size = 20)
    ) 
  
  if(moa == 'rr'){
    return(
      ind_hr + 
      if(max_rr > 4) {
        coord_cartesian(ylim = c(0, ceiling(max(estimates_hr$uci[estimates_hr$lag <= lag_num]))))
      }
    )
  }
  
  if(moa == 'pct_chg'){
    return(
      ind_hr + 
      if(max_rr > 400) {
        coord_cartesian(ylim = c(0, ceiling(max(estimates_hr$uci[estimates_hr$lag <= lag_num]))))
      }
    )
  }
    
}
