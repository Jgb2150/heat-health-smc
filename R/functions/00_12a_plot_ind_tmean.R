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
## percentile:    'per_95' = 95th percentile
##                'per_90' = 90th percentile
##                'per_10' = 10th percentile
##                'per_05' = 5th percentile
##
## temp_var:      'tmean' = mean daily temperature
##                'heat_risk' = HeatRisk score
##
## estimates:     'estimates_tmean' = dataframe of estimates of IRR
##
##                'estimates_under5_tmean' = dataframe of estimates of IRR for under 5
##                'estimates_5to17_tmean' = dataframe of estimates of IRR for 5 to 17
##                'estimates_18to64_tmean' = dataframe of estimates of IRR for 18 to 64
##                'estimates_65plus_tmean' = dataframe of estimates of IRR for over 65
##                'estimates_80plus_tmean' = dataframe of estimates of IRR for over 80
##
##                'estimates_north_tmean' = dataframe of estimates of IRR for North county
##                'estimates_mid_tmean' = dataframe of estimates of IRR for Mid-county
##                'estimates_south_tmean' = dataframe of estimates of IRR for South county
##                'estimates_coastside_tmean' = dataframe of estimates of IRR for Coastside
##
##                'estimates_f_tmean' = dataframe of estimates of IRR for females
##                'estimates_male_tmean' = dataframe of estimates of IRR for males
##
##                'estimates_cardiovascular_tmean' = dataframe of estimates of IRR for cardiovascular
##                'estimates_respiratory_tmean' = dataframe of estimates of IRR for respiratory
##                'estimates_intentinjury_tmean' = dataframe of estimates of IRR for intentional injury
##                'estimates_unintentinjury_tmean' = dataframe of estimates of IRR for unintentional injury
##                'estimates_stroke_tmean' = dataframe of estimates of IRR for stroke
##                'estimates_substance_tmean' = dataframe of estimates of IRR for substance use
##                'estimates_mental_tmean' = dataframe of estimates of IRR for mental health
##                'estimates_renal_tmean' = dataframe of estimates of IRR for renal
##                'estimates_genitourinary_tmean' = dataframe of estimates of IRR for genitourinary
##
##                'estimates_krwc_tmean' = dataframe of estimates of IRR for Kaiser Redwood City
##                'estimates_kssf_tmean' = dataframe of estimates of IRR for Kaiser South San Francisco
##                'estimates_mp_tmean' = dataframe of estimates of IRR for Mills-Peninsula
##                'estimates_smmc_tmean' = dataframe of estimates of IRR for San Mateo Medical Center
##                'estimates_seq_tmean' = dataframe of estimates of IRR for Sequoia
##                'estimates_sdc_tmean' = dataframe of estimates of IRR for Seton
##                'estimates_stan_tmean' = dataframe of estimates of IRR for Stanford
##
## subset:        input for plot title
##

################################################################################-
## 1: Define function ----
################################################################################-

plot_ind_tmean <- function(lag_num, 
                           mod, 
                           outcome, 
                           moa,
                           percentile, 
                           temp_var, 
                           estimates, 
                           subset) {
  
  # percentile: per_95, per_05, per_10, per_90
  
  # reformat for only comparison of the xth percentile to the median of temperature, all individual lags
  if (moa == 'rr') {
    estimates_per <- estimates %>%
    filter(label == percentile) %>% 
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
    estimates_per <- estimates %>%
      filter(label == percentile) %>% 
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
  
  median_temp <- round(median(mod$data[[temp_var]], na.rm = TRUE),1)
  per_num <- case_when(percentile == 'per_95' ~ 0.95, 
                       percentile == 'per_05' ~ 0.05,
                       percentile == 'per_10' ~ 0.1,
                       percentile == 'per_90' ~ 0.9,
                       TRUE ~ NA)
  per_temp <- round(quantile(mod$data[[temp_var]], per_num, na.rm = TRUE),1)
  
  # Plot percent change in mortality rate comparing xth percentile to median for lag days 0-20 
  estimates_per %>%
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
                        outcome, " for the ", 
                        per_num*100, "th percentile (", per_temp, "C) compared to the median (", median_temp, "C),\namong ", 
                        subset, "of San Mateo County, CA")
    ) +
    xlim(0, lag_num) +
    theme(
      legend.position = 'none',
      axis.title.x = element_text(size = 20),
      axis.text.x = element_text(size = 20),
      axis.title.y = element_text(size = 20),
      axis.text.y = element_text(size = 20)
    )
}
