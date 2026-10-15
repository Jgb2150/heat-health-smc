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

## 
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
## estimates:             'estimates_tmean' = dataframe of estimates of IRR
##
##                        'estimates_under5_tmean' = dataframe of estimates of IRR for under 5
##                        'estimates_5to17_tmean' = dataframe of estimates of IRR for 5 to 17
##                        'estimates_18to64_tmean' = dataframe of estimates of IRR for 18 to 64
##                        'estimates_65plus_tmean' = dataframe of estimates of IRR for over 65
##                        'estimates_80plus_tmean' = dataframe of estimates of IRR for over 80
##
##                        'estimates_north_tmean' = dataframe of estimates of IRR for North county
##                        'estimates_mid_tmean' = dataframe of estimates of IRR for Mid-county
##                        'estimates_south_tmean' = dataframe of estimates of IRR for South county
##                        'estimates_coastside_tmean' = dataframe of estimates of IRR for Coastside
##
##                        'estimates_f_tmean' = dataframe of estimates of IRR for females
##                        'estimates_male_tmean' = dataframe of estimates of IRR for males
##
##                        'estimates_cardiovascular_tmean' = dataframe of estimates of IRR for cardiovascular
##                        'estimates_respiratory_tmean' = dataframe of estimates of IRR for respiratory
##                        'estimates_intentinjury_tmean' = dataframe of estimates of IRR for intentional injury
##                        'estimates_unintentinjury_tmean' = dataframe of estimates of IRR for unintentional injury
##                        'estimates_stroke_tmean' = dataframe of estimates of IRR for stroke
##                        'estimates_substance_tmean' = dataframe of estimates of IRR for substance use
##                        'estimates_mental_tmean' = dataframe of estimates of IRR for mental health
##                        'estimates_renal_tmean' = dataframe of estimates of IRR for renal
##                        'estimates_genitourinary_tmean' = dataframe of estimates of IRR for genitourinary
##
##                        'estimates_krwc_tmean' = dataframe of estimates of IRR for Kaiser Redwood City
##                        'estimates_kssf_tmean' = dataframe of estimates of IRR for Kaiser South San Francisco
##                        'estimates_mp_tmean' = dataframe of estimates of IRR for Mills-Peninsula
##                        'estimates_smmc_tmean' = dataframe of estimates of IRR for San Mateo Medical Center
##                        'estimates_seq_tmean' = dataframe of estimates of IRR for Sequoia
##                        'estimates_sdc_tmean' = dataframe of estimates of IRR for Seton
##                        'estimates_stan_tmean' = dataframe of estimates of IRR for Stanford
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

plot_cum_tmean <- function(lag_num,
                           mod,
                           outcome, 
                           moa, 
                           temp_var, 
                           estimates, 
                           subset, 
                           with_hist) {
  
  # Calculate estimates by lag for IRR
  if(moa == 'rr') {
    estimates_bylag <- estimates %>%
      filter(label == 'er_values') %>% 
      dplyr::select(counterfactual_temp, paste0('fit_rr_lag', lag_num), paste0('lci_rr_lag', lag_num), paste0('uci_rr_lag', lag_num)) %>%
      rename(fit_rr_lag = paste0('fit_rr_lag', lag_num),
             lci_rr_lag = paste0('lci_rr_lag', lag_num),
             uci_rr_lag = paste0('uci_rr_lag', lag_num))  
    }
  
  # Calculate estimates by lag for percent change
  if(moa == 'pct_chg'){
    estimates_bylag <- estimates %>%
      filter(label == 'er_values') %>% 
      dplyr::select(counterfactual_temp, paste0('fit_rr_lag', lag_num), paste0('lci_rr_lag', lag_num), paste0('uci_rr_lag', lag_num)) %>%
      rename(fit_rr_lag = paste0('fit_rr_lag', lag_num),
             lci_rr_lag = paste0('lci_rr_lag', lag_num),
             uci_rr_lag = paste0('uci_rr_lag', lag_num)) %>%
      mutate(fit_rr_lag = (fit_rr_lag - 1) * 100,
             lci_rr_lag = (lci_rr_lag - 1) * 100,
             uci_rr_lag = (uci_rr_lag - 1) * 100)
  }
  
  # Calculate median, 5th percentile, and 95th percentile temp
  median_temp <- median(mod$data[[temp_var]])
  perc_5_temp <- quantile(mod$data[[temp_var]], 0.05)
  perc_95_temp <- quantile(mod$data[[temp_var]], 0.95)
  
  # Calculate minimum temperature and maximum temperature
  xmin_temp <- floor(min(mod$data[[temp_var]]))
  xmax_temp <- ceiling(max(mod$data[[temp_var]]))
  
  # Graph cumulative IRR
  cum_rr <- estimates_bylag %>%
    mutate(temp_group = case_when(counterfactual_temp >= median_temp ~ "hot",
                                  counterfactual_temp < median_temp ~ "cold",
                                  TRUE ~ NA)) %>% 
    ggplot(aes(x = counterfactual_temp, y = fit_rr_lag, group = temp_group)) +
    geom_line(aes(color = temp_group), linewidth = 1) +
    geom_ribbon(aes(ymin = lci_rr_lag, ymax = uci_rr_lag), 
                fill = "#dedbd2",
                alpha = 0.3) +
    geom_hline(aes(yintercept = switch(moa, rr = 1, pct_chg = 0)), color = "black", linetype = "dashed") +
    geom_vline(aes(xintercept = median_temp), color = "black", alpha = 0.8) +
    geom_vline(aes(xintercept = perc_5_temp), color = "black", linetype = "dotted") +
    geom_vline(aes(xintercept = perc_95_temp), color = "black", linetype = "dotted") +
    scale_color_manual(values = c("hot" = "#d00000",
                     "cold" = "black")) +
    scale_x_continuous(breaks = seq(xmin_temp, xmax_temp, by = 4)) +
    labs(x = expression("Mean daily temperature (" * degree * "C)"),
         y = switch(
           moa,
           rr = "IRR",
           pct_chg = "Percent change (%)"
         ),
         title = paste0("Cumulative ", switch(moa,
                                             rr = "relative risk of ",
                                             pct_chg = "percent change in "), 
                        outcome, " compared to the median temperature\nafter ", 
                        lag_num, " days lag, among ", 
                        subset, "of San Mateo County, CA")
    ) +
    theme_classic() +
    theme(legend.position = 'none',
          axis.title.x = element_text(size = 20),
          axis.text.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          axis.text.y = element_text(size = 20))
  
  # Plot proportion of ZIP-code days at each temperature
  if(with_hist == TRUE) {
    temp_hist <- ggplot(data = mod$data, aes(x = .data[[temp_var]] )) +
      geom_histogram(aes(y = after_stat(count / sum(count))*100),
                     binwidth = 2, fill = "#dedbd2", color = "black") +
      geom_vline(aes(xintercept = median_temp), color = "black", alpha = 0.8) +
      geom_vline(aes(xintercept = perc_5_temp), color = "black", linetype = "dotted") +
      geom_vline(aes(xintercept = perc_95_temp), color = "black", linetype = "dotted") +
      labs(x = expression("Mean daily temperature (" * degree * "C)"),
           y = "% ZIP-code\ndays") +
      theme_classic() +
      scale_x_continuous(breaks = seq(xmin_temp, xmax_temp, by = 4)) +
      scale_y_continuous(breaks = c(0, 10)) +
      theme(axis.title.x = element_text(size = 20),
            axis.text.x = element_text(size = 20),
            axis.title.y = element_text(size = 20),
            axis.text.y = element_text(size = 14))
    
    # Plot IRR and histogram together
    cum_rr_hist <- plot_grid(
      cum_rr +
        coord_cartesian(xlim = c(2, 34)) +
        theme(plot.margin = margin(5.5, 5.5, 5.5, 5.5),
              axis.title.x = element_blank(),
              axis.text.x = element_blank(),
              axis.line.x = element_blank(),
              axis.ticks.x = element_blank()),
      temp_hist +
        coord_cartesian(xlim = c(2, 34)) +
        theme(plot.margin = margin(5.5, 5.5, 5.5, 5.5)),
      nrow = 2,
      rel_heights = c(4, 1),
      align = "v",
      axis = "lr"
    )
    
    return(cum_rr_hist)
  }
  
  # Plot IRR
  if(with_hist == FALSE) {
    return(cum_rr)
  }
}
