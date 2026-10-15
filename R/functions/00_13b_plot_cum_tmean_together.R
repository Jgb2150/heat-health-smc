################################################################################-
# Function to plot cumulative deaths and ED visits, mean temperature ----
# Comparing HeatRisk and temperature for local heat surveillance: A time-stratified 
#  case-crossover study of emergency department visits in San Mateo County, 
#  California, 2017–2025
# Juliana GE Bartels
# Updated: 6/22/2026
################################################################################-
## 0: Define model parameters ----
################################################################################-

# Model parameters

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
##
## group:                 groups for subset
##                        age groups = {under 5, 5 to 17, 18 to 64, 65 plus, 80 plus}
##                        regions = {north, mid, south, coastside}
##                        sex = {female, male}
##                        cause
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
## subset_var:            'Age group' = age groups (under 5, 5 to 17, 18 to 64, 65 plus)
##                        'Region' = regions in SMC (coastside, mid, north, south)
##                        'Sex' = sex of patients (female, male)
##                        'Cause' = cause of death or visit (Cardiovascular, Respiratory, Stroke, Unintentional injury, Substance use, Mental health, Renal, Genitourinary)
##                        'Hospital' = hospital ('krwc', 'kssf', 'mp', 'smmc', 'seq', 'sdc', 'stan' )
##              
## with_hist:             TRUE or FALSE
##

################################################################################-
## 1: Define function ----
################################################################################-

plot_cum_tmean_together <- function(lag_num, 
                                    mod, 
                                    outcome, 
                                    moa, 
                                    temp_var, 
                                    group, 
                                    estimates, 
                                    subset_var, 
                                    with_hist) {
  
  # Calculate estimates by lag for IRR
  if(moa == 'rr') {
    
    estimates_bylag <- data.frame()
    
    for(i in 1:length(estimates)) {
      
      estimates_clean <- estimates[[i]] %>%
      filter(label == 'er_values') %>% 
      dplyr::select(counterfactual_temp, paste0('fit_rr_lag', lag_num), paste0('lci_rr_lag', lag_num), paste0('uci_rr_lag', lag_num)) %>%
      rename(fit_rr_lag = paste0('fit_rr_lag', lag_num),
             lci_rr_lag = paste0('lci_rr_lag', lag_num),
             uci_rr_lag = paste0('uci_rr_lag', lag_num)) %>%
      mutate(group = group[i])
      
      estimates_bylag <- estimates_bylag %>%
        bind_rows(estimates_clean)
      
    }
    }
  
  # Calculate estimates by lag for percent change
  if(moa == 'pct_chg'){
    
    estimates_bylag <- data.frame()
    
    for(i in 1:length(estimates)) {
      
    estimates_clean <- estimates[[i]] %>%
      filter(label == 'er_values') %>% 
      dplyr::select(counterfactual_temp, paste0('fit_rr_lag', lag_num), paste0('lci_rr_lag', lag_num), paste0('uci_rr_lag', lag_num)) %>%
      rename(fit_rr_lag = paste0('fit_rr_lag', lag_num),
             lci_rr_lag = paste0('lci_rr_lag', lag_num),
             uci_rr_lag = paste0('uci_rr_lag', lag_num)) %>%
      mutate(fit_rr_lag = (fit_rr_lag - 1) * 100,
             lci_rr_lag = (lci_rr_lag - 1) * 100,
             uci_rr_lag = (uci_rr_lag - 1) * 100) %>%
      mutate(group = group[i])
    
    estimates_bylag <- estimates_bylag %>%
      bind_rows(estimates_clean)
    }
  }
  
  # Calculate median, 5th percentile, and 95th percentile temp
  median_temp <- median(mod$data[[temp_var]])
  perc_5_temp <- quantile(mod$data[[temp_var]], 0.05)
  perc_95_temp <- quantile(mod$data[[temp_var]], 0.95)
  
  # Calculate minimum temperature and maximum temperature
  xmin_temp <- floor(min(mod$data[[temp_var]]))
  xmax_temp <- ceiling(max(mod$data[[temp_var]]))
  
  # Create separate dataframe for labels
  labels <- estimates_bylag %>%
    group_by(group) %>%
    arrange(desc(counterfactual_temp)) %>%
    slice(1) %>%
    ungroup()
  
  # Graph cumulative IRR
  cum_rr <- estimates_bylag %>%
    mutate(group = case_when(subset_var == "Age group" ~
                               factor(group, c("< 5", "5-17", "18-64", "\u2265 65")),
                             subset_var == "Region" ~
                               factor(group, c("North", "Mid", "South", "Coastside")),
                             subset_var == "Sex" ~
                               factor(group, c("Female", "Male")),
                             subset_var == "Cause" ~
                               factor(group, c("Cardiovascular", "Respiratory", "Stroke", "Unintentional injury", "Substance use", "Mental health", "Renal", "Genitourinary" )),
                             subset_var == "Hospital" ~
                               factor(group, c('krwc', 'kssf', 'mp', 'smmc', 'seq', 'sdc', 'stan'))
                             )
    ) %>%
    ggplot(aes(x = counterfactual_temp, y = fit_rr_lag, group = group)) +
    geom_ribbon(aes(ymin = lci_rr_lag, ymax = uci_rr_lag, fill = group),
                alpha = 0.05) +
    geom_line(aes(color = group), linewidth = 1) +
    geom_hline(aes(yintercept = switch(moa, rr = 1, pct_chg = 0)), color = "black", linetype = "dashed") +
    geom_vline(aes(xintercept = median_temp), color = "black", alpha = 0.8) +
    geom_vline(aes(xintercept = perc_5_temp), color = "black", linetype = "dotted") +
    geom_vline(aes(xintercept = perc_95_temp), color = "black", linetype = "dotted") +
    scale_color_manual(values =
                         switch(subset_var,
                                "Age group" = c("< 5" = "#5c0099", 
                                                "5-17" = "#8ecae6",
                                                "18-64" = "#797d62", 
                                                "\u2265 65" = "#ff8b94"),
                                "Region" = c("North" = "#5c0099",
                                             "Mid" = "#8ecae6", 
                                             "South" = "#797d62", 
                                             "Coastside" = "#ff8b94"),
                                "Sex" = c("Female" = "#5c0099",
                                          "Male" = "#8ecae6"),
                                "Cause" = c("Cardiovascular" = "#5c0099", 
                                            "Respiratory" = "#8ecae6", 
                                            "Stroke" =  "#797d62",
                                            "Intentional injury" = "forestgreen",
                                            "Unintentional injury" = "#ff8b94", 
                                            "Substance use" = "#ffcb69", 
                                            "Mental health" = "#fb8500" ,
                                            'Renal' = "#003049", 
                                            'Genitourinary' = "red"
                                            ),
                                "Hospital" = c('krwc' = "#5c0099",
                                               'kssf' = "#8ecae6",
                                               'mp' = "#797d62",
                                               'smmc' = "#ff8b94",
                                               'seq' = "#ffcb69", 
                                               'sdc' = "#fb8500" , 
                                               'stan' = "#003049"))
                       )+
    scale_fill_manual(values =
                        switch(subset_var,
                               "Age group" = c("< 5" = "#5c0099", 
                                               "5-17" = "#8ecae6",
                                               "18-64" = "#797d62", 
                                               "\u2265 65" = "#ff8b94"),
                               "Region" = c("North" = "#5c0099",
                                            "Mid" = "#8ecae6", 
                                            "South" = "#797d62", 
                                            "Coastside" = "#ff8b94"),
                               "Sex" = c("Female" = "#5c0099",
                                         "Male" = "#8ecae6"),
                               "Cause" = c("Cardiovascular" = "#5c0099", 
                                           "Respiratory" = "#8ecae6", 
                                           "Stroke" =  "#797d62",
                                           "Intentional injury" = "forestgreen",
                                           "Unintentional injury" = "#ff8b94", 
                                           "Substance use" = "#ffcb69", 
                                           "Mental health" = "#fb8500" ,
                                           'Renal' = "#003049", 
                                           'Genitourinary' = "red"
                               ),
                               "Hospital" = c('krwc' = "#5c0099",
                                              'kssf' = "#8ecae6",
                                              'mp' = "#797d62",
                                              'smmc' = "#ff8b94",
                                              'seq' = "#ffcb69", 
                                              'sdc' = "#fb8500" , 
                                              'stan' = "#003049"))
                      ) +
    scale_x_continuous(breaks = seq(xmin_temp, xmax_temp, by = 4)) +
    labs(x = expression("Mean daily temperature (" * degree * "C)"),
         y = switch(
           moa,
           rr = "IRR",
           pct_chg = "Percent change"
         ),
         title = paste0("Cumulative ", switch(moa,
                                             rr = "relative risk of ",
                                             pct_chg = "percent change in "), 
                        outcome, " compared to the median temperature\nafter ", 
                        lag_num, " days lag, among residents by ", 
                        subset_var, " of San Mateo County, CA")
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
      theme(axis.title.x = element_text(size = 20),
            axis.text.x = element_text(size = 20),
            axis.title.y = element_text(size = 20),
            axis.text.y = element_text(size = 20))
    
    # Plot IRR and histogram together with labels
    cum_rr_hist <- plot_grid(
      cum_rr +
        geom_text_repel(data = labels,
                  aes(x = counterfactual_temp,
                      y = fit_rr_lag,
                      label = group,
                      color = group),
                  direction = 'y',
                  nudge_x = 2.1,
                  fontface = "bold")  +
        coord_cartesian(clip = "off", 
                        xlim = c(2, 34),
                        ylim = c(min(estimates_bylag$fit_rr_lag -1), max(estimates_bylag$fit_rr_lag + 1))) +
        theme(plot.margin = margin(t = 5.5, b = 5.5, r = 100, l = 5.5),
              axis.title.x = element_blank(),
              axis.text.x = element_blank(),
              axis.line.x = element_blank(),
              axis.ticks.x = element_blank()),
      temp_hist +
       coord_cartesian(xlim = c(2, 34)) +
        theme(plot.margin = margin(t = 5.5, b = 5.5, r = 100, l = 5.5)),
      nrow = 2,
      rel_heights = c(4, 1),
      align = "v",
      axis = "lr"
    )
    
    return(cum_rr_hist)
  }
  
  # Plot IRR with labels
  if(with_hist == FALSE) {
    return(cum_rr +
             geom_text(data = labels,
                       aes(x = counterfactual_temp, 
                           y = fit_rr_lag,
                           label = group,
                           color = group),
                       hjust = -0.5,
                       fontface = "bold") + 
             coord_cartesian(clip = "off") +
             theme(plot.margin = margin(r = 70)))
  }
}
