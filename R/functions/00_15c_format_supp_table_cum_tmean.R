################################################################################-
# Function to format supplementary tables of estimates from mean temperature models ----
# Comparing HeatRisk and temperature for local heat surveillance: A time-stratified 
#  case-crossover study of emergency department visits in San Mateo County, 
#  California, 2017–2025
# Juliana GE Bartels
# Updated: 6/22/2026
################################################################################-
## 0: Define model parameters ----
################################################################################-

##
## estimates:             list of estimate csvs

################################################################################-
## 1: Define function ----
################################################################################-

format_supp_table_cum_tmean <- function(estimates){
  
  supp_table <- tibble()
  
  for (i in 1:length(estimates)) {
    oneline <- estimates[[i]] %>% 
      dplyr::select(fit_rr_lag6, lci_rr_lag6, uci_rr_lag6, label) %>% 
      filter(label != 'er_values') %>%
      mutate(fit_rr_lag6 = round((fit_rr_lag6-1) * 100,1), lci_rr_lag6 = round((lci_rr_lag6-1) * 100,1), uci_rr_lag6 = round((uci_rr_lag6-1)*100,1)) %>%
      pivot_wider(values_from = c(fit_rr_lag6, lci_rr_lag6, uci_rr_lag6), names_from = label) %>%
      mutate(strata = names(estimates[i])) %>%
      dplyr::select(strata, fit_rr_lag6_per_05, lci_rr_lag6_per_05, uci_rr_lag6_per_05,
                    fit_rr_lag6_per_10, lci_rr_lag6_per_10, uci_rr_lag6_per_10,
                    fit_rr_lag6_per_90, lci_rr_lag6_per_90, uci_rr_lag6_per_90,
                    fit_rr_lag6_per_95, lci_rr_lag6_per_95, uci_rr_lag6_per_95)
    
    supp_table <- supp_table %>%
      rbind(., oneline)
  }
  
  return(supp_table)
  
}
