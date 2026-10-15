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

format_supp_table_cum_hr <- function(estimates){
  
  supp_table <- tibble()
  
  for (i in 1:length(estimates)) {
    oneline <- estimates[[i]] %>% 
      dplyr::select(fit_rr_lag6, lci_rr_lag6, uci_rr_lag6, counterfactual_temp) %>% 
      mutate(fit_rr_lag6 = round((fit_rr_lag6-1) * 100,1), lci_rr_lag6 = round((lci_rr_lag6-1) * 100,1), uci_rr_lag6 = round((uci_rr_lag6-1)*100,1)) %>%
      pivot_wider(values_from = c(fit_rr_lag6, lci_rr_lag6, uci_rr_lag6), names_from = counterfactual_temp) %>%
      mutate(strata = names(estimates[i])) %>%
      dplyr::select(strata, fit_rr_lag6_1, lci_rr_lag6_1, uci_rr_lag6_1,
                    fit_rr_lag6_2, lci_rr_lag6_2, uci_rr_lag6_2,
                    fit_rr_lag6_3, lci_rr_lag6_3, uci_rr_lag6_3)
    
    supp_table <- supp_table %>%
      rbind(., oneline)
  }
  
  return(supp_table)
  
}
