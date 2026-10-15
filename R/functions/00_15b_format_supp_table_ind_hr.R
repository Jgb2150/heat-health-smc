################################################################################-
# Function to format supplementary tables of estimates from HeatRisk models ----
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

format_supp_table_ind_hr <- function(estimates){
  
  supp_table <- tibble()
  
  for (i in 1:length(estimates)) {
    oneline <- estimates[[i]] %>% 
      filter(counterfactual_temp %in% c(2, 3)) %>%
      mutate(across(-c(counterfactual_temp, label, fit_rr_refT, lci_rr_refT, uci_rr_refT),
                    ~ round((.x - 1 ) * 100, 1))) %>%
      pivot_longer(cols = -c(counterfactual_temp, label),
                   names_to = c('fit', 'lag'),
                   names_pattern = '(.*)_rr_(.*)',
                   values_to = 'estimate') %>%
      pivot_wider(names_from = 'fit', 
                  values_from = 'estimate') %>%
      mutate(strata = names(estimates[i])) 
    
    supp_table <- supp_table %>%
      rbind(., oneline)
  }
  
  return(supp_table)
  
}
