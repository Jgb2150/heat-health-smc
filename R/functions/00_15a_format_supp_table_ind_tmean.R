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

format_supp_table_ind_tmean <- function(estimates){
  
  supp_table <- tibble()
  
  for (i in 1:length(estimates)) {
    oneline <- estimates[[1]] %>% 
      filter(label %in% c('per_05', 'per_95')) %>%
      dplyr::select(-c(counterfactual_temp, fit_rr_refT, lci_rr_refT, uci_rr_refT)) %>%
      mutate(across(-c(label),
                    ~ round((.x - 1 ) * 100, 1))) %>% 
      pivot_longer(cols = -c(label),
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
