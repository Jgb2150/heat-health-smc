################################################################################-
# Plot results ----
# Comparing HeatRisk and temperature for local heat surveillance: A time-stratified 
#  case-crossover study of emergency department visits in San Mateo County, 
#  California, 2017–2025
# Juliana GE Bartels
# Updated: 9/29/2026
################################################################################-

# This file plots all results. 

################################################################################-
## 0: Set timer ---- 
################################################################################-

# This takes about 45 minutes
tic("Plotting complete.")

################################################################################-
## 1: Run all scripts for plotting ---- 
################################################################################-


if(data_source == 'original') {
  
  # Table 1
  source(here::here('R', '3_00_table1.R'))
  
  # ED visits 
  
    # Individual lag plots - main models and sensitivity models
    source(here::here('R', '3_01a_plot_ind_ed.R'))
    source(here::here('R', '3_01c_plot_ind_ed_nocov.R'))
    source(here::here('R', '3_01e_plot_ind_ed_restricthosp.R'))

    # Individual lag plots - EMM models
    source(here::here('R', '3_02a_plot_ind_ed_emm_age.R'))
    source(here::here('R', '3_02b_plot_ind_ed_emm_region.R'))
    source(here::here('R', '3_02c_plot_ind_ed_emm_sex.R'))
    source(here::here('R', '3_02d_plot_ind_ed_emm_cause.R'))

    # Cumulative plots - main models and sensitivity models
    source(here::here('R', '3_03a_plot_cum_ed.R'))
    source(here::here('R', '3_03c_plot_cum_ed_nocov.R'))
    source(here::here('R', '3_03e_plot_cum_ed_restricthosp.R'))

    # Cumulative plots - EMM models
    source(here::here('R', '3_04a_plot_cum_ed_emm_age.R'))
    source(here::here('R', '3_04b_plot_cum_ed_emm_region.R'))
    source(here::here('R', '3_04c_plot_cum_ed_emm_sex.R'))
    source(here::here('R', '3_04d_plot_cum_ed_emm_cause.R'))
    source(here::here('R', '3_04d_plot_cum_ed_emm_causeunder5.R'))
    source(here::here('R', '3_04d_plot_cum_ed_emm_cause5to17.R'))
    source(here::here('R', '3_04d_plot_cum_ed_emm_cause18to64.R'))
    source(here::here('R', '3_04d_plot_cum_ed_emm_cause65plus.R'))
  
    # Attributable number plots
    source(here::here('R', '3_05a_plot_an_ed.R'))

  # Death
  
    # Individual lag plots - main models and sensitivity models
    source(here::here('R', '3_01b_plot_ind_death.R'))
    source(here::here('R', '3_01d_plot_ind_death_nocov.R'))
  
    # Individual lag plots - EMM models
    source(here::here('R', '3_02a_plot_ind_death_emm_age.R'))
    source(here::here('R', '3_02b_plot_ind_death_emm_region.R'))
    source(here::here('R', '3_02c_plot_ind_death_emm_sex.R'))
    source(here::here('R', '3_02d_plot_ind_death_emm_cause.R'))
    
    # Cumulative plots - main models and sensitivity models
    source(here::here('R', '3_03b_plot_cum_death.R'))
    source(here::here('R', '3_03d_plot_cum_death_nocov.R'))
  
    # Cumulative plots - EMM models
    source(here::here('R', '3_04a_plot_cum_death_emm_age.R'))
    source(here::here('R', '3_04b_plot_cum_death_emm_region.R'))
    source(here::here('R', '3_04c_plot_cum_death_emm_sex.R'))
    source(here::here('R', '3_04d_plot_cum_death_emm_cause.R'))
    
    # Attributable number plots
    source(here::here('R', '3_05b_plot_an_death.R'))
  
  # Maps
  source(here::here('R', '3_07_map_zips_stations.R'))
  
  # Supplementary tables and figures
  source(here::here('R', '3_08a_make_supp_tables_ind.R'))
  source(here::here('R', '3_08b_make_supp_tables_cum.R'))
  source(here::here('R', '3_09_make_fig_supp_all_ers.R'))
  
}


if(data_source == 'synth') {
  # Table 1
  source(here::here('R', '3_00_table1.R'))
  
  # ED visit
  
    # Individual lag plots - main models and sensitivity models
    source(here::here('R', '3_01a_plot_ind_ed.R'))
    source(here::here('R', '3_01c_plot_ind_ed_nocov.R'))

    # Individual lag plots - EMM models
    source(here::here('R', '3_02c_plot_ind_ed_emm_sex.R'))
    
    # Cumulative plots - main models and sensitivity models
    source(here::here('R', '3_03a_plot_cum_ed.R'))
    source(here::here('R', '3_03c_plot_cum_ed_nocov.R'))
  
    # Cumulative plots - EMM models
    source(here::here('R', '3_04c_plot_cum_ed_emm_sex.R'))
    
    # Attributable number plots
    source(here::here('R', '3_05a_plot_an_ed.R'))
    
  # Death
  
    # Individual lag plots - main models and sensitivity models
    source(here::here('R', '3_01b_plot_ind_death.R'))
    source(here::here('R', '3_01d_plot_ind_death_nocov.R'))
    
    # Individual lag plots - EMM models
    source(here::here('R', '3_02c_plot_ind_death_emm_sex.R'))
    
    # Cumulative plots - main models and sensitivity models
    source(here::here('R', '3_03b_plot_cum_death.R'))
    source(here::here('R', '3_03d_plot_cum_death_nocov.R'))
    
    # Cumulative plots - EMM models
    source(here::here('R', '3_04c_plot_cum_death_emm_sex.R'))
    
    # Attributable number plots
    source(here::here('R', '3_05b_plot_an_death.R'))
  
  # Maps
  source(here::here('R', '3_07_map_zips_stations.R'))
  
  # Supplementary tables and figures
  source(here::here('R', '3_08a_make_supp_tables_ind.R'))
  source(here::here('R', '3_08b_make_supp_tables_cum.R'))
  source(here::here('R', '3_09_make_fig_supp_all_ers.R'))
  
}

################################################################################-
## 0: End timer ---- 
################################################################################-

toc()

