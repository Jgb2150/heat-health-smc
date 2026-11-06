---

editor_options: 
  markdown: 
    wrap: sentence
---

# Comparing HeatRisk and temperature for local heat surveillance: A time-stratified case-crossover study of emergency department visits in San Mateo County, California, 2017–2025

### Juliana GE Bartels, Sayema Badar, and Simon K Camponuri

This is the code repository for our analysis of the association of all-cause ED visits and all-cause mortality with ambient temperature in San Mateo County, CA, from 2017 to 2025.

We conducted a time-stratified case-crossover study with distributed lag non-linear models to estimate the percent change in ED visit and death rates associated with daily mean temperature compared to the median temperature, and for HeatRisk levels minor to major/extreme compared to little to none.

A thorough description of our methods and results is available in a forthcoming manuscript.

Time-stratitifed case crossover code and coding structure adapted from:

Elser, H., S. Rowland, et al. Ambient temperature and risk of urinary tract infection in California: A time-stratified case-crossover study using electronic health records. *Environment International* 165, 107303 (2022). Code available here: <https://github.com/s-rowland/DailyTemp-UTI>

Code for analysis of ED visits attributable to heat adapted from:

Vicedo-Cabrera, A. M.; Sera, F.; Gasparrini, A. Hands-on Tutorial on a Modeling Framework for Projections of Climate Change Impacts on Health. *Epidemiology* **2019**, *30* (3), 321–329. <https://doi.org/10.1097/EDE.0000000000000982>

## Description of code repository

Our analysis uses syndromic surveillance data and death data that are not available for public use, as well as privately-obtained NWS HeatRisk data. If you would like to run the code to test it, we have created a synthetic dataset that can be used to run a subset of our analyses. More information below.

The **R** folder contains all of the scripts that are necessary to conduct our analysis. These are numbered for organizational purposes.

Files are organized as follows: 
- 0: These are the main scripts that are necessary to run the full project. 
  - *0_00_set_up.R* loads packages and functions, builds folder structure, and creates templates for tables for model results. On line 26/27 of this file, you can set the data to 'original' or 'synth.' If you do not have access to the original data, set this to synthetic. - *0_01_prepare_data.R* downloads climate data, cleans and restructures exposure and outcome data, and creates the final dataframe to be used for all analyses.\
  - *0_02_run_all_models.R* runs all models, including main models, stratified models, and sensitivity models. If data_source is set to 'synthetic', this will run only a subset of the models. 
  - *0_03_plot_results.R* runs all scripts to plot results and save the plots to the outputs folder. 
- 1: These are the data cleaning files, called in *0_01_prepare_data.R*. 
- 2: These are the model fitting files, called in *0_02_run_all_models.R*. 
- 3: These are the plotting files, called in *0_03_plot_results.R*. 
- Functions are stored in the 'functions' folder, and have the heading 00. All functions used in scripts are defined in these files.
