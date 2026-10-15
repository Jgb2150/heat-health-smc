################################################################################-
# Download climate data from gridMET for 2017 to 2025 ----
# Comparing HeatRisk and temperature for local heat surveillance: A time-stratified 
#  case-crossover study of emergency department visits in San Mateo County, 
#  California, 2017–2025
# Juliana GE Bartels
# Updated: 6/1/2026
################################################################################-

# This file downloads gridMET minimum and maximum temperature and relative humidity
# data from https://www.climatologylab.org/gridmet.html.

################################################################################-
## 1: Download maximum temperature data ----
################################################################################-

# Define maximum temperature file names
tmax_file_names <- c('tmmx_2017', 
                     'tmmx_2018', 
                     'tmmx_2019', 
                     'tmmx_2020', 
                     'tmmx_2021', 
                     'tmmx_2022', 
                     'tmmx_2023', 
                     'tmmx_2024', 
                     'tmmx_2025')

# Define maximum temperature directory
tmax_directory <- 'tmax'

# Download maximum temperature files
for(i in 1:length(tmax_file_names)) {
  download_gridmet(tmax_file_names, tmax_directory)
}

################################################################################-
## 2: Download minimum temperature data ----
################################################################################-

# Define minimum temperature file names
tmin_file_names <- c('tmmn_2017', 
                     'tmmn_2018', 
                     'tmmn_2019', 
                     'tmmn_2020', 
                     'tmmn_2021', 
                     'tmmn_2022', 
                     'tmmn_2023', 
                     'tmmn_2024', 
                     'tmmn_2025')

# Define minimum temperature directory
tmin_directory <- 'tmin'

# Download minimum temperature files
for(i in 1:length(tmin_file_names)) {
  download_gridmet(tmin_file_names, tmin_directory)
}

################################################################################-
## 3: Download maximum relative humidity data ----
################################################################################-

# Define maximum relative humidity file names
rmax_file_names <- c('rmax_2017', 
                     'rmax_2018', 
                     'rmax_2019', 
                     'rmax_2020', 
                     'rmax_2021', 
                     'rmax_2022', 
                     'rmax_2023', 
                     'rmax_2024', 
                     'rmax_2025')

# Define maximum relative humidity directory
rmax_directory <- 'rmax'

# download maximum relative humidity files
for(i in 1:length(rmax_file_names)) {
  download_gridmet(rmax_file_names, rmax_directory)
}

################################################################################-
## 4: Download minimum relative humidity data ----
################################################################################-

# Define minimum relative humidity file names
rmin_file_names <- c('rmin_2017', 
                     'rmin_2018', 
                     'rmin_2019', 
                     'rmin_2020', 
                     'rmin_2021', 
                     'rmin_2022', 
                     'rmin_2023', 
                     'rmin_2024', 
                     'rmin_2025')

# Define minimum relative humidity directory
rmin_directory <- 'rmin'

# download minimum relative humidity files
for(i in 1:length(rmin_file_names)) {
  download_gridmet(rmin_file_names, rmin_directory)
}

################################################################################-
## 4: Clean up ----
################################################################################-

rm(tmax_file_names, tmax_directory, tmin_file_names, tmin_directory, rmax_file_names, rmax_directory)
