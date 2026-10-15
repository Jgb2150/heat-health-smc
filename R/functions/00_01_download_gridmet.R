################################################################################-
# Function to download gridMET data
# Comparing HeatRisk and temperature for local heat surveillance: A time-stratified 
#  case-crossover study of emergency department visits in San Mateo County, 
#  California, 2017–2025
# Juliana GE Bartels
# Updated: 6/2/2025
################################################################################-
## 0: Define model parameters ----
################################################################################-

# Model parameters:
## file_names:   vector of file names to download from gridMET (e.g., 'rmax_2017')
##
## directory:    folder name for raw data download (e.g., 'rmax')
##

################################################################################-
## 1: Define function ----
################################################################################-

download_gridmet <- function(file_names, directory) {
  if (!file.exists(here::here('data', 'raw', directory, paste0(file_names[i], '.nc')))) {
    
    download.file(url = paste0("https://www.northwestknowledge.net/metdata/data/", file_names[i], ".nc"), 
                  destfile = here::here('data', 
                                        'raw', 
                                        directory, 
                                        paste0(file_names[i], '.nc')
                  )
    )
  }
}
