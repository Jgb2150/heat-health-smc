################################################################################-
# Prepare climate data at the zip code level ----
# Comparing HeatRisk and temperature for local heat surveillance: A time-stratified 
#  case-crossover study of emergency department visits in San Mateo County, 
#  California, 2017–2025
# Juliana GE Bartels
# Updated: 5/4/2026
################################################################################-

# This file cleans and combines gridMET data for maximum temperature, minimum 
# temperature, and maximum relative humidity from 2017 to 2025, with historical 
# HeatRisk data from weather stations in an around San Mateo County. The output 
# of this script is a daily file with climatic and HeatRisk data for each ZIP code 
# in SMC.

################################################################################-
## 0: Load data ----
################################################################################-

# Load gridMET files for max temp from raw data folder
tmaxfiles <- list.files('data/raw/tmax',
                    full.names = T)
tmax <- terra::rast(tmaxfiles)

# Load gridMET files for minimum daily temperature from raw data folder
tminfiles <- list.files('data/raw/tmin',
                      full.names = T)
tmin <- terra::rast(tminfiles)

# Load gridMET files for max relative humidity from raw data folder
rhmaxfiles <- list.files('data/raw/rmax',
                      full.names = T)
rhmax <- terra::rast(rhmaxfiles)

# Load gridMET files for min relative humidity from raw data folder
rhminfiles <- list.files('data/raw/rmin',
                         full.names = T)
rhmin <- terra::rast(rhminfiles)

# HeatRisk files were obtained through personal communication with NWS HeatRisk employees
# Load HeatRisk files from raw data folder
hr_files <- list.files("data/raw/heat_risk", 
                       full.names = TRUE)

# Clean up HeatRisk files
hr_list <- lapply(hr_files, function(file) {
  read_csv(file) %>%
    mutate(
      ghcn = str_extract(basename(file), "US[A-Z]\\d{8}")  # Extract GHCN station ID
    )
})

# Combine all HeatRisk data into one dataframe
hr <- bind_rows(hr_list) %>%
  clean_names()

# Vector of SMC ZIP codes
smc_zips <- c('94018', '94019', '94020', '94021', '94037', 
              '94038', '94060', '94074', '94002', '94010', 
              '94065', '94070', '94401', '94402', '94403', 
              '94404', '94005', '94014', '94015', '94030', 
              '94044', '94066', '94080', '94128', '94025', 
              '94027', '94028', '94061', '94062', '94063', '94303')

# Load ZIP shapefile to match HeatRisk and temperature data
# This was created in ArcGIS using ZIP code shapefiles and weather station locations
zips_stations <- st_read(here::here(
  "data", "raw", "zips_shpfile",
  paste0("zips_stations_exp", ".shp")
)) %>%
  st_transform(zips_stations, crs = st_crs(tmax)) %>% # transform to match temperature crs
   clean_names() %>% # lowercase column names
    filter(zip_code %in% smc_zips) # filter to restrict to SMC zips

################################################################################-
## 1a: Wrangle tmax (max temperature) data ----
################################################################################-

tmax_allyears <- data.frame()
for (i in tmaxfiles) {
  tmax <- terra::rast(i)
  
  extractedtmax <- exact_extract(tmax, zips_stations,
                                 fun = 'mean',
                                 append_cols = c("zip_code", "po_name",
                                                 "ghcn", "name", "latitude", "longitude"))
  extractedtmax <- extractedtmax %>% pivot_longer(cols = !c("zip_code", "po_name",
                                                            "ghcn", "name", "latitude", "longitude"), 
                                                  names_to = 'doy', values_to = 'tmax_k')
  extractedtmax$doy <- as.integer(str_extract(extractedtmax$doy, "\\d{1,3}"))
  
  stopifnot(unique(extractedtmax$doy) %in% 1:366)
  
  extractedtmax$year <- str_extract(i, "\\d{4}")
  extractedtmax$date <- as.Date(paste0(extractedtmax$year, "-01-01")) + extractedtmax$doy - 1
  
  tmax_allyears <- rbind(tmax_allyears, extractedtmax)
}

# Reformat data
# Calculate temperature in Fahrenheit and correct date variable
tmax_allyears <- tmax_allyears %>%
  mutate(tmax_c = tmax_k - 273.15,
         tmax_f = (9/5) * tmax_c + 32)

################################################################################-
## 1b: Wrangle tmin (minimum temperature) data ----
################################################################################-

# Match temperature data to each ZIP code and take mean of temperature for that area 
# (could change to max, etc)
tmin_allyears <- data.frame()
for (i in tminfiles) {
  tmin <- terra::rast(i)
  
  extractedtmin <- exact_extract(tmin, zips_stations,
                                 fun = 'mean',
                                 append_cols = c("zip_code", "po_name",
                                                 "ghcn", "name", "latitude", "longitude"))
  extractedtmin <- extractedtmin %>% pivot_longer(cols = !c("zip_code", "po_name",
                                                            "ghcn", "name", "latitude", "longitude"), 
                                                  names_to = 'doy', values_to = 'tmin_k')
  extractedtmin$doy <- as.integer(str_extract(extractedtmin$doy, "\\d{1,3}"))
  
  stopifnot(unique(extractedtmin$doy) %in% 1:366)
  
  extractedtmin$year <- str_extract(i, "\\d{4}")
  extractedtmin$date <- as.Date(paste0(extractedtmin$year, "-01-01")) + extractedtmin$doy - 1
  
  tmin_allyears <- rbind(tmin_allyears, extractedtmin)
}

# Reformat data
# Calculate temperature in Fahrenheit and correct date variable
tmin_allyears <- tmin_allyears %>%
  mutate(tmin_c = tmin_k - 273.15,
         tmin_f = (9/5) * tmin_c + 32)

################################################################################-
## 1c: Wrangle max relative humidity data ----
################################################################################-

# Match temperature data to each ZIP code and take mean of maximum relative humidity for that area 
# (could change to max, etc)
rhmax_allyears <- data.frame()
for (i in rhmaxfiles) {
  rhmax <- terra::rast(i)
  
  extractedrhmax <- exact_extract(rhmax, zips_stations,
                                 fun = 'mean',
                                 append_cols = c("zip_code", "po_name",
                                                 "ghcn", "name", "latitude", "longitude"))
  extractedrhmax <- extractedrhmax %>% pivot_longer(cols = !c("zip_code", "po_name",
                                                            "ghcn", "name", "latitude", "longitude"), 
                                                  names_to = 'doy', values_to = 'rhmax')
  extractedrhmax$doy <- as.integer(str_extract(extractedrhmax$doy, "\\d{1,3}"))
  
  stopifnot(unique(extractedrhmax$doy) %in% 1:366)
  
  extractedrhmax$year <- str_extract(i, "\\d{4}")
  extractedrhmax$date <- as.Date(paste0(extractedrhmax$year, "-01-01")) + extractedrhmax$doy - 1
  
  rhmax_allyears <- rbind(rhmax_allyears, extractedrhmax)
}

################################################################################-
## 1d: Wrangle min relative humidity data ----
################################################################################-

# Match temperature data to each ZIP code and take mean of minimum relative humidity for that area 
# (could change to min, etc)
rhmin_allyears <- data.frame()
for (i in rhminfiles) {
  rhmin <- terra::rast(i)
  
  extractedrhmin <- exact_extract(rhmin, zips_stations,
                                  fun = 'mean',
                                  append_cols = c("zip_code", "po_name",
                                                  "ghcn", "name", "latitude", "longitude"))
  extractedrhmin <- extractedrhmin %>% pivot_longer(cols = !c("zip_code", "po_name",
                                                              "ghcn", "name", "latitude", "longitude"), 
                                                    names_to = 'doy', values_to = 'rhmin')
  extractedrhmin$doy <- as.integer(str_extract(extractedrhmin$doy, "\\d{1,3}"))
  
  stopifnot(unique(extractedrhmin$doy) %in% 1:366)
  
  extractedrhmin$year <- str_extract(i, "\\d{4}")
  extractedrhmin$date <- as.Date(paste0(extractedrhmin$year, "-01-01")) + extractedrhmin$doy - 1
  
  rhmin_allyears <- rbind(rhmin_allyears, extractedrhmin)
}

################################################################################-
## 1d: Join temperature and relative humidity data ----
################################################################################-

temp_rh <- tmax_allyears %>%
  left_join(tmin_allyears, by = c("zip_code", "po_name",
                                "ghcn", "name", "latitude", "longitude",
                                "doy", "year", "date")) %>%
  left_join(rhmax_allyears, by = c("zip_code", "po_name",
                                "ghcn", "name", "latitude", "longitude",
                                "doy", "year", "date")) %>%
  left_join(rhmin_allyears, by = c("zip_code", "po_name",
                                   "ghcn", "name", "latitude", "longitude",
                                   "doy", "year", "date"))

################################################################################-
## 1e: Calculate mean daily temperature in F and C ----
################################################################################-

temp_rh <- temp_rh %>%
  mutate(tmean_f = (tmax_f + tmin_f)/2,
         tmean_c = (tmax_c + tmin_c)/2,
         rhmean = (rhmax + rhmin)/2)

################################################################################-
## 2: Add HeatRisk scores ----
################################################################################-

# Combine HeatRisk data with temp_rh data
temp_rh_hr <- temp_rh %>%
  left_join(hr, 
            by = c("date", "ghcn"))

temp_rh_hr_clean <- temp_rh_hr %>%
  rename(tmax = tmax_c,
         tmin = tmin_c,
         tmean = tmean_c,
         station_name = name
         ) %>%
  mutate(heat_risk = case_when(
    heat_risk < 0  ~ NA,
    TRUE ~ heat_risk)) %>%
  dplyr::select("date", "zip_code", "po_name",
                "ghcn", "station_name", 
                "tmax", "tmin", "tmean", "rhmax", "rhmin", "rhmean", "heat_risk")

# Combine HeatRisk level 3 and 4 into one level, called 3, due to small numbers of exposures in levels 3 and 4
temp_rh_hr_clean_2 <- temp_rh_hr_clean %>%
  mutate(heat_risk2 = case_when(heat_risk == 4 ~ 3,
           TRUE ~ heat_risk)) %>%
  rename(heat_risk_orig = heat_risk,
         heat_risk = heat_risk2)

################################################################################-
## 4: Export intermediate climate data file ----
################################################################################-

# Export climate data to intermediate folder
temp_rh_hr_clean_2 %>%
  write.csv(here::here(
    "data", "intermediate", 
    "climate_data.csv"
  ))

################################################################################-
## 5: Clean up ----
################################################################################-

rm(rhmax_allyears, rhmin_allyears, tmax_allyears, tmin_allyears, extractedrhmax, extractedrhmin, extractedtmax, extractedtmin, hr, hr_list, rhmax, rhmin, hr_files, tmax, 
   tmin, temp_rh, temp_rh_hr, temp_rh_hr_clean, zips_stations, temp_rh_hr_clean_2,
   smc_zips)
