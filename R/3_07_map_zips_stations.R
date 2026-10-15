################################################################################-
# Map: Reference maps ---
# Comparing HeatRisk and temperature for local heat surveillance: A time-stratified 
#  case-crossover study of emergency department visits in San Mateo County, 
#  California, 2017–2025 
# Juliana GE Bartels
# Updated: 5/21/2026
################################################################################-

# This file makes reference maps of zip codes and weather stations, as well as 
# locations of regions in SMC.

################################################################################-
## 0: Upload data ----
################################################################################-

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
  st_transform(zips_stations, crs = 'NAD83') %>%
  clean_names() %>% # lowercase column names
  filter(zip_code %in% smc_zips) # filter to restrict to SMC zips

# Load weather station location
station_metadata <- read.csv(here::here('data', 'raw', 'zips_shpfile', 'station_metadata.csv')) %>%
  clean_names() %>%
  #st_as_sf(., coords = c("longitude", "latitude"), crs = st_crs(tmax)) %>%
  rename(station_name = name)

# Santa Cruz zips
sc_zips <- c('95006', '95060', '95017', '95005', '95007', '95018', '94060', '95033', '95066', '95065')

# San Jose po_names
sj_poname <- c('Los Altos', 'Portola Valley', 'Cupertino', 'Mountain View', 'Stanford', 'Palo Alto', 'San Jose', 'Campbell', 'Saratoga',
               'Morgan Hill', 'Santa Clara', 'Sunnyvale', 'Los Gatos')

# Load all CA zips
ca_zips <- st_read(here::here(
  "data", "raw", "ca_zips",
  paste0("zip_poly", ".shp")
)) %>%
  st_transform(ca_zips, crs = 'NAD83') %>%
  clean_names() %>% # lowercase column names 
  filter(zip_code %in% sc_zips | po_name %in% sj_poname | po_name == "San Francisco") # keep zips in SF and Santa Cruz

################################################################################-
## 1: Create ZIP code and station map ----
################################################################################-

stations_smc <- unique(zips_stations$ghcn)

colors <- c("#fcca46", "#a1c181", "#fe7f2d", "#e0b1cb", "#233d4d")

ggplot() +
  geom_sf(data = ca_zips, fill = "gray", alpha = 0.1) +
  geom_sf(data = zips_stations,
          aes(fill = factor(ghcn)), 
          show.legend = c(fill = FALSE)) +
  theme_minimal() +
  geom_point(data = station_metadata %>% filter(ghcn %in% stations_smc), 
             aes(x = longitude, y = latitude, fill = ghcn),
             shape = 21,        # Shape with fill + border
             color = "black",   # Outline color
             size = 5,          # Point size
             stroke = 1.5,
             show.legend = FALSE) +
  scale_fill_manual(values = colors) +
  geom_label(data = station_metadata %>% filter(ghcn %in% stations_smc),
                aes(x = longitude, y = latitude, label = station_name),
                size = 3,
             fontface = "bold",
             
             fill = alpha("white", 0.85),
             color = "black",
             linewidth = 0.5,
             label.r = unit(0.15, "lines"),
             label.padding = unit(0.15, "lines"),
             nudge_y = -0.02) +
  coord_sf(xlim = c(-122.55, -122.02),
           ylim = c(37.08, 37.78)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

ggsave(filename = here::here('outputs', 'plots', 'maps', 'zip_stations_map.png'))

################################################################################-
## 2: Create region reference map ----
################################################################################-

zips_stations %>%
  mutate(region = case_when(zip_code %in% c('94018', '94019', '94020', '94021', '94037',
                                         '94038', '94060', '94074') ~ 'Coastside',
                            zip_code %in% c('94002', '94010', '94065', '94070', '94401',
                                         '94402', '94403', '94404') ~ 'Mid',
                            zip_code %in% c('94005', '94014', '94015', '94030', '94044',
                                         '94066', '94080', '94128') ~ 'North',
                            zip_code %in% c('94025', '94027', '94028', '94061', '94062',
                                         '94063', '94303') ~ 'South',
                            TRUE ~ NA)) %>%
  group_by(region) %>%
  summarize(geometry = st_union(geometry), .groups = 'drop') %>%
  mutate(value = c(1,2,3,4)) %>%
  ggplot() +
  geom_sf(aes(fill = factor(value)), 
          show.legend = c(fill = FALSE)) +
  scale_fill_manual(values = c("1" = "#ecf39e", "2" = "#90a955", "3" = "#4f772d", "4" = "#31572c" )) +
  theme_minimal() +
  geom_sf_label(aes(label = region),
                size = 5,
                nudge_y = 0.02)

ggsave(filename = here::here('outputs', 'plots', 'maps', 'reference_map.png'), 
       height = 8, width = 12 )

################################################################################-
## 2: Clean up ----
################################################################################-

rm(smc_zips, zips_stations, station_metadata, stations_smc, sj_poname, sc_zips, ca_zips, colors)
