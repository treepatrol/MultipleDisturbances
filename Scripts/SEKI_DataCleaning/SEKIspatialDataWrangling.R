# SEKI Spatial Data Wrangling 
# 19 June 2025
# Author: Jenny Cribbs

# Inputs: tree data from SEKI data entry (can directly import GPS coordinates later)

# Code Description: (1) calculate tree position based on plot beginning, dOut, and dSide (2) choose GPS or relative position if some trees have both (preference will be evaluated on a plot by plot basis) (3) visualize result (4) convert all points to decimal degrees. 

# Outputs: (1) a csv file with lat/long coordinates for each plot beginning and end point. (2) a csv file with lat/long coordinates for each tree.
# Desired output should eventually be a CSV or use RPostgres to direcly populate relational database

library(tidyverse)
library(terra)
library(tigris)
library(tidycensus)
library(rmapshaper)
library(sf)
library(tmap)
library(readxl)
library(writexl)

# Set the working directory
setwd("/Users/jennifercribbs/Documents/R-Projects/climate-data")

# load user defined functions relativeTreeCalculation and calculateTreePositions by running 0_TreePositionCalculationFunctions.R first
source("/Users/jennifercribbs/Documents/R-Projects/MultipleDisturbances/Scripts/UnderstoryCleaning/0_TreePositionCalculationFunctions.R")

# read in SEKI data

# plot data
plots <- read_csv('/Users/jennifercribbs/Documents/TreePatrol.org/Analysis/SEKIanalysis/SEKI_all_PlotData_20250821.csv')

# tree data
trees2024 <- read_csv('/Users/jennifercribbs/Documents/R-Projects/MultipleDisturbances/dataSandbox/SEKI_2024_TreeFieldData - Sheet1.csv')

trees2022 <- read_csv('/Users/jennifercribbs/Documents/SEKI_beetles/Data/Beetles Project Tree Data - Sheet1.csv')

# select and re-order columns for database, rename using snake case
trees2022 <- select(Plot, )

#resinCount <- read_csv("/Users/jennifercribbs/Documents/SEKI_beetles/ResinDuctRoughCount_SEKI2021.csv")
# change dOut_m to numeric (no NAs introduced)
#pilas$dOut_m <- as.numeric(pilas$dOut_m)

# check for unique species codes
unique(trees$Species)

# filter out PILA trees only
pilas <- trees %>% filter(Species == "PILA" & Year == 2024)

# filter out PILA plots only
pilaPlots <- plots %>% filter (Species == "PILA")

# convert dSide Right to numeric (no NAs introduced)
#pilas$dSideR_m <- as.numeric(pilas$dSideR_m)

# PILA_UTM_E is character type because of preserving the leading zeros in Excel
# coerce PILA_UTM_E to numeric (no NAs added)
# bring in kml files from gps units
kml_66i <- st_read("/Users/jennifercribbs/Documents/YOSE/Waypoints/Recently Read from GPSMAP 66i (Unit ID 3404379582).kml") %>% 
  mutate(Source = "66i")
kml_66sr <- st_read("/Users/jennifercribbs/Documents/YOSE/Waypoints/Recently Read from GPSMAP 66sr (Unit ID 3377332670).kml") %>% 
  mutate(Source = "66sr")

# create one kml from both units
kml_sf <- rbind(kml_66i, kml_66sr)
st_crs(kml_sf) # already lat/long WGS84 decimal degrees

# remove rows with name Coordinates (not from YOSE project and no Z axis)
filter(kml_sf, Name == "Coordinates")
kml_sf = filter(kml_sf, Name != "Coordinates")

# replace geometry column of kml with lat and long
kml = bind_cols(st_drop_geometry(kml_sf), st_coordinates(kml_sf))

# Convert both to lowercase and trim any whitespace
pilas$GPSdevice <- trimws(tolower(pilas$GPSdevice))
kml$Source <- trimws(tolower(kml$Source))
i <- filter(pilas, GPSdevice == "66i")
sr <- filter(pilas, GPSdevice == "66sr")
# there is no si unit--but waypoint name should allow for matching
si <- filter(pilas, GPSdevice == "66si")
si_check <- si %>% select(occurrenceID, GPSdevice, dOut_m, dSide_m, PILA_waypoint, PILA_UTM_E, PILA_UTM_N, crew, date, plotID)
pilas <- pilas %>%
  mutate(GPSdevice = ifelse(GPSdevice == "66si", "66i", GPSdevice))

# 30 points were taken on the inreach that was returned to UCD
# fortunately they are entered as utms, so hopefully no errors 
# we will not be able increase precision by using the device directly, but shouldn't be a big deal
inreach <- filter(pilas, GPSdevice == "inreach")

# normalize waypoint names to ensure they have the same length
# Remove any rows where the waypoint name contains letters

# this keeps only numeric waypoint names (570 of 639)
# this should exclude points from other projects
kml_filtered <- kml[!grepl("[a-zA-Z]", kml$Name), ]

# deal with 66i names that end in 1--we don't know why the GPS did this
# Remove trailing "1" from the first 190 lines
# hard coding works fine
#kml_filtered$Name[1:190] <- gsub("1$", "", kml_filtered$Name[1:190])
# but substr is much better 
# substr keeps the subset between the index values specified in arguments 2 (first) and 3 (last)
kml_filtered$Name <- substr(kml_filtered$Name, 1, 4)

kml_filtered <- kml_filtered %>% 
  mutate(kml_id = paste(Source, Name, sep = "_"))

# Check rows with NAs prior to digit normalization (1490)
sum(is.na(pilas$PILA_waypoint))
# Check rows with GPSdevice == "66i" and valid PILA_waypoint (195)
sum(!is.na(pilas$PILA_waypoint) & pilas$GPSdevice == "66i")
# Check rows with GPSdevice == "66sr" and valid PILA_waypoint (56)
sum(!is.na(pilas$PILA_waypoint) & pilas$GPSdevice == "66sr")

# Replace entries that are empty, all whitespace, or variations of "NA" with true NA
pilas$PILA_waypoint <- ifelse(grepl("^\\s*NA\\s*$|^\\s*$", 
                                    pilas$PILA_waypoint, ignore.case = TRUE), 
                              NA_character_, pilas$PILA_waypoint)
# check that cleaning worked
# Verify no problematic strings remain
any(grepl("^\\s*NA\\s*$|^\\s*$", pilas$PILA_waypoint, ignore.case = TRUE))  # Should return FALSE

# normalize data entry of PILA waypoint names based on the device source
# for 66i: ensure 4 digits
pilas$PILA_waypoint[!is.na(pilas$PILA_waypoint) & pilas$GPSdevice == "66i"] <- 
  formatC(as.numeric(pilas$PILA_waypoint[!is.na(pilas$PILA_waypoint) & pilas$GPSdevice == "66i"]), width = 4, flag = "0")
# for 66sr: ensure 4 digits
pilas$PILA_waypoint[!is.na(pilas$PILA_waypoint) & pilas$GPSdevice == "66sr"] <-
  formatC(as.numeric(pilas$PILA_waypoint[!is.na(pilas$PILA_waypoint) & pilas$GPSdevice == "66sr"]), width = 4, flag = "0")
# create kml_id
pilas <- pilas %>% 
  mutate(kml_id = paste(GPSdevice, PILA_waypoint, sep = "_"))
# clean up to clearly name true NAs
pilas$kml_id <- ifelse(grepl(pattern ="_NA", pilas$kml_id), NA_character_, pilas$kml_id)
# investigae duplicate waypoints
non_na_duplicates <- pilas$kml_id[duplicated(pilas$kml_id) & !is.na(pilas$kml_id)]
unique(non_na_duplicates)
# join pila and kml data
pilas_kml <- left_join(pilas, kml_filtered, by = "kml_id")

# lots of NAs check the kml_ids
table(pilas$PILA_waypoint %in% kml_filtered$Name)
table(kml_filtered$Name %in% pilas$PILA_waypoint)

# Check for any non-printable characters--None
table(grepl("[[:cntrl:]]", pilas$PILA_waypoint))
table(grepl("[[:cntrl:]]", kml_filtered$Name))
# Add position information for unusual cases
# modify specific values for eventID == "E58-PILA16"--calculated by hand using trig from WP29 as described in the notes
pilas_kml <- pilas_kml %>%
  mutate(
    est_dOut_m = if_else(occurrenceID == "E58-PILA16", 63.5, est_dOut_m),  # Modify dOut_m
    est_dSideL = if_else(occurrenceID == "E58-PILA16", -47, est_dSideL)  # Modify dSide
  )

# modify specific values for eventID == "E36-PILA100"--imputed dSide after the fact based nearby trees. This was apparently a sapling in a cluster. Position error likely within 10m, but may not match accuracy of other trees. 
pilas_kml <- pilas_kml %>%
  mutate(
    est_dSideR = if_else(occurrenceID == "E36-PILA100", 5, est_dSideR)  # Modify dSide
  )

# fix incorrect data entry for positive dSideL values (manually reviewed to ensure this was the problem)
pilas_kml <- pilas_kml %>%
  mutate(dSideL_m = if_else(!is.na(dSideL_m) & dSideL_m > 0, -dSideL_m, dSideL_m),
         est_dSideL = if_else(!is.na(est_dSideL) & est_dSideL > 0, -est_dSideL, est_dSideL))

# combine right and left sides
pilas_kml <- pilas_kml %>%
  mutate(dSide_combined = case_when(
    !is.na(dSideR_m) & dSideR_m >= 0 ~ dSideR_m,
    !is.na(dSideL_m) & dSideL_m <= 0 ~ dSideL_m,
    TRUE ~ NA_real_
  ))
summary(pilas_kml)

# combine right and left sides 
pilas_kml <- pilas_kml %>% mutate(est_dSide = case_when(
  !is.na(est_dSideR) & est_dSideR >= 0 ~ est_dSideR,
  !is.na(est_dSideL) & est_dSideL <= 0 ~ est_dSideL,
  TRUE ~ NA_real_))

# finalize dSide by prioritizing measured over estimated values
pilas_kml <- pilas_kml %>% 
  mutate(dSide_final = case_when(!is.na(dSide_m) ~ dSide_m,
                                 !is.na(dSide_combined) ~ dSide_combined,
                                 !is.na(est_dSide) ~ as.numeric(est_dSide),
                                 TRUE ~ NA_real_))
# finalize dOut by prioritizing measured over estimated values
pilas_kml <- pilas_kml %>% 
  mutate(dOut_final = case_when(!is.na(dOut_m) ~ dOut_m,
                                !is.na(est_dOut_m) ~ as.numeric(est_dOut_m),
                                TRUE ~ NA_real_))                               
# create flag columns to distinguish position data quality
pilas_kml <- pilas_kml %>%  
  mutate(
    dOut_flag = case_when(
      !is.na(dOut_m) ~ "measured",
      !is.na(est_dOut_m) ~ "estimated",
      TRUE ~ NA_character_  
    ),
    dSide_flag = case_when(
      !is.na(dSide_m) ~ "measured",
      !is.na(dSide_combined) ~ "measured",
      !is.na(est_dSide) ~ "estimated",
      TRUE ~ NA_character_
    )
  )
# plot UTM columns need to be numeric 
pilas_kml$plot_beg_UTM_E <- as.numeric(pilas_kml$plot_beg_UTM_E)
pilas_kml$plot_beg_UTM_N <- as.numeric(pilas_kml$plot_beg_UTM_N)

# run calculate positions function to convert dOut and dSide to UTMs
pila_positions <- calculate_tree_positions(pilas_kml)

# combine UTMs from dOut, dSide and GPS transcriptions
pila_positions <- pila_positions %>%
  mutate(
    UTM_E = coalesce(tree_UTM_E, PILA_UTM_E),  # prioritize dOut/dSide
    UTM_N = coalesce(tree_UTM_N, PILA_UTM_N)   # prioritize dOut/dSide
  )
# separate the data into those with UTM coordinates and those with XY coordinates
utm_data <- pila_positions %>% filter(!is.na(UTM_E) & !is.na(UTM_N))
xy_data <- pila_positions %>% filter(!is.na(X) | !is.na(Y))

utm_lat_long_fixed <- utm_data %>%
  mutate(
    # identify rows where coordinates are out of expected range
    transposed_coords = UTM_E > 1000000 & UTM_N < 4000000,
    corrected_UTM_E = if_else(transposed_coords, UTM_N, UTM_E),
    corrected_UTM_N = if_else(transposed_coords, UTM_E, UTM_N)
  ) %>%
  # Replace the original UTM coordinates with corrected ones, no filtering
  select(-UTM_E, -UTM_N, -transposed_coords) %>%
  rename(
    UTM_E = corrected_UTM_E,
    UTM_N = corrected_UTM_N
  )

# check summary of the corrected coordinates
summary(utm_lat_long_fixed$UTM_E) # range is good now
summary(utm_lat_long_fixed$UTM_N) # range is good now

# convert UTMs to lat/long decimal degrees
lat_long <- utm_lat_long_fixed %>%
  mutate(UTM_zone = case_when(
    UTM_E > 600000 ~ 10,  # zone 10
    UTM_E < 600000 ~ 11   # zone 11
  )) %>%
  group_split(UTM_zone) %>%
  map_dfr(function(df) {
    # Convert the data frame to an SF object with proper coordinates
    df_sf <- st_as_sf(df, 
                      coords = c("UTM_E", "UTM_N"), 
                      crs = paste0("+proj=utm +zone=", unique(df$UTM_zone), " +datum=NAD83"))
    
    # Transform the CRS to lat/long
    df_sf <- st_transform(df_sf, crs = 4326)
    
    # Extract the latitude and longitude coordinates
    df_sf <- df_sf %>%
      mutate(
        latitude = st_coordinates(df_sf)[, 2],  # Extract latitude
        longitude = st_coordinates(df_sf)[, 1]  # Extract longitude
      )
    
    # Return the data frame without the geometry column
    df_sf %>%
      st_drop_geometry()  # Remove geometry column to simplify the data
  })
# trim columns before joining
pilas_kml_trimmed <- pilas_kml %>% select (occurrenceID, X, Y)
lat_long_trimmed <- lat_long %>% select (occurrenceID, longitude, latitude)

# join converted coordinates back to the original dataset
final_pila_positions <- left_join(pilas_kml_trimmed, lat_long_trimmed, by = "occurrenceID")

# how many trees are missing any position information?
missing_positions <- final_pila_positions %>%
  filter(is.na(X) & is.na(Y) & is.na(longitude) & is.na(latitude))

nrow(missing_positions)  # How many trees have no position data at all? 89-->56 after keeping the flipped coordinates

# combine XY positions from the GPS with converted positions
gbif_pilas <- final_pila_positions %>% 
  mutate(
    verbatimLatitude = coalesce (Y, latitude),
    verbatimLongitude = coalesce (X, longitude)) %>% 
  select (occurrenceID, verbatimLatitude, verbatimLongitude)

missing_positions <- gbif_pilas %>% filter(is.na(verbatimLatitude) & is.na(verbatimLongitude))

# write out CSV with latitude and longitude 
write.csv(occurrence_positions, "/Users/jennifercribbs/Documents/YOSE/Analysis/MultipleDisturbances/outputSandbox/occurrence_positions.csv")

#~~~~~~~~~~~~~~~~~~~~~ Mapping Trees ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Bring in the NPS boundary for YOSE
nps <- st_read("/Users/jennifercribbs/Documents/TreePatrol.org/Analysis/Data/nps_boundary") %>% filter(UNIT_CODE == "YOSE")
st_crs(nps) # 6269
nps <- st_transform(nps, crs = 4326)
# Bring in county boundaries
counties <- st_read("/Users/jennifercribbs/Documents/YOSE/Analysis/MultipleDisturbances/dataSandbox/tl_2024_us_county/tl_2024_us_county.shp") %>% filter(NAME == "Mariposa" | NAME == "Madera" | NAME == "Tuolumne")
# Reproject county boundaries to lat/long
counties <- st_transform(counties, crs = 4326)

# filter out missing positions
occurrences <-  occurrence_positions %>%
  filter(!is.na(verbatimLatitude) & !is.na(verbatimLongitude)) 

occurrences_sf <- st_as_sf(occurrences, coords = c("verbatimLongitude", "verbatimLatitude"), crs = 4326)
# Map all trees and gps points

tmap_mode("view")
tm_shape(nps) +
  tm_polygons(col = "gray",
              title = "Yosemite") +
  tm_shape(kml_sf) +
  tm_dots(col = "black") +
  tm_shape(occurrences_sf) +
  tm_dots(col = "#00FF00", border.col = "black", border.lwd = 0.5)

# write out for Google Earth
st_write(occurrences_sf, "/Users/jennifercribbs/Documents/YOSE/Analysis/MultipleDisturbances/outputSandbox/YOSEallTrees2023.kml", driver = "KML", delete_dsn = TRUE)
# write out for QGIS or ARCGIS
st_write(occurrences_sf, "/Users/jennifercribbs/Documents/YOSE/Analysis/MultipleDisturbances/outputSandbox/YOSEallTrees2023.shp", driver = "ESRI Shapefile", delete_dsn = TRUE)

