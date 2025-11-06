# Spatial Data Wrangling 
# 01 January 2025
# Author: Jenny Cribbs

# Inputs: plot data from the event ID script, tree data and plot data read in from the Google Sheets

# Code Description: (1) spatial data wrangling to use plot beginning waypoints in utms to calculate plot end waypoints in utms based on azimuth and transect length in meters (2) visualize points (3) choose calculated or gps waypoint to get one end point per plot (4) visualize result (5) convert all points to decimal degrees. (6) calculate utms for associated trees based on dOut and dSide (7) calculate utms for pila based on dOut and dSide (8) map all trees and pila (from gps and xy)
# Output: (1) a csv file with lat/long coordinates for each plot beginning and end point. (2) a csv file with lat/long coordinates for each tree.

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
setwd("/Users/jennifercribbs/Documents/YOSE/Analysis/MultipleDisturbances/")
#setwd("/Users/tazli/Downloads/YOSE_SugarPine/MultipleDisturbances")

# load user defined functions relativeTreeCalculation and calculateTreePositions by running 0_TreePositionCalculationFunctions.R first
source("Scripts/UnderstoryCleaning/0_TreePositionCalculationFunctions.R")

# ~~~~~~~~~~~~~~~~~~~~ PILAs ~~~~~~~~~~~~~~~~~~~~
# Bring in the PILA data for each plot in the project folder from Google Sheets
# setting the directory for data extraction--change to your local data directory and make sure it has changes from 2/3/2025
datadir <- "/Users/jennifercribbs/Documents/YOSE/Analysis/MultipleDisturbances/Data/RawData/YPE_Data"

#datadir <- "/Users/tazli/Downloads/YOSE_SugarPine/MultipleDisturbances/Data/RawData/YPE_Data"
# provide path for files in datadir
folders <- list.dirs(datadir, full.names = TRUE)[-c(1,4)] # Ensure full path names are used

# initialize an empty dataframe 
pilas <- data.frame()

# loop through each folder corresponding to each plot
for (folder in folders) {
  # list all files in the folder
  files <- list.files(folder, pattern = "PILA", full.names = TRUE) # choose PILA data only
  
  # Loop through each "PILA" file in the folder
  for (file in files) {
    # Read the Excel file
    data <- read_excel(file, na = "NA")
    
    # Add an empty `dSide` column if it doesn't already exist
    if (!"dSide_m" %in% colnames(data)) {
      data$dSide_m <- NA_real_
      
      # Save the updated file back to its original location
      write_xlsx(data, file)
    }
    
    # Select relevant columns and format for merging
    xlsfile <- data %>%
      mutate_if(is.numeric, as.numeric) %>%  # Keep numeric columns as numeric
      dplyr::select(plotID, plot_type, date, crew, trans_length, width, 
                    slope, aspect, plot_azimuth,
                    plot_beg_UTM_N, plot_beg_UTM_E, waypoint_beg,
                    plot_end_UTM_N, plot_end_UTM_E, waypoint_end,
                    plot_notes, plot_elevation_ft, 
                    treeNum, species, 
                    DBH_cm, est_DBH_cm, height_m, est_height_m,
                    PILA_UTM_E, PILA_UTM_N, 
                    GPSdevice, PILA_waypoint, estimatedAccuracy_PILA_ft,
                    dOut_m, dSideR_m, dSideL_m, dSide_m,  # Include the `dSide` column here
                    est_dOut_m, est_dSideR, est_dSideL)
    
    # Append the cleaned data to the final dataframe
    pilas <- rbind(pilas, xlsfile)
  }
}

# create a unique occurrenceID (for PILAs) 
pilas <- pilas %>% 
  mutate(occurrenceID = paste(occurrenceID = paste("E", plotID, "-", "PILA", treeNum, sep = "")))

# remove row with no data except "bearing above" dSideR column
# pilas <- pilas %>% filter(!is.na(plotID))

# E75 is mostly GPS, 18 and 19 are xy, but 16 and 17 are missing all position information because of cryptic position notes down and towards trail is pretty close to south
# clean up estimated position values based on notes
# use relative tree calculation for plot 75 tree 19
relativeTreeCalculation(283229, 4201801, 28, 160) # 283333, 4201923
# use relative tree calculation for plot 48 tree 1
relativeTreeCalculation(0271604, 4182348, 174, 900.9) # 271502, 4181453
# Calculated relative tree position for plot 75 tree 19 in TreeMaps2.R
# impute data to replace notes with calculated utms
pilas <- pilas %>%
  mutate(
    dOut_m = ifelse(occurrenceID == "E75-PILA19", NA, dOut_m),
    dSideR_m = ifelse(occurrenceID == "E75-PILA19", NA, dSideR_m),
    PILA_UTM_E = ifelse(occurrenceID == "E75-PILA19", 283333, PILA_UTM_E),
    PILA_UTM_N = ifelse(occurrenceID == "E75-PILA19", 4201923, PILA_UTM_N)     
    )   

# E48-PILA1 has no position information--could use relative position for 900.9m out at 262 magnetic (270655.9, 4182507)
pilas <- pilas %>% mutate(
  PILA_UTM_E = ifelse(occurrenceID == "E48-PILA1", 270655.9, PILA_UTM_E),
  PILA_UTM_N = ifelse(occurrenceID == "E48-PILA1", 4182507, PILA_UTM_N))

# trees 16 and 17 have the following notes 
#[895] "30m down and towards trail from tree 10"
#[896] "25m up and 10m towards stand"  
# for tree 16: tree 10 (283260, 4201677) "30m down and towards trail"--> assuming near south bearing keep easting the same and subtract 30 from nothing (283260, 4201647) 
pilas <- pilas %>%
  mutate(
    dOut_m = ifelse(occurrenceID == "E75-PILA16", NA, dOut_m),
    dSideR_m = ifelse(occurrenceID == "E75-PILA16", NA, dSideR_m),
    PILA_UTM_E = ifelse(occurrenceID == "E75-PILA16", 283260, PILA_UTM_E),
    PILA_UTM_N = ifelse(occurrenceID == "E75-PILA16", 4201647, PILA_UTM_N)  ) 

# for tree 17: tree 10 (283260, 4201677)--> add 25 to northing, subtract 10 from easting because most trees are to the east of tree 10 (283285, 4201667)
pilas <- pilas %>%
  mutate(
    dOut_m = ifelse(occurrenceID == "E75-PILA17", NA, dOut_m),
    dSideR_m = ifelse(occurrenceID == "E75-PILA17", NA, dSideR_m),
    PILA_UTM_E = ifelse(occurrenceID == "E75-PILA17", 283285, PILA_UTM_E),
    PILA_UTM_N = ifelse(occurrenceID == "E75-PILA17", 4201667, PILA_UTM_N)  ) 

# change dOut_m to numeric (no NAs introduced)
pilas$dOut_m <- as.numeric(pilas$dOut_m)

# convert dSide Right to numeric (no NAs introduced)
pilas$dSideR_m <- as.numeric(pilas$dSideR_m)

# PILA_UTM_E is character type because of preserving the leading zeros in Excel
# coerce PILA_UTM_E to numeric (no NAs added)
pilas$PILA_UTM_E <- as.numeric(pilas$PILA_UTM_E)

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
# all three cases seem to be different trees at a similar point
#point00118 <- pilas %>% filter(PILA_waypoint == "00118") %>% select(occurrenceID, PILA_waypoint, DBH_cm, height_m) # gives relative position from a large ABCO snag, but would need to find this
#point00174 <- pilas %>% filter(PILA_waypoint == "00174") %>% select(occurrenceID, PILA_waypoint, DBH_cm, height_m) # could check full note
#point0262 <- pilas %>% filter(PILA_waypoint == "0262") %>% select(occurrenceID, PILA_waypoint, DBH_cm, height_m) # could check full note
#point00047 <- pilas %>% filter(PILA_waypoint == "00047") %>% select(occurrenceID, PILA_waypoint, DBH_cm, height_m) # could check full note
#point00060 <- pilas %>% filter(PILA_waypoint == "00060") %>% select(occurrenceID, PILA_waypoint, DBH_cm, height_m) # could check full note
# all seem to be trees that should share a waypoint

# join pila and kml data
pilas_kml <- left_join(pilas, kml_filtered, by = "kml_id")

# lots of NAs check the kml_ids
table(pilas$PILA_waypoint %in% kml_filtered$Name)
table(kml_filtered$Name %in% pilas$PILA_waypoint)

# Check for any non-printable characters--None
table(grepl("[[:cntrl:]]", pilas$PILA_waypoint))
table(grepl("[[:cntrl:]]", kml_filtered$Name))

## PILA Position Accounting
# E10-PILAs 2-6 have a dOut, but no dSide (measured or estimated)--> recorded correctly in the field, but dSideL was entered as positive, so the conversion adds NAs?
# E14-PILA1 has a dOut, but no dSide (measured or estimated)
# E15-PILAs 1-5, 7, 8, 13-16 (something wrong with dSide conversion? Need to convert dSideLs to negative)
# E-26 has UTMs, but also several dOuts without dSides--should ignore those, but check to see if dSide conversion went wrong
# E-26 last three trees 37, 38, 39 are missing dSides with no UTMs--entered correctly should have dOut, dSide
# E13-PILA 36 has a measured dSide (-39) and an estimated dOut (156)--> check this is fixed after estimated chunk
# E32-PILA 1,2 have missing dSide--this is one of a few cases where dSide_m was entered correctly into a dSide_m column with negatives and positives--need to pull in the this column, tree 3 had dOut entered in measured and estimated, but should be estimated
# E36-PILA 100 is missing dSide--no photo for this portion of the data, cannot review apparently missed
# E39 trees 1-31 have missing dSide--likely left entered without negative
# E43 1-3 have dout dside and UTMs--should use GPS UTMs ideally (WP entered)
# E43 PILA8 Waypoint 27 does not have UTMs entered, but ideally import from GPS
# E55 all PILAs 1-51 have dOut only
# E58 1-3 have both UTMs and dOut dSide--not sure which is more accurate, but definitly don't want duplicates
# E58-16 has relative position information that can be calculated WP29 + 10 and +8
# E59 all trees have no position information except PILA 7 (dout, dSide) and 12-19 (PILA_waypoint)--waypoint name only
# E6 27-33 have no position information--data sheet is clear for wp and UTMs, added waypoint number
# E60 trees 18-27 have estimated dOut only
# E61-PILA10 has dSide missing and conflicting estimated and measured dOut--should be 208.1 out and 10m dSideR
# E64-PILA1 has waypoint name and dOUt dside--go with waypoint
# E68-PILA48 has waypoint only
# E7, E8, and E29 have no PILA so NAs for position are real
# E73-PILA 30 has dOut in addition to waypoint name and UTMs--try using UTMs
# E73 PILAs 36 and 37 have estimated position information

# see unique values
unique(pilas_kml$est_dOut_m)  # all numeric except "WP29 + 10m"
unique(pilas_kml$est_dSideL) # all numeric except "WP29+8m"

# coerce estimated columns to numeric 
pilas_kml <- pilas_kml %>% mutate(
  est_dOut_m = as.numeric(est_dOut_m),
  est_dSideL = as.numeric(est_dSideL)
) 

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

# check if 'dSide' exists and drop it if necessary
#if ("dSide" %in% colnames(pilas_kml)) {
  #pilas_kml <- pilas_kml %>% select(-dSide) # Drop old version
#}
# rename dSide_final to match function
#pilas_kml_final <- pilas_kml %>%
  #rename(dSide = dSide_final)

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

# E13-PILA 36 has a measured dSide (-39) and an estimated dOut (156)--> check this is fixed after estimated chunk--> resolved
# E30 PILAs 7-12 were all estimated for dOut and dSide--> resolved
# E31 PILAs 1-7 have a plot note estimated with trig, so entered as estimated--> resolved
# E36 PILA 100 has dOut, but dSide missed in the field--adjacent trees seem close, may be able to impute 
# E58 PILA 16 had +10 and +8 relative to WP29, so imputation didn't work--still not working
# E6 PILA 27-33 all have waypoints with a note past the - end (waypoints 319-324)
# E60 PILA 18-27--> UCSB plot check Google Sheet--estimated dOuts, but no sides entered
# E7 no PILA
# E72 PILA 29 dSide missed in the field (good chance the dSide is between 3m and 0 because negative/left side is lines out and the prior tree was 2.9)
# E73 PILA 36 and 37 all measurements including position estimated after the fact--> resolved as estimates
# E8 no PILA
# Conclusion is many estimated positions are not making it through--> resolved
# Coalesce does not seem to be prioritizing the XY straight from the GPS and does not even take some positions when no other option is available--> not sure about prioritization, but at least filling in waypoint names only with coordinates
# 16 missing now, 3 are plots with no PILA
# plot 60 has 10 PILA with estimated dOut and no estimated dSide
# E58 PILA16 imputation is not working
# 2 have dSide missed in the field--imputation sketchy, but maybe workable as an estimate

## ~~~~~~~~~~~~~~~~~~~~ Associated Trees ~~~~~~~~~~~~~~~~~~~~

# read in clean plot data
plotData <- read.csv("/Users/jennifercribbs/Documents/YOSE/Analysis/MultipleDisturbances/dataSandbox/CleanData/PlotLevelData.csv")

# provide path for files in datadir
folders <- list.dirs(datadir, full.names = TRUE)[-c(1,4)] # Ensure full path names are used

# initialize an empty list to store data for each plot
trees <- data.frame()

# loop through each folder
for (folder in folders) {
  # list all files in the folder
  files <- list.files(folder, pattern = "Tree") # choose Tree data only
  
  # Loop through each file
  for(file in files) {
    # Read the Excel file
    xlsfile <- read_excel(paste0(folder,"/",file), na = "NA") %>%
      mutate_if(is.numeric, as.numeric) %>% # keeps numbers numeric
      dplyr::select(plot, date, crew,
                    treeNum, species, DBH_cm, height_m,
                    dOut_m, dSideR_m, dSideL_m,
                    percentLive, damageCodes, notes
      )
    trees <- rbind(trees, xlsfile)
    
  }
}

# check data types
summary(trees)

# Part4: Associated Tree Data Wrangling -------------------------

# fix plotID naming and make dOut consistent with function
trees <- trees %>% 
  rename(plotID = plot,
         dOut_final = dOut_m) # plotID entered as plot for Trees

# convert dSide Left to numeric 
trees$dSideL_m <- as.numeric(trees$dSideL_m) # likely dash entry error

# combine dSideR_m and dSideL_m into a single dSide_final column with checks
trees <- trees %>%
  mutate(dSide_final = case_when(
    !is.na(dSideR_m) & dSideR_m >= 0 ~ dSideR_m,
    !is.na(dSideL_m) & dSideL_m <= 0 ~ dSideL_m,
    TRUE ~ NA_real_
  ))
summary(trees)

#correct misspellings and inconsistencies in tree_list
trees <- trees %>%
  mutate(species = case_when(
    species == "PYGE" ~ "PIJE",
    species %in% c("unknown", "UNK", "UNKNOWN", "Charcol", "Unknown") ~ "Pinales",
    TRUE ~ species
  ))

# check species field again
unique(trees$species)

#remove SALIX and CONU because they should be shrubs 
# although some are tree-like, these species were likely not assessed as trees uniformly across the plots
trees <- trees %>% 
  filter(species != "CONU") %>% 
  filter(species != "SALIX") # removes only 2 records total (3309 to 3307)

# join tree data with plotData
treeData <- left_join(trees, select(plotData, -date, -crew), by = "plotID")
# create a unique identifier
treeData <- treeData %>% mutate(occurrenceID = paste("E", plotID, "-", "Tree", treeNum, sep = ""))

# plot 74 tree 13--appears to have no position because dOut is NA
# tree_positions <- tree_positions %>% filter(!is.na(tree_UTM_E) & !is.na(tree_UTM_N))
# dOut was missed in the field, but neighboring trees are 1m apart (48.2 and 49.2 dOut), so a dOut estimate of 48.7 seems more reasonable than deleting the full record
# impute 48.7 as best guess for the missing dOut value
treeData <- treeData %>% 
  mutate(dOut_final = if_else(occurrenceID == "E74-Tree13", 48.7, dOut_final))

# calculate tree positions
tree_positions <- calculate_tree_positions(treeData)

# check for missing positions--none after imputation
missing_positions <- tree_positions %>%
  filter(is.na(tree_UTM_E) & is.na(tree_UTM_N))
nrow(missing_positions)

# create spatial points for trees and convert to lat/long (WGS84)
tree_points <- tree_positions %>%
  group_split(UTM_zone) %>%
  map_dfr(function(df) {
    st_as_sf(df, 
             coords = c("tree_UTM_N", "tree_UTM_E"), 
             crs = paste0("+proj=utm +zone=", unique(df$UTM_zone), " +datum=NAD83")) %>%
      st_transform(crs = 4326)  # Convert to lat/long
  })
    
# extract the latitude and longitude coordinates
    tree_points <- tree_points %>%
      mutate(
        verbatimLatitude = st_coordinates(tree_points)[, 2],  # extract latitude
        verbatimLongitude = st_coordinates(tree_points)[, 1]  # extract longitude
      )

# trim columns before binding with pilas
tree_points_trimmed <- tree_points %>% select (occurrenceID, verbatimLatitude, verbatimLongitude) %>% st_drop_geometry()

# bind associated trees with pilas
occurrence_positions <- rbind(gbif_pilas, tree_points_trimmed)

# check for missing positions--still 14 from the PILAs
missing_positions <- occurrence_positions %>%
  filter(is.na(verbatimLatitude) & is.na(verbatimLongitude))
nrow(missing_positions)

# write out CSV with latitude and longitude for each tree (except 89 problem trees)
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


  
  


  
  
            