# Tree Map 
# 30 January 2025
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
# load user defined functions relativeTreeCalculation and calculateTreePositions by running 0_TreePositionCalculationFunctions.R first

# Set the working directory
setwd("/Users/jennifercribbs/Documents/YOSE/Analysis/MultipleDisturbances/")
#setwd("/Users/tazli/Downloads/YOSE_SugarPine/MultipleDisturbances")

# ~~~~~~~~~~~~~~~~~~~~ PILAs ~~~~~~~~~~~~~~~~~~~~
# Bring in the PILA data for each plot in the project folder from Google Sheets
# setting the directory for data extraction--change to your local data directory
datadir <- "/Users/jennifercribbs/Documents/YOSE/Analysis/MultipleDisturbances/dataSandbox/RawData/YPE_Data"

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
pilas <- pilas %>% filter(!is.na(plotID))

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

# E48-PILA1 has no position information--could use relative position for 900.9m out at 262 magnetic (271502, 4181453)
pilas <- pilas %>% mutate(
  PILA_UTM_E = ifelse(occurrenceID == "E48-PILA1", 271502),
  PILA_UTM_N = ifelse(occurrenceID == "E48-PILA1", 4181453))

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

summary(pilas)
# Why is PILA_UTM_E character?
# Probably because of preserving the leading zeros in Excel
# Coerce PILA_UTM_E to numeric (no NAs added)
pilas$PILA_UTM_E <- as.numeric(pilas$PILA_UTM_E)

# bring in kml files from gps units
kml_66i <- st_read("/Users/jennifercribbs/Documents/YOSE/Waypoints/Recently Read from GPSMAP 66i (Unit ID 3404379582).kml") %>% 
  mutate(Source = "66i",
         kml_id = paste(Source, Name))
kml_66sr <- st_read("/Users/jennifercribbs/Documents/YOSE/Waypoints/Recently Read from GPSMAP 66sr (Unit ID 3377332670).kml") %>% 
  mutate(Source = "66sr", 
         kml_id = paste(Source, Name))
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
si_check <- si %>% select(occurrenceID, GPSdevice, dOut_m, dSide, PILA_waypoint, PILA_UTM_E, PILA_UTM_N, crew, date, plotID)
# 30 points were taken on the inreach that was returned to UCD
# fortunately they are entered as utms, so hopefully no errors 
# we will not be able increase precision by using the device directly, but shouldn't be a big deal
inreach <- filter(pilas, GPSdevice == "inreach")

# normalize waypoint names to ensure they have the same length
# Remove any rows where the waypoint name contains letters

# this keeps only numeric waypoint names (570 of 639)
# this should exclude points from other projects
kml_filtered <- kml[!grepl("[a-zA-Z]", kml$Name), ]

# Check rows with NAs prior to digit normalization (1490)
sum(is.na(pilas$PILA_waypoint))
# Check rows with GPSdevice == "66i" and valid PILA_waypoint (187)
sum(!is.na(pilas$PILA_waypoint) & pilas$GPSdevice == "66i")
# Check rows with GPSdevice == "66sr" and valid PILA_waypoint (56)
sum(!is.na(pilas$PILA_waypoint) & pilas$GPSdevice == "66sr")

# Replace entries that are empty, all whitespace, or variations of "NA" with true NA
pilas$PILA_waypoint <- ifelse(grepl("^\\s*NA\\s*$|^\\s*$", 
                                    pilas$PILA_waypoint, ignore.case = TRUE), 
                              NA, pilas$PILA_waypoint)
# check that cleaning worked
# Verify no problematic strings remain
any(grepl("^\\s*NA\\s*$|^\\s*$", pilas$PILA_waypoint, ignore.case = TRUE))  # Should return FALSE

# normalize data entry of PILA waypoint names based on the device source
# for 66i: ensure 5 digits
pilas$PILA_waypoint[!is.na(pilas$PILA_waypoint) & pilas$GPSdevice == "66i"] <- 
  formatC(as.numeric(pilas$PILA_waypoint[!is.na(pilas$PILA_waypoint) & pilas$GPSdevice == "66i"]), width = 5, flag = "0")
# for 66sr: ensure 4 digits
pilas$PILA_waypoint[!is.na(pilas$PILA_waypoint) & pilas$GPSdevice == "66sr"] <- 
  formatC(as.numeric(pilas$PILA_waypoint[!is.na(pilas$PILA_waypoint) & pilas$GPSdevice == "66sr"]), width = 4, flag = "0")

# investigae duplicate waypoints
non_na_duplicates <- pilas$PILA_waypoint[duplicated(pilas$PILA_waypoint) & !is.na(pilas$PILA_waypoint)]
unique(non_na_duplicates)
# all three cases seem to be different trees at a similar point
point00118 <- pilas %>% filter(PILA_waypoint == "00118") %>% select(occurrenceID, PILA_waypoint, DBH_cm, height_m, notes) # gives relative position from a large ABCO snag, but would need to find this
point00174 <- pilas %>% filter(PILA_waypoint == "00174") %>% select(occurrenceID, PILA_waypoint, DBH_cm, height_m, notes) # could check full note
point0262 <- pilas %>% filter(PILA_waypoint == "0262") %>% select(occurrenceID, PILA_waypoint, DBH_cm, height_m, notes) # could check full note
point00047 <- pilas %>% filter(PILA_waypoint == "00047") %>% select(occurrenceID, PILA_waypoint, DBH_cm, height_m, notes) # could check full note
point00060 <- pilas %>% filter(PILA_waypoint == "00060") %>% select(occurrenceID, PILA_waypoint, DBH_cm, height_m, notes) # could check full note
# all seem to be trees that should share a waypoint

# join pila and kml data
pilas_kml <- left_join(pilas, kml_filtered, by = c("PILA_waypoint" = "Name")) 
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

# fix incorrect data entry for positive dSideL values (manually reviewed to ensure this was the problem)
pilas_kml <- pilas_kml %>%
  mutate(dSideL_m = if_else(!is.na(dSideL_m) & dSideL_m > 0, -dSideL_m, dSideL_m))
# combine right and left sides
pilas_kml <- pilas_kml %>%
  mutate(dSide_combined = case_when(
    !is.na(dSideR_m) & dSideR_m >= 0 ~ dSideR_m,
    !is.na(dSideL_m) & dSideL_m <= 0 ~ dSideL_m,
    TRUE ~ NA_real_
  ))
summary(pilas_kml)

# coerce estimated columns to numeric (NAs introduced from notes relative to WP29--corrected in next line)
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
# rename dSide_final to match function
pilas_kml <- pilas_kml %>% rename(dSide = dSide_final)
# plot UTM columns need to be numeric 
pilas_kml$plot_beg_UTM_E <- as.numeric(pilas_kml$plot_beg_UTM_E)
pilas_kml$plot_beg_UTM_N <- as.numeric(pilas_kml$plot_beg_UTM_N)

# run calculate positions function to convert dOut and dSide to UTMs
pila_positions <- calculate_tree_positions(pilas_kml)

# consolidate all position information and convert to decimal degrees

# ~~~~~~~~~~~~~~~~~~~~ Associated Trees ~~~~~~~~~~~~~~~~~~~~

# join tree data with plotData
treeData <- left_join(treeData, select(plotData, -date, -crew), by = "plotID")
# need to check plot 74 tree 13--appears to have no position because dOut is NA
#tree_positions <- tree_positions %>% filter(!is.na(tree_UTM_E) & !is.na(tree_UTM_N))
# dOut was missed in the field, but neighboring trees are 1m apart (48.2 and 49.2 dOut), so a dOut estimate of 48.7 seems more reasonable than deleting the full record
# impute 48.7 as best guess for the missing dOut value
treeData <- treeData %>% 
  mutate(dOut_m = if_else(plotID == 74 & treeNum == 13, 48.7, dOut_m))

# Calculate tree positions
tree_positions <- calculate_tree_positions(treeData)

# Create spatial points for trees and convert to lat/long (WGS84)
tree_points <- tree_positions %>%
  group_split(UTM_zone) %>%
  map_dfr(function(df) {
    st_as_sf(df, 
             coords = c("tree_UTM_E", "tree_UTM_N"), 
             crs = paste0("+proj=utm +zone=", unique(df$UTM_zone), " +datum=NAD83")) %>%
      st_transform(crs = 4326)  # Convert to lat/long
  })

# find non-numeric dOut values
# 271 NAs + 3 trees in plot 75 with relative positions
# NAs include pilas with estimated position and gps position
pilaData %>%
  filter(!grepl("^-?[0-9.]+$", dOut_m)) %>%
  select(occurrenceID, dOut_m) %>%
  distinct() %>%
  print()

# Change dOut_m to numeric
pilaData$dOut_m <- as.numeric(pilaData$dOut_m)

# combine dSideR_m and dSideL_m into a single dSide column with checks
pilaData <- pilaData %>%
  mutate(dSide = case_when(
    !is.na(dSideR_m) & dSideR_m >= 0 ~ dSideR_m,
    !is.na(dSideL_m) & dSideL_m <= 0 ~ dSideL_m,
    TRUE ~ NA_real_
  ))
summary(pilaData)