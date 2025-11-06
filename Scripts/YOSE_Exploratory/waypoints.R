# YOSE Plot Data Read In for Waypoint Wrangling -------------------------
# Authors: Jenny Cribbs, Andrew Latimer, Joan Dudney
# Date: 05 June 2024

# Inputs: Raw data from the YOSE PILA Google sheets 

# Code description: This code chunk reads in PILA data and pares it down to plot-level data only

# Output: Plot-level data to join with kml files. 

# Load initial packages
library(readxl)
library(tidyverse)
library(sf)

# Set the working directory if not done in step 1
setwd("/Users/jennifercribbs/Documents/YOSE/Analysis/MultipleDisturbances/")

# bring in points from GPS 66i
points66i <- st_read("/Users/jennifercribbs/Documents/YOSE/Waypoints/Recently Read from GPSMAP 66i (Unit ID 3404379582).kml")
# bring in points from GPS 66sr
points66sr <- st_read("/Users/jennifercribbs/Documents/YOSE/Waypoints/Recently Read from GPSMAP 66sr (Unit ID 3377332670).kml")
# bind waypoints from both units into one object
points2024 <- rbind(points66i, points66sr)

# setting the directory for data extraction
datadir <- "/Users/jennifercribbs/Documents/YOSE/Analysis/MultipleDisturbances/Data/RawData/YPE_Data"
# list all file names in the data directory
files <- list.files(datadir)
# provide path for files in datadir
folders <- list.dirs(datadir)[-c(1,4)]
# create a blank dataframe 
tree_list <- data.frame()
# bring in selected variables from excel files in datadir
for (folder in folders) {
  newfiles = list.files(folder, pattern = "PILAdata") # PILA data only
  for(file in newfiles) {
    xlsfile <- read_excel(paste0(folder,"/",file)) %>% 
      dplyr::select(plotID, GPSdevice, waypoint_beg, plot_type, trans_length, width, slope, aspect, plot_type,
                    fireseverity_50m, fireseverity_100m, fireseverity_150m, fireseverity_200m, 
                    ribes_50m, ribes_100m, ribes_150m, ribes_200m, 
                    DBH_cm, height_m, 
                    pitchTubes, exitHoles, 
                    activeBranchCanker, inactiveBranchCanker, 
                    activeBoleCanker, inactiveBoleCanker, 
                    DTOP, flags, percentLive, notes, plot_notes)
    tree_list <- rbind(tree_list, xlsfile)
  }
}

# change data type to numeric for appropriate vars
tree_list$trans_length <- as.numeric(tree_list$trans_length)
tree_list$width <- as.numeric(tree_list$width)
tree_list$slope <- as.numeric(tree_list$slope)
tree_list$aspect <- as.numeric(tree_list$aspect)
tree_list$DBH_cm <- as.numeric(tree_list$DBH_cm)
tree_list$height_m <- as.numeric(tree_list$height_m)
tree_list$flags <- as.numeric(tree_list$flags)
tree_list$percentLive <- as.numeric(tree_list$percentLive)
tree_list$activeBoleCanker <- as.numeric(tree_list$activeBoleCanker) 
tree_list$inactiveBoleCanker <- as.numeric(tree_list$inactiveBoleCanker)
tree_list$activeBranchCanker <- as.numeric(tree_list$activeBranchCanker)
tree_list$inactiveBranchCanker <- as.numeric(tree_list$inactiveBranchCanker)

# Load the packages
library(dplyr)
library(stringr)

# Ensure Name and waypoint_beg columns are character type
points2024$Name <- as.character(points2024$Name)
tree_list$waypoint_beg <- as.character(tree_list$waypoint_beg)

# Function to add leading zeros based on device type
add_leading_zeros <- function(name, device) {
  if (is.na(device)) {
    return(name)
  } else if (device == "66i") {
    return(str_pad(name, width = 5, side = "left", pad = "0"))
  } else if (device == "66sr") {
    return(str_pad(name, width = 4, side = "left", pad = "0"))
  } else {
    return(name)
  }
}

# Apply the function to the gps_data data frame
tree_list <- tree_list %>%
  mutate(Name = mapply(add_leading_zeros, waypoint_beg, GPSdevice))

# Perform a left join on the gps_data and waypoints data frames
merged_point_data <- left_join(tree_list, points2024) %>% 
  select(plotID, GPSdevice, waypoint_beg, plot_type, trans_length, width, slope, aspect, fireseverity_50m, ribes_50m, Name, plot_notes, geometry)

# Reduce to one point per plot
unique_plots <- merged_point_data %>%
  distinct(plotID, .keep_all = TRUE)

# View the cleaned data
print(unique_plots)

# remove row of NAs
unique_plots <- unique_plots %>% filter(!is.na(plotID))

# write a spatial file--can't write with empty geometry
st_write(unique_plots, "/Users/jennifercribbs/Documents/YOSE/Analysis/MultipleDisturbances/Data/CleanData/UniquePlots")


# to separate plot data from PILA data
# PILA tree-level
select(treeNum, species, dOut_m, dSideR_m, dSideL_m, est_dOut_m, est_dSideR, est_dSideL, PILA_waypoint, PILA_UTM_E, PILA_UTM_N, estimatedAccuracy_PILA_f, DBH_cm, est_DBH_cm, height_m, est_height_m, pitchTubes, exitHoles, activeBranchCanker, inactiveBranchCanker, activeBoleCanker, inactiveBoleCanker, flags, DTOP, percentLive, resistance, damageCodes, fire_scar, notes)