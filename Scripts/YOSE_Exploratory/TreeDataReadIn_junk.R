# Set the working directory
setwd("/Users/jennifercribbs/Documents/YOSE/Analysis/YPE_Data")

# Load initial packages
library(tidyverse)
library(sp)

## reading in all data
# lists all files in the working directory
files <- list.files()
# assigns the YPE_Data folder to the object outdir
outdir <- "/Users/jennifercribbs/Documents/YOSE/Analysis/YPE_Data"
# creating a directory path for each plot
folders <- list.dirs(outdir)[-c(1,4)]
# creates a blank data frame
plot_list <- data.frame()
# for loop to read in PILA data to plot_location
for (folder in folders) {
  newfiles = list.files(folder, pattern = "PILAdata")
  for(file in newfiles) {
    xlsfile <- read_excel(paste0(folder,"/",file)) %>% 
      dplyr::select(plotID, plot_beg_UTM_E, plot_beg_UTM_N, estimatedAccuracy_beg_ft, plot_type)
    plot_list <- rbind(plot_list, xlsfile)
  }
}

# Use distinct to keep only unique combinations of Plot_ID and coordinates
unique_plot_coords <- plot_list %>%
  distinct(plotID, plot_beg_UTM_E, plot_beg_UTM_N, .keep_all = TRUE)

# remove entries where coordinates are NA
plot_coords <- unique_plot_coords %>%  filter(plot_beg_UTM_E != "NA" & plot_beg_UTM_N != "NA")

# change data type to numeric 
plot_coords$plot_beg_UTM_E <- as.numeric(plot_coords$plot_beg_UTM_E) 
plot_coords$plot_beg_UTM_N <- as.numeric(plot_coords$plot_beg_UTM_N)

# write out data 
write_csv(plot_coords, "PlotUTMs.csv")

# Convert UTM coordinates to SpatialPoints object
coordinates(plot_coords) <- c("plot_beg_UTM_E", "plot_beg_UTM_N")

# Define the UTM zone (e.g., zone 11 for Yosemite)
utm_zone <- 11

# Define the CRS (Coordinate Reference System) for UTM
utm_crs <- CRS(paste0("+proj=utm +zone=", utm_zone, " +datum=WGS84"))

# Project the coordinates to latitude and longitude
data_latlong <- spTransform(plot_coords, CRS("+proj=longlat +datum=WGS84"))

# Extract the latitude and longitude from the SpatialPoints object
latitude <- coordinates(data_latlong)[, 2]
longitude <- coordinates(data_latlong)[, 1]

# Add latitude and longitude to your data frame
data$Latitude <- latitude
data$Longitude <- longitude







