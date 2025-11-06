# Set the working directory
#setwd("/Users/jennifercribbs/Documents/YOSE/Analysis/MultipleDisburbances")

# Load initial packages
library(tidyverse)
library(sp)
library(readxl)

## reading in all data

# assigns the YPE_Data folder to the object outdir
outdir <- "/Users/jennifercribbs/Documents/YOSE/Analysis/MultipleDisturbances/Data/RawData/YPE_Data"
# lists all files in outdir
files <- list.files(outdir)
# creating a directory path for each plot
folders <- list.dirs(outdir)[-c(1,4)]
# creates a blank data frame
plot_list <- data.frame()
# for loop to read in PILA data to plot_location
for (folder in folders) {
  files = list.files(folder, pattern = "PILAdata")
  for(file in files) {
    xlsfile <- read_excel(paste0(folder,"/",file)) %>% 
      dplyr::select(plotID, plot_beg_UTM_E, plot_beg_UTM_N, estimatedAccuracy_beg_ft, plot_type)
    plot_list <- rbind(plot_list, xlsfile)
  }
}

# Use distinct to keep only unique combinations of Plot_ID and coordinates
unique_plot_coords <- plot_list %>%
  distinct(plotID, plot_beg_UTM_E, plot_beg_UTM_N, .keep_all = TRUE)

# remove entries where coordinates are NA, arrange by plotID
plot_coords <- unique_plot_coords %>%  
  filter(plot_beg_UTM_E != "NA" & plot_beg_UTM_N != "NA") %>% 
  arrange(plotID)

# change data type to numeric--maybe not to preserve leading 0s
# plot_coords$plot_beg_UTM_E <- as.numeric(plot_coords$plot_beg_UTM_E) 
# plot_coords$plot_beg_UTM_N <- as.numeric(plot_coords$plot_beg_UTM_N)


# Convert UTM coordinates to SpatialPoints object
coordinates(plot_coords) <- c("plot_beg_UTM_E", "plot_beg_UTM_N")

# Create a column for UTM zone (zone 11 for most of Yosemite, but zone 10 for plots 32 and 50 in Stanislaus NF)
plot_coords$utm_zone <- ifelse(plot_coords$plotID %in% c(32, 50), 10, 11 )
# Create a column for dataum
plot_coords <- plot_coords %>% mutate(datum = "NAD83") %>% select(-plot_type, -estimatedAccuracy_beg_ft)

# write out data 
write_csv(plot_coords, "Plot_beg_UTMs_66sr22.csv")


# Not working -------------------------------------------------------------


# Below was generating errors
# Define the CRS (Coordinate Reference System) for UTM
utm_crs <- CRS(paste0("+proj=utm +zone=", plot_coords$utm_zone, " +datum=NAD83"))

# Getting error
# Error in st_transform.sfc(st_geometry(x), crs, ...) : 
# cannot transform sfc object with missing crs
# Project the coordinates to latitude and longitude
data_latlong <- spTransform(plot_coords, utm_crs)

# Extract the latitude and longitude from the SpatialPoints object
latitude <- coordinates(data_latlong)[, 2]
longitude <- coordinates(data_latlong)[, 1]

# Add latitude and longitude to your data frame
data$Latitude <- latitude
data$Longitude <- longitude


