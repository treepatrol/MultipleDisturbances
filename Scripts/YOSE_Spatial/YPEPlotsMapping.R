# YPE Plots Mapping -------------------------------------------------------

# Author: Tazlina Dentinger
# Date: 02 May 2025

# Input: SpeciesInventoryDeliverable_PILAproject_EventTab_20230326.csv
  # csv file with only the event tab (plots) of the most recent version of the clean data submitted to GBIF, which includes Lat/Long location data

# Code description: Takes the Lat/Long columns from the GBIF Data and produces spatial files with the beginning point of each plot

# Output: YPEPlots.kml and YPEPlots.shp, a KML and shapefile of the beginning points of each plot.

# -------------------------------------------------------------------------
library(tidyverse)
library(terra)
#read in csv and select relevant plot name and location columns
plots <- read_csv("dataSandbox/SpeciesInventoryDeliverable_PILAproject_EventTab_20230326.csv") %>% 
  select(eventID, verbatimLatitude, verbatimLongitude) %>% filter(!is.na(eventID))
#create spatial object
plotssf <- st_as_sf(plots, coords = c("verbatimLatitude", "verbatimLongitude"), crs = 4326)
#write out to .kml and .shp files
st_write(plotssf, "outputSandbox/YPEPlots.kml", driver = "KML", delete_dsn = T)
st_write(plotssf, "outputSandbox/YPEPlots.shp", driver = "ESRI Shapefile", delete_dsn = T)
