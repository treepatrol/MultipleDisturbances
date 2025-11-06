# Locate Individual Tree Canopies 
# 19 May 2025 
# Jenny Cribbs
# Based on tutorial: https://tgoodbody.github.io/lidRtutorial/06_its.html

# Clear environment
rm(list = ls(globalenv()))

# Load packages
library(lidR)
library(sf)
library(terra)
library(tidyverse)

# Read in all tree data from YOSE
trees <- read.csv("/Users/jennifercribbs/Documents/R-Projects/MultipleDisturbances/Scripts/Spatial/YOSE_treeOccurrence.csv")

summary(trees)

# remove blank rows at the end, plus 3 plots with no trees
# Also removing E72-PILA29 due to no coordinates
# Also plot E60 trees 18-27 were estimated in rough blocks, so no coordinates
trees <- trees %>% dplyr::filter(!is.na(verbatimLatitude))

# Create spatial object
trees_sf <- st_as_sf(trees, coords = c("verbatimLatitude", "verbatimLongitude"), crs = 4326)
# Write out to .kml and .shp files
st_write(plotssf, "outputSandbox/YPEPlots.kml", driver = "KML", delete_dsn = T)
st_write(plotssf, "outputSandbox/YPEPlots.shp", driver = "ESRI Shapefile", delete_dsn = T)

# Read in LiDAR file and set some color palettes
las <- readLAS("data/MixedEucaNat_normalized.laz",  filter = "-set_withheld_flag 0")
col <- height.colors(50)
col1 <- pastel.colors(900)