# Locate Individual Tree Canopies 
# 19 May 2025 
# Jenny Cribbs
# Based on tutorial: https://tgoodbody.github.io/lidRtutorial/

# Clear environment
rm(list = ls(globalenv()))

# Load packages
library(lidR)
library(sf)
library(terra)
library(tidyverse)

# set working directory if necessary 
setwd("/Users/jennifercribbs/Documents/R-Projects/MultipleDisturbances")

# Read in all tree data from YOSE
trees <- read.csv("dataSandbox/SpatialData/YOSE_treeOccurrence.csv")

summary(trees)

# remove blank rows at the end, plus 3 plots with no trees
# Also removing E72-PILA29 due to no coordinates
# Also plot E60 trees 18-27 were estimated in rough blocks, so no coordinates
trees <- trees %>% dplyr::filter(!is.na(verbatimLatitude))

# Create spatial object
trees_sf <- st_as_sf(trees, coords = c("verbatimLongitude", "verbatimLatitude"), crs = 4326)
# Write out to .kml and .shp files
#st_write(trees_sf, "outputSandbox/YOSE_trees.kml", driver = "KML", delete_dsn = T)
#st_write(trees_sf, "outputSandbox/YOSE_trees.shp", driver = "ESRI Shapefile", delete_dsn = T)

# filter trees for Hooper Peak Plot only
trees_sf_YPE75 <- filter(trees_sf, eventID == 75)

# Read in LiDAR file for plot 75 Hooper Peak and set some color palettes
las_raw <- readLAS("dataSandbox/SpatialData/Lidar/USGS_LPC_CA_YosemiteNP_2019_D19_11SKB5385.laz",  filter = "-set_withheld_flag 0")
col <- height.colors(50)
col1 <- pastel.colors(900)

# generate digital terrain model
dtm <- rasterize_terrain(filter_poi(las, Classification == 2), res = 0.5, algorithm = knnidw())
plot(dtm)

# normalize the lidar results with the surface height
las_norm <- normalize_height(las, dtm)

# define the canopy by filtering out ground (class 2) [and noise (>7)]
las_canopy <- filter_poi(las_norm, Classification != 2)

# Generate CHM
chm <- rasterize_canopy(las_canopy, res = 0.5, algorithm = p2r(0.15))
plot(chm, col = col)

# Detect trees
ttops <- locate_trees(las_canopy, algorithm = lmf(ws = 5))
print(ttops)
# plot tree tops
plot(chm, col = col, main = "Tree Tops on Canopy Height Model")
#plot(terra::vect(ttops), col = "black", add = TRUE, cex = 0.5)
# Pextract coordinates (X, Y, Z)
coords <- sf::st_coordinates(ttops)

# Plot tree tops
x <- coords[,1]
y <- coords[,2]
points(x, y, col = "white", cex = 0.05)

# Label with tree height (from ttops$Z, the non-geometry column)
#text(coords[,1], coords[,2], labels = round(ttops$Z, 1), pos = 3, cex = 0.3, col = "black")

# segment trees using dalponte
las_seg <- segment_trees(las = chm, algorithm = dalponte2016(chm, treetops = ttops))

# Count number of trees detected and segmented
length(unique(las$treeID) |> na.omit())
#> [1] 0 --> not working

plot(las_seg)

# Tree Top Detection without a CHM
# Detect trees
ttops2 <- locate_trees(las = las_canopy, algorithm = lmf(ws = 3, hmin = 5))
# Visualize
x <- plot(las_canopy)
add_treetops3d(x = x, ttops = ttops2, radius = 0.5)
plot(x)
