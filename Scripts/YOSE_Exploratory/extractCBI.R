# Script to extract continuous fire severity metrics. 
# It loads plot gps locations as well as the Rim and Ferguson fire severity rasters, reprojects tht plot locations, then extracts the severity data. 

library(sf)
library(tidyverse)
library(terra)


# Load fire severity rasters from Mike Koontz (metric is predicted CBI)
rim_sev <- rast("./Data/Rim_2013-CASTF-002857_CBI_bc_v1.0_2013_extended.tif")
st_crs(rim_sev)
ferguson_sev <- rast("./Data/FERGUSON_2018-CASNF-000745_CBI_bc_v1.0_2018_extended.tif")
plot(ferguson_sev)

# merge fire rasters 
fire_sev <- merge(rim_sev, ferguson_sev)
plot(fire_sev) 

# Load fire severity rasters from MTBS (metric is dnbr)
rim_mtbs <- rast("./Data/ca3765211988120180713_20180705_20190708_dnbr.tif")
ferguson_mtbs <- rast("./Data/ca3785712008620130817_20130714_20140701_dnbr.tif")
fire_sev_mtbs <- merge(rim_mtbs, ferguson_mtbs)
fire_sev_mtbs[fire_sev_mtbs < 0] <- NA
plot(fire_sev_mtbs)

# reproject Mike's fire raster into MTBS raster projection 
fire_sev <- project(fire_sev, fire_sev_mtbs) 
plot(fire_sev)
plot(fire_sev_mtbs, add=T)

# Load the point data
yose_pts <- read.csv("./Data/Plot_beg_UTMs_66sr22.csv", header = TRUE) 
head(yose_pts) 
yose_pts_11 <- dplyr::filter(yose_pts, utm_zone == 11) |>
  st_as_sf(coords = c("plot_beg_UTM_E", "plot_beg_UTM_N"), crs = 26911)
yose_pts_10 <- dplyr::filter(yose_pts, utm_zone == 10) |>
  st_as_sf(coords = c("plot_beg_UTM_E", "plot_beg_UTM_N"), crs = 26910)

# reproject points to raster crs
yose_pts_11 <- st_transform(yose_pts_11, crs = crs(rim_mtbs))
yose_pts_10 <- st_transform(yose_pts_10, crs = crs(rim_mtbs))
yose_pts_sf <- rbind(yose_pts_11, yose_pts_10)

# Plot the data to check 
plot(fire_sev_mtbs)
points(yose_pts_sf, pch = 19, col = "red")

# Extract the values from the CBI raster
pred_CBI <- extract(fire_sev, yose_pts_sf, method = "bilinear")
dnbr <- extract(fire_sev_mtbs, yose_pts_sf, method = "bilinear")
plot_sev_data <- cbind(yose_pts, pred_CBI, dnbr)
names(plot_sev_data)[7] <- "predicted_CBI"
names(plot_sev_data)[9] <- "dnbr"
head(plot_sev_data)
hist(plot_sev_data$predicted_CBI)
hist(plot_sev_data$dnbr)
plot(predicted_CBI ~ dnbr, data = plot_sev_data) 
# Pretty good correspondence, except for 5 points where Mike's CBI is much higher than MTBS dnbr


# Save the data
write.csv(plot_sev_data, "./Data/plot_sev_data.csv", row.names = FALSE)
