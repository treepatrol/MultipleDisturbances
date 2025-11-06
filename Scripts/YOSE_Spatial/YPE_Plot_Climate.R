# Author: Olivia Ross
# Date: 23 May 2025

# this script views spatial locations of plots related to climate data

# packages 
librarian::shelf(sf, dplyr, tmap, prism, here, terra, ggplot2)

# reading in plot locations
ype.plots <- st_read(here("data/YPEPlotsExport/YPEPlots.shp"))

# survey area - Stanislaus-Sequoia
survey_area <- st_read(here("data/survey_area/survey_area.shp"))

# projecting to plot crs (wgs84)
survey_area <- st_transform(survey_area, crs = 4326)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# PRISM DATA

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# setting directory (olivia local)
prism_set_dl_dir(here("data", "prism"))

# tmax, vpd, and ppt normals (1991-2020)
get_prism_normals(type = "tmax", resolution = "800m", annual = TRUE, keepZip = FALSE)
get_prism_normals(type = "ppt", resolution = "800m", annual = TRUE, keepZip = FALSE)
get_prism_normals(type = "vpdmax", resolution = "800m", annual = TRUE, keepZip = FALSE)

# now reading these in 
ppt <- rast("/Users/oliviaross/Documents/MultipleDisturbances/Data/prism/PRISM_ppt_30yr_normal_800mM4_annual_bil/PRISM_ppt_30yr_normal_800mM4_annual_bil.bil")
temp <- rast("/Users/oliviaross/Documents/MultipleDisturbances/Data/prism/PRISM_tmax_30yr_normal_800mM5_annual_bil/PRISM_tmax_30yr_normal_800mM5_annual_bil.bil")
vpd <- rast("/Users/oliviaross/Documents/MultipleDisturbances/Data/prism/PRISM_vpdmax_30yr_normal_800mM5_annual_bil/PRISM_vpdmax_30yr_normal_800mM5_annual_bil.bil")

# stacking rasters
clim.stack <- c(ppt, temp, vpd)

# projecting to plot crs (wgs84)
clim.proj <- project(clim.stack, "EPSG:4326")

# clipping to survey area 
clim.clip <- crop(clim.proj, survey_area)

clim.clip <- mask(clim.clip, survey_area)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# EXTRACTING VALUES AT POINTS

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# reading in elevation data (Olivia calculated this in another project for the 
# survey area)

elev <- rast(here("Data/elevation_surveyarea.tif"))
aspect <- rast(here("Data/aspect_surveyarea.tif"))

# stacking and projecting
topo.stack <- c(elev, aspect)

topo.proj <- project(topo.stack, "EPSG:4326")

# extracting clim and topo at points
clim.values <- extract(clim.clip, ype.plots)
topo.values <- extract(topo.proj, ype.plots)

# binding to points
points.data <- cbind(clim.values, topo.values, ype.plots)

# cleaning the df 
names(points.data)

points.clean <- points.data |>
  dplyr::select(-c(ID)) |>
  dplyr::rename(tmax = PRISM_tmax_30yr_normal_800mM5_annual_bil,
                ppt = PRISM_ppt_30yr_normal_800mM4_annual_bil,
                vpd = PRISM_vpdmax_30yr_normal_800mM5_annual_bil,
                elev = n34_w118_1arc_v3)

# now separating our climate layers 
ppt <- clim.clip[[1]]
temp <- clim.clip[[2]]
vpd <- clim.clip[[3]]

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# MAPPING

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
tmap_mode("view")

tm_shape(temp, raster.downsample=FALSE) +
tm_raster(palette = "Blues", style="quantile", n=5, title= "Temp - 30 Yr Normal") +

tm_shape(survey_area) +
  tm_borders("black") +
  
tm_shape(ype.fixed) +
tm_symbols(col = "orchid", scale= 0.08)  



