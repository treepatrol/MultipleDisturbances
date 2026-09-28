# Extract environmental data from PRISM for plot coordinates

# load packages
library(prism)
library(terra)
library(sf)
library(tidyverse)
library(tidyterra)
library(flextable)
library(officer)
library(DBI)
library(RPostgres)

# Part 1: Bring in park boundary, climate strata breaks, and field data

# Create SEKI boundary from geodatabase with all NPS boundaries
# Read in all NPS boundaries
nps <- st_read("/Users/jennifercribbs/Documents/TreePatrol.org/Analysis/Data/nps_boundary/nps_boundary.shp") 
# filter out sequoia and kings canyon
SEKI <- nps %>% filter(UNIT_CODE == "SEQU" | UNIT_CODE == "KICA")

# Read in strata breaks
breaks <- read_csv("/Users/jennifercribbs/Documents/SEKI_beetles/Analysis/Data/quantilesforJenny.csv") %>% rename(strata = number, prism_ppt = quant_value)

# connect to database
con <- dbConnect(
  Postgres(),
  dbname = "phd_project",
  host = "localhost",
  port = 5432,
  user = "jennifercribbs"
)

# load plot field data from database
plotCoords <- dbGetQuery(
  con,
  "
  SELECT DISTINCT ON (p.plot_id)
      p.plot_id,
      p.plot_name,
      pv.beg_easting,
      pv.beg_northing
  FROM forest_health.plot p
  JOIN forest_health.plot_visit pv
      ON p.plot_id = pv.plot_id
  WHERE pv.beg_easting IS NOT NULL
    AND pv.beg_northing IS NOT NULL
  ORDER BY p.plot_id, pv.plot_visit_id;
  "
)

# note without distinct we get the 53 plot visits 
dbGetQuery(
  con,
  "
  SELECT
      p.plot_id,
      p.plot_name,
      pv.plot_visit_id,
      pv.beg_easting,
      pv.beg_northing
  FROM forest_health.plot p
  LEFT JOIN forest_health.plot_visit pv
      ON p.plot_id = pv.plot_id
  ORDER BY p.plot_id, pv.plot_visit_id;
  "
)

# disconnect when done with database
dbDisconnect(con)

# Convert data frame to sf object with UTM projection (zone 11 NAD83)
utm_sf <- st_as_sf(plotCoords, coords = c("beg_easting", "beg_northing"), crs = 26911) # zone 11, NAD83

ggplot() +
  geom_sf(data = SEKI, fill = "lightblue", color = "black", lwd = 0.5) +
  geom_sf(data = utm_sf, color = "red", size = 1) +
  theme_minimal() +
  ggtitle("SEKI Plots")

# write to CSV for later use
write.csv(plotData, "SEKIplots.csv", row.names = FALSE)

### Part 2: Find PRISM values for each plot

# 0. Make sure you have set the prism download dir:
prism_set_dl_dir("/Users/jennifercribbs/Documents/TreePatrol.org/Analysis/Data/prism_data")

ppt_r <- rast("/Users/jennifercribbs/Documents/TreePatrol.org/Analysis/Data/prism_data/PRISM_ppt_30yr_normal_800mM4_annual_bil/PRISM_ppt_30yr_normal_800mM4_annual_bil.bil")

tmean_r <- rast("/Users/jennifercribbs/Documents/TreePatrol.org/Analysis/Data/prism_data/prism_tmean_us_30s_2020_avg_30y/prism_tmean_us_30s_2020_avg_30y.tif")

vpdmax_r <- rast("/Users/jennifercribbs/Documents/TreePatrol.org/Analysis/Data/prism_data/prism_vpdmax_us_30s_2020_avg_30y/prism_vpdmax_us_30s_2020_avg_30y.tif")

# check it worked
plot(ppt_r)
plot(tmean_r)
plot(vpdmax_r)

# check this out in tidy terra later
plot(crop(ppt_r, SEKI))
plot(utm_sf, add = TRUE)

plot(crop(tmean_r, SEKI))
plot(utm_sf, add = TRUE)

plot(crop(vpdmax_r, SEKI))
plot(utm_sf, add = TRUE)

# extract the data
seki_ppt <- terra::extract(ppt_r, utm_sf)
seki_tmean <- terra::extract(tmean_r, utm_sf)
seki_vpdmax <- terra::extract(vpdmax_r, utm_sf)

# stick them together
env_data <- data.frame(plot = plots_sf$Plot_Name, aspect = plots_sf$Aspect_beg, slope = plots_sf$Slope_beg, elevation = plots_sf$Plot_Elevation_m, ppt = seki_ppt$PRISM_ppt_30yr_normal_800mM4_annual_bil, vpdmax = seki_vpdmax$prism_vpdmax_us_30s_2020_avg_30y, tmean = seki_tmean$prism_tmean_us_30s_2020_avg_30y) 

ggplot() +
  geom_spatraster(data = crop(ppt_r, SEKI)) +
  geom_sf(data = utm_sf, color = "red", size = 1) +
  geom_sf(data = SEKI, fill = NA, color = "black") +
  theme_minimal()
