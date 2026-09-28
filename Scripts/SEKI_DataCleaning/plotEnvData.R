# Extract environmental data from PRISM for plot coordinates

# load packages
library(prism) # to access and work with PRISM data
library(terra) # spatial analysis, esp raters
library(sf) # spatial analysis, esp vectors
library(tidyverse) # datawrangling, plotting
library(tidyterra) # tidyverse and terra work together?
library(flextable) # for presentation-ready tables
library(DBI) # database connections
library(RPostgres) # Postgres connection
library(ggspatial) # for north arrow and scale bar
library(lubridate) # handles dates

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
      pv.beg_northing, 
      pv.visit_date,
      pv.survey_species,
      pv.survey_stratum
  FROM forest_health.plot p
  JOIN forest_health.plot_visit pv
      ON p.plot_id = pv.plot_id
  WHERE pv.beg_easting IS NOT NULL
    AND pv.beg_northing IS NOT NULL
  ORDER BY p.plot_id, pv.plot_visit_id;
  "
)

plotCoords <- plotCoords %>%
  mutate(year = year(visit_date))

# disconnect when done with database
dbDisconnect(con)

plotCoords <- plotCoords %>%
  mutate(year = year(visit_date))

# Convert data frame to sf object with UTM projection (zone 11 NAD83)
utm_sf <- st_as_sf(plotCoords, coords = c("beg_easting", "beg_northing"), crs = 26911) # zone 11, NAD83

# check they are in SEKI
ggplot() +
  geom_sf(data = SEKI, fill = "lightblue", color = "black", lwd = 0.5) +
  geom_sf(data = utm_sf, color = "red", size = 1) +
  theme_minimal() +
  ggtitle("SEKI Plots")

### Part 2: Find PRISM values for each plot

# set the prism download dir:
prism_set_dl_dir("/Users/jennifercribbs/Documents/TreePatrol.org/Analysis/Data/prism_data")
ppt_r <- rast("/Users/jennifercribbs/Documents/TreePatrol.org/Analysis/Data/prism_data/PRISM_ppt_30yr_normal_800mM4_annual_bil/PRISM_ppt_30yr_normal_800mM4_annual_bil.bil")
tmean_r <- rast("/Users/jennifercribbs/Documents/TreePatrol.org/Analysis/Data/prism_data/prism_tmean_us_30s_2020_avg_30y/prism_tmean_us_30s_2020_avg_30y.tif")
vpdmax_r <- rast("/Users/jennifercribbs/Documents/TreePatrol.org/Analysis/Data/prism_data/prism_vpdmax_us_30s_2020_avg_30y/prism_vpdmax_us_30s_2020_avg_30y.tif")

# check it worked
plot(ppt_r)
plot(tmean_r)
plot(vpdmax_r)

# crop to park boundary
ppt_seki <- crop(ppt_r, vect(SEKI))
tmean_seki <- crop(tmean_r, vect(SEKI))
vpdmax_seki <- crop(vpdmax_r, vect(SEKI))

#check
plot(ppt_seki)
plot(tmean_seki)
plot(vpdmax_seki)

# extract the climate data for each plot data
seki_ppt <- terra::extract(ppt_r, utm_sf)
seki_tmean <- terra::extract(tmean_r, utm_sf)
seki_vpdmax <- terra::extract(vpdmax_r, utm_sf)

# CRS

# convert utm_sf crs to match SEKI and PRISM
map_sf <- st_transform(utm_sf, st_crs(SEKI))

# check crs
st_crs(utm_sf)
st_crs(map_sf)
st_crs(SEKI)
crs(ppt_r)
crs(vpdmax_r)
crs(tmean_r)

# map it all
ppt_map <- ggplot() +
  geom_spatraster(data = ppt_seki) +
  
  scale_fill_viridis_c(
    name = "Precipitation\n(mm)",
    option = "mako",
    na.value = "transparent"
  ) +
  
  geom_sf(
    data = SEKI,
    aes(linetype = UNIT_NAME),
    fill = NA,
    color = "gray80",
    linewidth = 0.7
  ) +
  geom_sf(
    data = map_sf,
    aes(color = factor(year)),
    size = 2,
    alpha = 0.7
  ) +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(
    location = "bl",
    which_north = "true",
    pad_x = unit(0.2, "in"),
    pad_y = unit(0.3, "in"),
    style = north_arrow_fancy_orienteering()
  ) +
  
  scale_linetype_manual(
    name = NULL,
    values = c(
      "Sequoia" = "dashed",
      "Kings Canyon" = "solid"
    )
  ) +
  guides(
    linetype = guide_legend(
      title = "National Park",
      override.aes = list(color = "gray70")
    )
  ) +
  labs(
    title = "Forest Health Plots Sampled 2022-2026",
    #subtitle = "Background: Mean Annual Precipitation Baseline",
    caption = "Coordinate System: NAD83",
    x = "Longitude",
    y = "Latitude"
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    legend.background = element_rect(
      fill = "white",
      color = "transparent"
    ),
    panel.grid.major = element_line(color = "white")
  )

ppt_map
