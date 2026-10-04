# Extract environmental data from PRISM for plot coordinates

# load packages
library(here) # for tidy relative paths
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
library(patchwork) # for combining figures

# Part 1: Bring in park boundary, climate strata breaks, and field data

# Create SEKI boundary from geodatabase with all NPS boundaries
# Read in all NPS boundaries
nps <- st_read("/Users/jennifercribbs/Documents/TreePatrol.org/Analysis/Data/nps_boundary/nps_boundary.shp") 
# filter out sequoia and kings canyon
SEKI <- nps %>% filter(UNIT_CODE == "SEQU" | UNIT_CODE == "KICA")

# Read in strata breaks
# updated absolute path
breaks <- read_csv(here("Data", "CleanData", "precipQuantiles.csv")) %>% rename(strata = number, prism_ppt = quant_value)

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
      pv.start_date,
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
  mutate(year = year(start_date))

# disconnect when done with database
dbDisconnect(con)

plotCoords <- plotCoords %>%
  mutate(year = year(start_date))

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

# Map precipitation, park boundaries, and forest-health sampling plots

ppt_map <- ggplot() +
  
  # PRISM precipitation raster
  geom_spatraster(data = ppt_seki) +
  
  scale_fill_viridis_c(
    name = "Precipitation\n(mm)",
    option = "mako",
    na.value = "transparent"
  ) +
  
  # National park boundaries
  geom_sf(
    data = SEKI,
    aes(linetype = UNIT_NAME),
    fill = NA,
    color = "gray80",
    linewidth = 0.7
  ) +
  
  # Survey plots by focal species
  geom_sf(
    data = map_sf,
    aes(color = survey_species),
    size = 2.7,
    alpha = 0.9
  ) +
  
  scale_color_manual(
    name = "Species",
    values = c(
      "PILA" = "#D9A38F",
      "PIMO" = "#C47C67",
      "PICO" = "#985044",
      "PIBA" = "#6E302B",
      "PIAL" = "#421C1C"
    ),
    labels = c(
      "PILA" = "Sugar pine",
      "PIMO" = "Western white pine",
      "PICO" = "Lodgepole pine",
      "PIBA" = "Foxtail pine",
      "PIAL" = "Whitebark pine"
    )
  ) +
  
  # Scale bar
  annotation_scale(
    location = "bl",
    width_hint = 0.3
  ) +
  
  # North arrow
  annotation_north_arrow(
    location = "bl",
    which_north = "true",
    pad_x = unit(0.2, "in"),
    pad_y = unit(0.3, "in"),
    style = north_arrow_fancy_orienteering()
  ) +
  
  # Distinguish Sequoia and Kings Canyon National Parks
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
  
  # Labels
  labs(
    title = "Forest Health Plots Sampled 2022–2026",
    caption = "Coordinate System: NAD83; Background Data Source: PRISM",
    x = "Longitude",
    y = "Latitude"
  ) +
  
  # Theme
  theme_minimal() +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 14
    ),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    legend.position = "right",
    legend.background = element_rect(
      fill = "white",
      color = "transparent"
    ),
    panel.grid.major = element_line(
      color = "white"
    )
  )

# Display map
ppt_map

# Create a function for mapping
make_climate_map <- function(raster, fill_name, palette, panel_title) {
  
  ggplot() +
    geom_spatraster(data = raster) +
    
    scale_fill_viridis_c(
      name = fill_name,
      option = palette,
      na.value = "transparent",
      guide = guide_colorbar(
        direction = "horizontal",
        title.position = "top",
        title.hjust = 0.5,
        barwidth = unit(3.2, "cm"),
        barheight = unit(0.25, "cm")
      )
    ) +
    
    geom_sf(
      data = SEKI,
      aes(linetype = UNIT_NAME),
      fill = NA,
      color = "gray80",
      linewidth = 0.7
    ) +
    
    # Survey plots by focal species
    geom_sf(
      data = map_sf,
      aes(shape = survey_species),
      color = "gray90",
      size = 2.8,
      stroke = 0.9
    ) +
    
    scale_shape_manual(
      name = "Species",
      values = c(
        "PILA" = 15,  # filled square
        "PIMO" = 0,   # open square
        "PICO" = 16,  # filled circle
        "PIBA" = 1,   # open circle
        "PIAL" = 17   # filled triangle
      ),
      labels = c(
        "PILA" = "Sugar pine",
        "PIMO" = "Western white pine",
        "PICO" = "Lodgepole pine",
        "PIBA" = "Foxtail pine",
        "PIAL" = "Whitebark pine"
      )
    ) +
    
    annotation_scale(
      location = "bl",
      width_hint = 0.3
    ) +
    
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
      shape = "none",
      linetype = "none",
      fill = guide_colorbar(
        direction = "horizontal",
        title.position = "top",
        title.hjust = 0.5,
        barwidth = unit(3, "cm"),
        barheight = unit(0.25, "cm")
      )
    ) +
    
    labs(
      title = panel_title,
      x = NULL,
      y = NULL
    ) +
    
    theme_minimal() +
    
    theme(
      plot.title = element_text(face = "bold", size = 11),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      
      legend.position = "bottom",
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 7),
      legend.key.size = unit(0.35, "cm"),
      legend.margin = margin(t = 2, r = 2, b = 2, l = 2),
      legend.box.spacing = unit(0.1, "cm")
    )
    
}

# Use function for 3 variables
ppt_map <- make_climate_map(
  ppt_seki,
  "Precipitation\n(mm)",
  "mako",
  "(a) Mean annual precip"
)

tmean_map <- make_climate_map(
  tmean_seki,
  "Temperature\n(°C)",
  "inferno",
  "(b) Mean temp"
)

vpd_map <- make_climate_map(
  vpdmax_seki,
  "VPDmax",
  "magma",
  "(c) Max VPD"
)

# Stick them together
climate_map <- ppt_map + tmean_map + vpd_map +
  plot_layout(ncol = 3)

climate_map

# Dummy plot for species legend 
shared_legend_plot <- ggplot() +
  
  # Dummy species points
  geom_point(
    data = data.frame(
      species = factor(
        c("PILA", "PIMO", "PICO", "PIBA", "PIAL"),
        levels = c("PILA", "PIMO", "PICO", "PIBA", "PIAL")
      ),
      x = 1:5,
      y = 1
    ),
    aes(x = x, y = y, shape = species),
    color = "gray15",
    size = 2.8
  ) +
  
  scale_shape_manual(
    name = "Species",
    values = c(
      "PILA" = 15,
      "PIMO" = 0,
      "PICO" = 16,
      "PIBA" = 1,
      "PIAL" = 17
    ),
    labels = c(
      "PILA" = "Sugar pine",
      "PIMO" = "Western white pine",
      "PICO" = "Lodgepole pine",
      "PIBA" = "Foxtail pine",
      "PIAL" = "Whitebark pine"
    )
  ) +
  
  # Dummy park-boundary lines
  geom_line(
    data = data.frame(
      park = c("Sequoia", "Sequoia", "Kings Canyon", "Kings Canyon"),
      x = c(1, 2, 3, 4),
      y = c(2, 2, 2, 2)
    ),
    aes(x = x, y = y, linetype = park),
    color = "gray50"
  ) +
  
  scale_linetype_manual(
    name = "National Park",
    values = c(
      "Sequoia" = "dashed",
      "Kings Canyon" = "solid"
    )
  ) +
  
  guides(
    shape = guide_legend(
      nrow = 1,
      order = 1
    ),
    linetype = guide_legend(
      nrow = 1,
      order = 2
    )
  ) +
  
  theme_void() +
  
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 7),
    legend.key.width = unit(0.6, "cm"),
    legend.spacing.x = unit(0.15, "cm")
  )

# and print
final_climate_map <- climate_map / shared_legend_plot +
  plot_layout(
    heights = c(12, 1.2)
  )

final_climate_map

# save 
ggsave(
  "climate_map.png",
  plot = climate_map,
  width = 11,
  height = 5.5,
  units = "in",
  dpi = 300
)
