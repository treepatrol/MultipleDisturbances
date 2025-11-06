# GBIF: Event Tab
# Authors: Jenny Cribbs
# Date: 09 September 2024

# Overall Input: Read in csv files for plot-level data, fire severity data, and PRISM data
# Overall Output: Clean csv with a row for each plot and columns matching GBIF columns, so the output can be pasted into NPS template Event tab

library(tidyverse)
library(sf)
library(tmap)

# read itmap# read in clean plot data
plotData <- read_csv("dataSandbox/CleanData/PlotLevelData.csv")

# bring in merged ground and MTBS fire severity data
fireData <- read_csv("dataSandbox/fireCompare_22052024.csv")

# bring in estimated effort
effortData <- read_csv("dataSandbox/CleanData/estimatedEffort.csv") %>% 
  rename(plotID = "YPE.Plot.ID", effort = "Estimated.Effort..in.hrs.",
         startDateTime = "Start.Date.time",
         endDateTime = "End.Date.time") %>% 
  select(plotID, effort, startDateTime, endDateTime)

# bring in spatial info
spatial <- read.csv("dataSandbox/CleanData/SpatialDataCleaning.csv")

# join prism and fire data then join to plot data
#prismFire <- left_join(fireData, prismData, by = c("plotID" = "PlotID"))
plotData <- left_join(plotData, fireData)

# add estimated effort data
plotData <- left_join(plotData, effortData)

# add spatial info
plotData <- left_join(plotData, spatial)

# plot end point calculation 

# Adjust the transect length for plots with negative ends 
plotData <-plotData %>% mutate(calculatedLength = case_when(
  plotID == 3 ~ trans_length - 40,
  plotID == 28 ~ trans_length - 19,
  plotID == 31 ~ trans_length - 82.2,
  plotID == 35 ~ trans_length - 26,
  plotID == 44 ~ trans_length - 180,
  plotID == 68 ~ trans_length - 10.3,
  plotID == 71 ~ trans_length - 50,
  plotID == 72 ~ trans_length - 1.5,
  TRUE ~ trans_length
                               ))
# Convert field azimuth from magnetic to true and degrees to radians
plotData <- plotData %>% mutate(azimuth_rad = (plot_azimuth + 12.5) * pi / 180)

# Calculate the easting and northing offsets
plotData <- plotData %>% mutate(delta_easting = calculatedLength * sin(azimuth_rad), delta_northing = calculatedLength * cos(azimuth_rad))

# Calculate the ending UTM coordinates
plotData <- plotData %>% mutate(end_easting = plot_beg_UTM_E + delta_easting, end_northing = plot_beg_UTM_N + delta_northing)

# create spatial points and convert to lat/long (WGS84)
# plot beginnings
plotBeg_sf <- plotData %>%
  group_split(UTM_zone) %>%
  map_dfr(function(df) {
    st_as_sf(df, 
             coords = c("plot_beg_UTM_E", "plot_beg_UTM_N"), 
             crs = paste0("+proj=utm +zone=", unique(df$UTM_zone), " +datum=NAD83")) %>%
      st_transform(crs = 4326)  # Convert to lat/long
  })
# plot ends (calculated)
plotEnds_sf <- plotData %>% 
  group_split(UTM_zone) %>% 
  map_dfr(function(df) {
    st_as_sf(df, 
             coords = c("end_easting", "end_northing"), 
             crs = paste0("+proj=utm +zone=", unique(df$UTM_zone), " +datum=NAD83")) %>%
      st_transform(crs = 4326)  # Convert to lat/long
  })
# plot ends (gps)
plotEndsGPS_sf <- plotData %>% 
  filter(!is.na(plot_end_UTM_N) & !is.na(plot_end_UTM_E)) %>%
  group_split(UTM_zone) %>% 
  map_dfr(function(df) {
    st_as_sf(df, 
             coords = c("plot_end_UTM_E", "plot_end_UTM_N"), 
             crs = paste0("+proj=utm +zone=", unique(df$UTM_zone), " +datum=NAD83")) %>%
      st_transform(crs = 4326)  # Convert to lat/long
  })

# Map pila and non-pila trees with plot points and gps points
tmap_mode("view")

  
  tm_shape(plotBeg_sf) +
  tm_dots(fill = "green", popup.vars = "plotID") +
  tm_shape(plotEnds_sf)+
  tm_dots(fill = "red", popup.vars = "plotID") +
  tm_shape(plotEndsGPS_sf) +
  tm_dots(fill = "pink", popup.vars = "plotID") 

# choose GPS coordinates as endpoints (unless manual review determined calculated is better)
# use calculated when no GPS coordinates were taken
plotData <- plotData %>% 
  mutate(plot_end_final_E = case_when(
    plotEndType == "GPS" ~ plot_end_UTM_E, 
    TRUE ~ end_easting
  )) %>% 
  mutate(plot_end_final_N = case_when(
    plotEndType == "GPS" ~ plot_end_UTM_N, 
    TRUE ~ end_northing
    
  ))

# create spatial points for final plot end position and convert to lat/long (WGS84)
plotData <- plotData %>%
  group_split(UTM_zone) %>%
  map_dfr(function(df) {
    st_as_sf(df, 
             coords = c("plot_end_final_E", "plot_end_final_N"), 
             crs = paste0("+proj=utm +zone=", unique(df$UTM_zone), " +datum=NAD83")) %>%
      st_transform(crs = 4326)  # Convert to lat/long
  })

# extract the latitude and longitude coordinates
plotData <- plotData %>%
  mutate(
    verbatimLatitudeEnd = st_coordinates(plotData)[, 2],  # extract latitude
    verbatimLongitudeEnd = st_coordinates(plotData)[, 1]  # extract longitude
  )

# bring in tree level data
treeData <- read.csv("/Users/jennifercribbs/Documents/YOSE/Analysis/MultipleDisturbances/outputSandbox/occurrence_positions.csv")
# separate occurrenceID into plotID and treeNum
treeData <- treeData %>% 
  separate(occurrenceID, into = c("plotID", "tree_number"), sep = "-", convert = TRUE) 
# remove Es for plot ID prior to join 
treeData$plotID <- gsub("E", "", as.character(treeData$plotID))
# change data type
treeData$plotID <- as.integer(treeData$plotID)

# Example: Assume your data is in a data frame called tree_data with columns: plotID, longitude, latitude
tree_sf <- treeData %>%
  filter(!is.na(verbatimLatitude)) %>% 
  st_as_sf(coords = c("verbatimLongitude", "verbatimLatitude"), crs = 4326) %>%  # Convert to sf object with WGS84 CRS
  group_by(plotID) %>%
  summarise(geometry = st_union(geometry)) %>%  # Merge all points in each plot
  mutate(convex_hull = st_convex_hull(geometry))  # Compute convex hull

# Calculate area in square meters (convert to a projected CRS first)
tree_sf <- tree_sf %>%
  st_transform(crs = 32611) %>%  # Change to an appropriate UTM zone for your region
  mutate(area_m2 = st_area(convex_hull)) %>% st_drop_geometry()

# join tree polygons with the plot data
plotData <- left_join(plotData, tree_sf)

# calculate area based on plot type
plotData <- plotData %>% 
  mutate(area_PILA = case_when(
    plotShape == "Transect" ~ trans_length * width_pila,
    plotShape == "Balloon" ~ as.numeric(area_m2),
    plotShape == "Trigonometry" ~ as.numeric(area_m2),
    plotShape == "Observation" ~ NA,
    TRUE ~ trans_length * width_pila
  ))

plotData <- st_drop_geometry(plotData)  # Removes spatial geometry

# rename and add columns to match template
eventData <- plotData %>% rename(eventID = plotID, 
                                     samplingProtocol = plot_type,
                                     samplingEffort = effort,
                                     verbatimLatitudeStart = plot_beg_UTM_N,
                                     verbatimLongitudeStart = plot_beg_UTM_E,
                                     verbatimLatitudeStop = verbatimLatitudeEnd,
                                     verbatimLongitudeStop = verbatimLongitudeEnd,
                                     eventRemarks = plot_notes
                                     ) %>% 
  mutate(parentEventID = "", 
         sampleSizeValue = area_PILA, 
         sampleSizeUnit = "square meters",
         waterFeatureName = "",
         countryCode = "US",
         stateProvince = "California",
         county = "", # need to fill in column from st_intersects
         habitatDescription = "mixed-conifer forest",
         verbatimCoordinateSystem = "decimal degrees",
         verbatimSRS = "EPSG:4326", # 26910 for zone 10 plots or 4326 for lat long for all
         verbatimDatum = "WGS84",# WGS84 for lat/long
         coordinateUncertaintyinMeters = "", # maybe add GPS field uncertainty with metadata notes 
         coordinatePrecision = "", # this may be a feature of the GPS units
         publicDisplay = "yes",
         dataAccess = "unrestricted",
         fieldNumber = "",
         fieldNotes = "",
         verbatimElevation = (plot_elevation_ft / 3.28084), # convert to meters
         verbatimDepth = "",
         waterFeatureSubstrate = "",
         locationRemarks = "",
         footprintWKT = "",
         footprintSRS = "", 
         azimuth = as.numeric(plot_azimuth + 12.5),
         verbatimLatitude = verbatimLatitudeStart,
         verbatimLongitude = verbatimLongitudeStart)


eventExcelData <- eventData %>% select (eventID, parentEventID, samplingProtocol, sampleSizeValue, sampleSizeUnit, samplingEffort, startDateTime, endDateTime, waterFeatureName, countryCode, stateProvince, county, habitatDescription, verbatimLatitude, verbatimLongitude, verbatimLatitudeStart, verbatimLongitudeStart, verbatimLatitudeStop, verbatimLongitudeStop, verbatimCoordinateSystem, verbatimSRS, verbatimDatum, coordinateUncertaintyinMeters, coordinatePrecision, publicDisplay, dataAccess, fieldNumber, fieldNotes, eventRemarks, plot_elevation_ft, verbatimElevation, verbatimDepth, waterFeatureSubstrate, locationRemarks, footprintWKT, footprintSRS, slope, aspect, azimuth) %>% 
  arrange(by = eventID)

write.csv(eventExcelData, "/Users/jennifercribbs/Documents/YOSE/NPS_Report/Data Products/gbifEvent.csv")

# Spatial data wrangling to fix up for December deadline
# For now, changing 2 zone 10 plots by hand in Excel sheet (YPE32, YPE50) to zone 10
# Changed plots 10 and 43 to Madera county by hand
# Changed Tuolume County plots based on eyeballing county line: 3 plots near Smith Mountain (199-201, 208-209, 202-206), 3 near Hetch Hetchy (1281-1301, 0941, 881-931), Cathedral Creek, Mount Hooper, 2 near Lake Eleanor (247-248, 137-138), 266-264 (plot44?), 0091-0092, 01751-01761 (return of the saplings?), 0093-0094, 0095-0096, 01871-01861, North Crane Creek, and Tuolume Grove, bald mountain YPE 5, Aspen Valley Area (0255-0254, 01311-01321, Yosemite Wilderness, need to be changed to Tuolume county