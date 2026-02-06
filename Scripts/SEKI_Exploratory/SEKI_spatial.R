# Install packages if necessary
#install.packages('terra')
#install.packages('rnaturalearth')

# load necessary packages for GIS in R
library('terra')
library('rnaturalearth')
library('sf')

# Define goal: where in San Francisco is it safest during an earthquake
# Avoid trees that could fall, soil liquifaction, etc. 

# Compile Relevant Data
# load spat vector (specific to terra) data for precip
precip <- vect('.shp')
# quick look at the spatial vector data
plot(precip, col = 'grey')
# include outline of SEKI
park <- vect('raw/Intro-To-Desktop-GIS-with-QGIS/Shoreline.shp')
plot(park, col = 'darkgreen') # polygons can be filled with color

# load csv file for trees (not spatial in original form)
trees_df <- read.csv('raw/Intro-To-Desktop-GIS-with-QGIS/Street_Tree_Map.csv')
# convert to a spatial object
trees <- vect(trees_df, geom=c('Longitude', 'Latitude'), crs = 'EPSG:4326')
plot(trees) 
# use natural earth to check things out
usa = ne_countries(country = 'united states of america', returnclass = 'sv')
plot(usa) # outlines of USA including Alaska
plot(trees, add = TRUE) # add the trees on top 

# need to clean data to crop out trees not in SF 
# check coordinate reference system 
crs(streets, describe = TRUE)
crs(shore, describe = TRUE)
crs(hz, describe = TRUE)
crs(trees, describe = TRUE)
# All WGS84--could use this, but CA Albers looks better for mapping in CA
streets_ca <- project(streets, 'EPSG:3310') # reproject (can use EPSG.io to check)
crs(streets_ca) # check that it worked
# reproject the rest
shore_ca <- project(shore, streets_ca) # 2nd argument provides target EPSG
hz_ca <- project(hz, streets_ca)
trees_ca <- project(trees, streets_ca)

# Now we can crop out the ocean trees
# ext function creates a bounding box around the SF region
streets_ext <- ext(streets_ca)
plot(streets_ext) # check
shore_ca <- crop(shore_ca, streets_ext)
trees_ca <- crop(trees_ca, streets_ext)


# Create a function for plotting SF objects -------------------------------
# ctr + shift + R to make the section
# Function Interlude--- move this to R folder later if working on a real project

# defines inputs and default color 
plot_trees <- function(geom_top, geom_bottom, 
                       poly_color = 'lightblue', ...) {
  plot_title = 'Street Trees in San Francisco'
  plot(geom_bottom, col = poly_color, axes = FALSE, main = plot_title)
  plot(geom_top, add = TRUE, ...)
}

# use the function to plot the trees, shore, etc. 
plot_trees(trees_ca, shore_ca, col = 'darkgreen', pch = '.')


# Formatting Hazard Data --------------------------------------------------

head(hz_ca)
# load tabular hazard data
hz_data <- read.csv('raw/Intro-To-Desktop-GIS-with-QGIS/Seismic_Hazard_Zones_Data.csv')
hz_data <- unique(hz_data) # remove duplicates
# merge spatial and nonspatial data
hz_ca = merge(hz_ca, hz_data, by.x = 'id', by.y = 'GEOID')
plot(hz_ca, 'Zone_Type')


# Filtering Data ----------------------------------------------------------

# filter SF City trees
summary(trees_ca$DBH)
# create a vector that acts as a flag column for big trees
is_big <- !is.na(trees_ca$DBH) & trees_ca$DBH > 48 & trees_ca$DBH < 384
# filter trees to include only big trees
trees_big = trees_ca[is_big,]

# filter hazard zone for liquefaction
table(hz_ca$Zone_Type, exclude = FALSE) # table more useful than summary for categorical data
is_liquid = hz_ca$Zone_Type == 'Liquefaction'
liquid = hz_ca[is_liquid, ] # filter hz_ca to where liquid is TRUE
nrow(liquid) # checks with 67 that we got from table

plot_trees(trees_big, liquid, col = 'darkgreen', pch = 19)


# Geoprocessing -----------------------------------------------------------

# where do big trees and liquifaction zones both occur
trees_liquid = intersect(trees_big, liquid)
plot_trees(trees_liquid, liquid, col = 'darkgreen', pch = 17)
# where not to walk--use a set buffer of 100ft around big trees (could use actual height if available)
# buffer around trees units are meters 100ft ~ 33m
trees_100 <- buffer(trees_liquid, width = 66)
plot_trees(trees_100, liquid, col = 'red')
# overlay trees and streets
danger_zone <- aggregate(trees_100)
plot_trees(danger_zone, liquid, col = 'red')
# flag danger zones (still showing all streets)
in_danger_zone <- is.related(streets_ca, danger_zone, relation = 'intersects')
head(in_danger_zone) # boolean vector use to add a column 
streets_ca$in_danger = in_danger_zone # add the boolean vector to the df
head(streets_ca) # check that it worked
# define a status column with more understandable names using if/else
streets_ca$status <- ifelse(streets_ca$in_danger,
                            'Danger! Avoid!',
                            'Have a nice walk')
head(streets_ca)

danger_title <- 'Street Safety Status Under Seismic Hazards'
plot(streets_ca, 'status', col = (''))
