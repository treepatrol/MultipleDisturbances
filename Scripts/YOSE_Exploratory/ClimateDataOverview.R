#### Script to demonstrate how to access PRISM climate and weather data and extract values from it. ####
# FOCAL-Latimer Lab meeting 11/25/2024
# Andrew Latimer 

# This approach uses the prism R library, which has some functions that shortcut some of the more general and manual steps Derek is demonstrating. 
# So it's limited to PRISM data only, and less flexible that those more general methods, but sometimes quicker if you want to grab data from particular points or areas. 

# Which gridded climate product to use??? Lots to consider! But here's one recent paper by authors including some familiar names that compares PRISM to TopoWx, DayMet, Livneh, and ClimateNA. 
# https://www.fs.usda.gov/psw/publications/wright/psw_2022_wright001_stern.pdf
#It concludes PRISM is overall best although not always best for every variable. If you need daily data, though, for computing things like Growing Degree Days, note gridmet is available for a slightly longer time period (1979-present), and includes many more variables (e.g. ET, PDSI, burning index, etc). https://www.climatologylab.org/gridmet.html

# PRISM website: https://prism.oregonstate.edu/
# Description of datasets: https://prism.oregonstate.edu/documents/PRISM_datasets.pdf
# Key points: - PRISM produces both long-term climatic averages ("normals") available at ~800m resolution, and daily, monthly, and annual data at ~4 km resolution (available down to ~800m for a fee of several thousand $). 
# - Time periods: Normals are averages across 1991-2020, monthly and annual data is available from 1895-present, and daily data from 1981-present.
# - Variables available are: tmean (avg daily mean temperature), tmin (avg daily min temperature), tmax (avg daily max temperature), ppt (total precipitation), tdmean (avg daily dewpoint temperature), vpdmin (avg daily minimum vapor pressure deficit) and vpdmax (avg daily maximum vapor pressure deficit).
# - Note: they offer 2 forms of the time-series data sets: one that uses all the data available at any given time to give the best snapshot for that time period("AN"); and a "long-term" data set that uses only longer-term, more stable stations to improve temporal consistency ("LT"). For our work looking at change over time, it's probably important to use the "long-term" or "LT" versions.

# This demo just focuses on ppt and tmean data. 

# Load libraries
library(prism) 
library(sf)
library(dplyr)
library(ggplot2)
library(terra)
library(tidyterra)
library(raster)
library(lubridate)

#### GETTING THE DATA #### 

# The first step in using PRISM is usually to download the data we need from the PRISM site. The prism library does this for us, but we need to set up a directory to store the data.

# Normally we need to make a subdirectory to store the climate data from PRISM 
#dir.create("../prism_data")
#prism_data_path <- "../prism_data/"

# Alternatively, we might connect to a shared drive where we (or someone) already downloaded and stored the relevant PRISM data. In that case we would set the data path to point to that location instead. 
prism_data_path <- "/Users/latimer/Google Drive/My Drive/Cones/GeoData/prism_data/" # or whatever the path is on your computer!

# Download the climate layers we want from PRISM 
#   (assuming it's not already stored on Box or somewhere else!)

# To work with the prism library, we have to set this path to tell the prism functions where to store and access the data
prism_set_dl_dir(prism_data_path) 

# Download long-term normal data -- warning: this failed and I had to mqnually download these from https://prism.oregonstate.edu/normals/ (to do this, click select the spatial resolution then click "Download All Normals Data")
#get_prism_normals("tmean", keepZip = FALSE, resolution = "800m")
#get_prism_normals("ppt", keepZip = FALSE, resolution = "800m")

# Download annual data
get_prism_annual("tmean", years = 1993:2023, keepZip = FALSE)
get_prism_annual("ppt", years = 1993:2023, keepZip = FALSE)

# Download monthly data
get_prism_monthlys("tmean", years = 1993:2023, mon = 1:12, keepZip = FALSE)
get_prism_monthlys("ppt", years = 1993:2023, mon = 1:12, keepZip = FALSE)

# Check on those files that we downloaded and see what's there
list.files(prism_data_path) %>%
  head(20)
list.files(prism_data_path, recursive = TRUE) %>%
  head(30)
# Look at a the first available file as a raster 
testfile_name <- list.files(prism_data_path, recursive = TRUE)[1]
example_raster <- rast(paste0(prism_data_path, testfile_name))
plot(example_raster)


#### USING THE DATA ####

# Getting time series values for a location.

# First we can walk through the steps of extracting annual values for an arbitrary location for which we have latitude and longitude coordinates in digital degrees, then compiling and looking at the data 

# At the end of this script, there are drafts of functions that can pull data for multiple locations. 

# Choose the site 
focal_site_name <- "MammothLakes"
focal_site <- c(-118.990280, 37.592576)
# Note that coordinate refererence system (CRS) for PRISM layers is NAD83 and uses digital degrees longitude and latitude. If the location data we have is in a different projection, we'd need to reproject it. 

#For example, to use a centroid of a TRS location: 
# plss <- st_read("./Public_Land_Survey_System_(PLSS)__Sections/Public_Land_Survey_System_(PLSS)%3A_Sections.shp")
# plss_reproj <- st_transform(plss_data, st_crs(example_raster))
# focal_centroid <- plss_reproj %>% filter(Township == "T24N" & Range == "R10E" & Section == 24) %>% st_centroid()
# focal_site <- st_coordinates(focal_centroid$geometry)

# Extract annual tmean data for the location using the prism library's slice function
focal_data_tmean <- prism_archive_subset("tmean", "annual", years = 1993:2023)
tmean_plot <- pd_plot_slice(focal_data_tmean, focal_site) # This function returns a bunch of stuff but what we want is the $data component 

# To get water year precipitation, instead of calendar year, we have to grab the monthly data instead and do some mutating
focal_data_ppt <- prism_archive_subset("ppt", "monthly", years = 1993:2023)
ppt_plot <- pd_plot_slice(focal_data_ppt, focal_site)
ppt_plot$data <- ppt_plot$data %>% 
  mutate(month = month(date)) %>%
  mutate(water_year = ifelse(month(date) < 10, year(date), year(date)+1)) 
# Sum each year's ppt from Oct 1 - Sept 30 
# Then summarize back up to annual, this time using the water year to group by
ppt_plot$data <- ppt_plot$data %>% 
  group_by(water_year) %>% 
  summarize(ppt = sum(data, na.rm = TRUE), num_months = length(unique(month))) %>% 
  filter(num_months == 12) %>% # only keep years with all 12 months of data
  ungroup()

# Get the data for the site and rename the columns
tmean <- tmean_plot$data %>%
  mutate(year = year(date))
names(tmean)[1] <- "tmean"
ppt <- ppt_plot$data

# Combine the tmean and ppt data using year 
climate_data <- left_join(ppt, tmean, by = join_by(water_year == year))

# Add year and anomaly values to the data frame
climate_data <- climate_data %>% 
  mutate(year = year(date), tmean_anomaly = tmean - mean(tmean), ppt_anomaly = ppt - mean(ppt))


#### DISPLAYING THE DATA -- Plot the time series of temperature and precipitation anomalies for the site ####

# For visualization, get values of the 10th hottest and coldest years for the site
hottest_years <- climate_data %>% arrange(desc(tmean)) %>% slice(1:10)
hotyears_threshold <- min(hottest_years$tmean_anomaly)
coldest_years <- climate_data %>% arrange(tmean) %>% slice(1:10)
coldyears_threshold <- max(coldest_years$tmean_anomaly)

# Also get values of the 10th wettest and driest years for the site
wettest_years <- climate_data %>% arrange(desc(ppt)) %>% slice(1:10)
wetyears_threshold <- min(wettest_years$ppt_anomaly)
driest_years <- climate_data %>% arrange(ppt) %>% slice(1:10)
dryyears_threshold <- max(driest_years$ppt_anomaly)

# Make the plots 
p1 <- ggplot(climate_data, aes(x = date, y = tmean_anomaly, color = tmean_anomaly)) + 
  geom_point(size = 2) + geom_line(color = "darkgray") +
  labs(title = paste("Annual Tmean anomalies at", focal_site_name), x = "Year", y = "Value") + theme_minimal() + scale_color_gradient(low="darkblue", high="yellow") + 
  geom_line(y = hotyears_threshold, color = "darkblue") + 
  geom_line(y = coldyears_threshold, color = "darkblue")
p2 <- ggplot(climate_data, aes(x = date, y = ppt_anomaly, color = ppt_anomaly)) + 
  geom_point(size = 2) + geom_line(color = "darkgray") +
  labs(title = paste("Annual PPT anomalies at", focal_site_name), x = "Year", y = "Value") + theme_minimal() + scale_color_gradient(low="orange", high="darkblue") +
  geom_line(y = wetyears_threshold, color = "darkblue") +
  geom_line(y = dryyears_threshold, color = "darkblue")

gridExtra::grid.arrange(p1, p2, ncol = 1)


#### Functions we could use to get prism values for lots of locations ####

extract_prism_ts <- function(location, years, variable, resolution = "annual", location_name = NA) {
  # Extract time series data for a location from PRISM
  # location: a vector of latitude and longitude coordinates in digital degrees
  # variable: the climate variable to extract (e.g., "tmean", "ppt")
  # resolution: the temporal resolution of the data to extract (e.g., "annual", "monthly")
  # NOTE: the prism archive location already has to be set using prism_set_dl_dir() for this function to work 
  
  # Extract the data for the location using the prism library's slice function
  prism_subset <- prism_archive_subset(variable, resolution, years = years)
  focal_plot <- pd_plot_slice(prism_subset, location)
  
  # Get the data for the site and rename the columns
  focal_data <- focal_plot$data
  names(focal_data)[1] <- variable
  focal_data$location <- location_name
  return(focal_data)
}

# Checking that it works
testdata <- extract_prism_ts(focal_site, years = 1993:2023, "tmean", "annual", focal_site_name)

# Function that can handle a list of locations all at once 
extract_prism_ts_list <- function(locations, years, variable, resolution = "annual") {
  # Extract time series data for a list of locations from PRISM
  # locations: a data frame containing the names ("location") and latitude and longitude coordinates of the locations
  # variable: the climate variable to extract (e.g., "tmean", "ppt")
  # resolution: the temporal resolution of the data to extract (e.g., "annual", "monthly")
  
  # Initialize an empty data frame to store the extracted data
  extracted_data <- data.frame()
  
  # Loop through the locations and extract the data for each location
  for (i in 1:nrow(locations)) {
    location <- c(locations$longitude[i], locations$latitude[i])
    location_name <- locations$location[i]
    focal_data <- extract_prism_ts(location, years = years, variable, resolution, location_name)
    extracted_data <- rbind(extracted_data, focal_data)
  }
  return(extracted_data)
}

# Test the functions 

# create an example location data frame
location_data <- data.frame(location = c("IceHouseRoad", "Hayfork", "MammothLakes"),
                            latitude = c(38.793884, 40.593064, 37.592576),
                            longitude = c(-120.398519, -123.175443, -118.990280))

# test the function on the example data
climate_data <- extract_prism_ts_list(location_data, years = 1993:2023, "tmean", "annual")

# add some useful columns to the extracted data 
climate_data <- climate_data %>%
  mutate(year = year(date)) %>%
  group_by(location) %>%
  mutate(tmean_anomaly = tmean - mean(tmean)) %>%
  ungroup()

# Check that it looks ok by plotting the time series of climate anomalies for the sites
ggplot(climate_data, aes(x = date, y = tmean_anomaly, group = location, color = location)) + 
  geom_point(size = 2) + #geom_line(color = "darkgray") +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Annual Tmean anomalies at multiple sites", x = "Year", y = "Value") + theme_minimal() + scale_color_manual(values = c("darkblue", "darkgreen", "gold")) 

# Make a boxplot of climate values instead
ggplot(climate_data, aes(x = location, y = tmean, fill = location)) + 
  geom_boxplot(notch = TRUE) + 
  labs(title = "Annual Tmean Values", x = "Location", y = "Value") + theme_minimal() + scale_fill_manual(values = c("darkblue", "darkgreen", "gold"))