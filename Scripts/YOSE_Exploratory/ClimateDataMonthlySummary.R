# Script to demonstrate loading and summarizing monthly climate data layers

library(tidyr)
library(terra)
library(sf)
library(purrr)

#### Constants

# Path to the directory containing the monthly climate data layers
CLIM_DIR = "~/Downloads/climate_workshop/prism_data_monthly"

# Path to a file containing points to get cliate data for
PLOTS_FILE = "~/Downloads/climate_workshop/plot_locs/ofo-plots.gpkg"


#### Workflow

## Load the climate data layers

# Get a list of all the relevant files
rast_files = list.files(CLIM_DIR, pattern = "[0-9]{6}_bil.bil$")

# Load them all as a big many-layer raster
r = rast(paste0(CLIM_DIR, "/", rast_files))

## Manually inspecting the layers

# Example of viewing a single layer (i.e. one month)
plot(r[[1]])

# What is the name of that layer?
names(r)[[1]]

# You could do the same thing for any of the layers. If you want to look up which layer to display,
# you can display all the names:
names(r)

# Then you can plot layer(s) by name
plot(r[["PRISM_tmean_stable_4kmM3_202210_bil"]])

## Extracting values from the rasters at point (e.g., plot) locations

# Load plot locations
plots = st_read(PLOTS_FILE)

# Extract values from the rasters at the plot locations
extracted = extract(r, plots, method = "bilinear")

# The extracted data does not have any of the attributes (or even geospatial locations) of the
# original plot data:
head(extracted)

# So we need to bind the extracted data onto the plot data. But first let's make the column names
# more intelligible.

# Drop the "ID" column at the start of the extracted data, so that we only have actual extracted
# data
extracted = extracted[, -1]

# A function to take the 5th part of a string (split by "_")
get_5th = function(x) {
  parts = strsplit(x, "_")
  fifth = map(parts, 5)
  unlist(fifth)
}

names(extracted) = get_5th(names(extracted))

# Prepend these names with "ppt_
names(extracted) = paste0("ppt_", names(extracted))
head(extracted)

# Join these columns onto the plot data. The order of the rows in the extracted data is the same as
# the order of the plot data.
plots = cbind(plots, extracted)
head(plots)

# Now we have the climate data for our plots in a tabluar format! Now let's do some tabular data
# wrangling.


## Tabular data wrangling

# Remove the geospatial data
d = st_drop_geometry(plots)
head(d)

# For data summaries, some operations are easier in a "long" format
d_long = pivot_longer(d, cols = starts_with("ppt_"), names_to = "prism_layer", values_to = "value")

head(d_long)

# Now we will split the "ppt" and "YYYYMM" part of the column names into their own columns
d_long = d_long  |>
  separate(prism_layer, into = c("clim_var", "yearmonth"), sep = "_")
head(d_long)

# Now we will split out the "YYYY" and "MM" parts of the "yearmonth" column
d_long = d_long  |>
  separate(yearmonth, into = c("year", "month"), sep = 4)

# Now we have a nice way of operating on the monthly climate data for eacn plot. For example, we can
# pull out precip for February 2020
d_long |>
  filter(clim_var == "ppt", year == "2020", month == "02")

# Or we can get annual summaries for each plot and climate variable
annual_summary = d_long |>
  group_by(id, year, clim_var) |>
  summarize(mean = mean(value, na.rm = TRUE),
            sd = sd(value, na.rm = TRUE))
head(annual_summary)

