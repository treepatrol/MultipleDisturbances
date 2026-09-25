
# Calculate Understory Species Richness -----------------------------------
# Authors: Jenny Cribbs
# Date: 26 April 2024

library(dplyr)
library(tidyr)

# new comment

# Add leading zeros to plotID values for single-digit numbers
all_plots_understory$plotID <- sprintf("%02s", all_plots_understory$plotID)

# filter out substrates that shouldn't count for species richness
species_richness <- all_plots_understory %>%
  filter(!(species %in% c("litter", "rock", "wood", "bareground"))) %>%
  group_by(plotID) %>% # organize by plot
  summarise(richness = n_distinct(species)) %>% # list unique occurences of species in each plot
  complete(plotID = unique(all_plots_understory$plotID)) %>%
  replace_na(list(richness = 0)) # fill in zero for any plot with no plant species e.g. only litter

# Next goal is to bind the plot-level species richness numbers with other plot data, so the richness list needs to be in the right order or maybe better to join on plotID

# read in PRISM data from Joan
prism <- read.csv("/Users/jennifercribbs/Documents/R-Projects/MultipleDisturbances/dataSandbox/CleanData/PRISMdata_YOSEonlyApril29_24.csv")

# read in fire history data from Joan
fire_history <- read.csv("/Users/jennifercribbs/Documents/R-Projects/MultipleDisturbances/dataSandbox/CleanData/LastFireThatBurnedApril29_24.csv")
# bring in saved plot level data (or all tree level data with plot data)
# combined_data <- cbind(plot_data, rich)

# Join fire history and fire data
climate_fire <- fire_history %>% left_join(prism, by = c("plotID" = "PlotID"))

# Add leading zeros to plotID values for single-digit numbers
# Note this converts to character data type
climate_fire$plotID <- sprintf("%02s", climate_fire$plotID)

# Join species richness results
climFireRich <- left_join(climate_fire, species_richness)

# NA values for richness should be zero bc all hits were substrate
climFireRich$richness[is.na(climFireRich$richness)] <- 0

# Write results to a CSV
write_csv(climFireRich, "prismFireRichness.csv", na = "NA")
