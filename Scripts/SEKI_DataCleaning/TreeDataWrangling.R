
# Input: Tree level data entered from paper data sheets into Google Sheet
# Code Description: harmonize data entered primarily at the tree core level to accurately represent tree core, tree, and tree visit attributes. 
# Output: 3 CSV files for tree, tree_visit, and tree_core
# Jenny Cribbs (with ChatGPT suggestions and review)
# Updated: 2026-09-28

# load data wrangling packages
library(tidyverse)
library(janitor)

# load raw data
raw <- read_csv("/Users/jennifercribbs/Documents/R-Projects/MultipleDisturbances/Data/RawData/SEKI_Data/SEKI_treesAndCores.csv")

# convert to snake case with janitor
treesAndCores <- janitor::clean_names(raw)

# create a list of candidate trees
treeCandidate <- treesAndCores %>%
  select(
    plot_name,
    tree_number,
    species,
    dbh,
    height
  )

# check for plot name and tree number combinations with different values of species, dbh, and height (should be zero)
treeCandidate %>%
  group_by(plot_name, tree_number) %>%
  summarise(
    nRows = n(),
    nSpecies = n_distinct(species, na.rm = TRUE),
    nDBH = n_distinct(dbh, na.rm = TRUE),
    nHeight = n_distinct(height, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(
    nRows > 1,
    nSpecies > 1 |
      nDBH > 1 |
      nHeight > 1
  )

# select columns for tree table
tree <- treesAndCores %>% select(
  plot_name, 
  tree_number, 
  species, 
  easting, 
  northing, 
  elevation_m,
  position_accuracy_ft, 
  slope, 
  aspect
) %>% 
  rename(
    species_id = species, 
    tree_easting = easting, 
    tree_northing = northing, 
    slope_tree = slope, 
    aspect_tree = aspect, 
    tree_elevation_m = elevation_m
  ) %>%
  distinct()
  

# check for any duplicate plot_name, tree_number combinations
tree %>%
  count(plot_name, tree_number, name = "nRows") %>%
  filter(nRows > 1) # none

tree %>%
  semi_join(
    tree %>%
      count(plot_name, tree_number) %>%
      filter(n > 1),
    by = c("plot_name", "tree_number")
  ) %>%
  arrange(plot_name, tree_number)

# check for expected number of rows
tree %>%
  summarise(
    nTrees = n(),
    nPlots = n_distinct(plot_name)
  )

# check candidate trees (1407) versus unique trees (1404)
# three trees have multiple cores
treesAndCores %>%
  summarise(
    nRows = n(),
    nPlotTree = n_distinct(paste(plot_name, tree_number))
  )

# remove plot visit fields from tree table
tree <- tree %>%
  left_join(plot_lookup, by = "plot_name") %>%
  select(
    -plot_name,
    -tree_easting,
    -tree_northing,
    -position_accuracy_ft
  )



# write tree-level data to a csv for loading in database
write_csv(tree, "/Users/jennifercribbs/Documents/R-Projects/MultipleDisturbances/Data/CleanData/treeData.csv")
