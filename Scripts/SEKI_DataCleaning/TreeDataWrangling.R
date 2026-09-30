
# Input: Tree level data entered from paper data sheets into Google Sheet
# Code Description: Harmonize data entered primarily at the tree core level to accurately represent tree core, tree, and tree visit attributes. 
# Output: 3 CSV files for tree, tree_visit, and tree_core
# Author: Jenny Cribbs (with ChatGPT suggestions and review)
# Updated: 2026-09-28

# load data wrangling packages
library(tidyverse)
library(janitor)

# ---------------------------------------------------------
# 1. Read and clean raw data
# ---------------------------------------------------------

# load raw data
raw <- read_csv("/Users/jennifercribbs/Documents/R-Projects/MultipleDisturbances/Data/RawData/SEKI_Data/SEKI_treesAndCores.csv")

# convert to snake case with janitor
treesAndCores <- janitor::clean_names(raw)

# ---------------------------------------------------------
# 2. Create tree table
# ---------------------------------------------------------

# select columns for tree table
tree <- treesAndCores %>% select(
  plot_name, 
  tree_number, 
  species
) %>% 
  rename(
    species_code = species, 
  ) %>%
  distinct()

# Check that the same physical tree is not assigned multiple species
treesAndCores %>%
  group_by(plot_name, tree_number) %>%
  summarise(
    n_species = n_distinct(species, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n_species > 1)

# check number of trees 
nrow(tree)
  
# write tree-level data to a csv for loading in database
write_csv(tree, "/Users/jennifercribbs/Documents/R-Projects/MultipleDisturbances/Data/CleanData/tree.csv")

# ---------------------------------------------------------
# 3. Tree Visit Table 
# ---------------------------------------------------------

# review column names
names(treesAndCores)
glimpse(treesAndCores)

# parse wpbr notes colum with regex
treeVisit <- treesAndCores %>%
  mutate(
    wpbr_active_branch = as.numeric(str_extract(
      wpbr, regex("\\d+\\s*[Aa][Bb][Rr][Cc]")
    ) |> str_extract("\\d+")),
    
    wpbr_inactive_branch = as.numeric(str_extract(
      wpbr, regex("\\d+\\s*[Ii][Bb][Rr][Cc]")
    ) |> str_extract("\\d+")),
    
    wpbr_active_bole = as.numeric(str_extract(
      wpbr, regex("\\d+\\s*[Aa][Bb][Oo][Cc]")
    ) |> str_extract("\\d+")),
    
    wpbr_inactive_bole = as.numeric(str_extract(
      wpbr, regex("\\d+\\s*[Ii][Bb][Oo][Cc]")
    ) |> str_extract("\\d+")),
    # fill flags column
    flags = as.integer(
      str_extract(
        wpbr,
        regex("\\d+\\s*flags?", ignore_case = TRUE)
      ) |> str_extract("\\d+")
    ),
    # populate dead top column
    dead_top = case_when(
      str_detect(wpbr, regex("\\b(dead\\s+top|dtop)\\b", ignore_case = TRUE)) ~ TRUE,
      !is.na(wpbr) ~ FALSE,
      TRUE ~ NA
    )
  )

# Ambiguous WPBR codes:
# "BrC" indicates a branch canker but does not specify active/inactive.
# "BC" indicates a bole canker but does not specify active/inactive.
# Split ambiguous counts equally between the two categories rather than
# assigning them arbitrarily. The original source notation is retained
# in wpbr_extra_notes for auditability.

treeVisit <- treeVisit %>% mutate(
    
    wpbr_inactive_branch = case_when(
      wpbr == "4IBC, 1BC" ~ 4,
      TRUE ~ wpbr_inactive_branch
    ),
    
    wpbr_active_branch = case_when(
      wpbr == "4IBC, 1BC" ~ 0,
      TRUE ~ wpbr_inactive_branch
    ),
    
    wpbr_active_bole = case_when(
      wpbr == "4IBC, 1BC" ~ 0.5,
      TRUE ~ wpbr_active_bole
    ),
    
    wpbr_inactive_bole = case_when(
      wpbr == "4IBC, 1BC" ~ 0.5,
      TRUE ~ wpbr_inactive_bole
    )
  )
# check
treeVisit %>%
  filter(wpbr %in% c("4IBC, 1BC")) %>%
  select(
    wpbr,
    wpbr_active_branch,
    wpbr_inactive_branch,
    wpbr_active_bole,
    wpbr_inactive_bole
  ) %>%
  print(width = Inf)

# retain the full wpbr note
treeVisit <- treeVisit %>%
  mutate(
    wpbr_extra_notes = wpbr
  )

# select columns for tree_visit table
treeVisit <- treeVisit %>% select(
  plot_name, 
  tree_number, 
  year, 
  month, 
  day, 
  canopy_position,
  dbh,
  height,
  fire,
  beetles,
  wpbr_extra_notes,
  wpbr_active_branch,
  wpbr_inactive_branch, 
  wpbr_active_bole, 
  wpbr_inactive_bole, 
  flags, 
  dead_top,
  p_dead_rg,
  p_dead_full,
  notes, 
  waypoint_number,
  easting, 
  northing, 
  position_accuracy_ft, 
  slope, 
  aspect,
  elevation_m, 
  species
) %>% 
  rename(
    fire_notes = fire, 
    tree_visit_notes = notes, 
    tree_waypoint_number = waypoint_number, 
    tree_northing = northing,
    slope_tree = slope,
    tree_elevation_m = elevation_m,
    species_code = species
  ) %>% 
  mutate(
    visit_date = lubridate::make_date(year, month, day),
    aspect_tree = as.numeric(na_if(aspect, "Flat")),
    tree_easting = as.numeric(easting), 
    tree_accuracy_ft = as.numeric(na_if(position_accuracy_ft, "N/A"))
  )

# check that plot, tree number, and date are unique (except for 3 B cores)
treeVisit %>%
  count(plot_name, tree_number, visit_date) %>%
  filter(n > 1)

# check number of visits
nrow(treeVisit)

# review 3 plots with B cores
treeVisit %>%
  filter(
    (plot_name == "PICO6_SummitLake" & tree_number == 10) |
      (plot_name == "PICO6a_TwinLakesTrail" & tree_number == 9) |
      (plot_name == "PIMO1c_steep_PICO_KernPlateau" & tree_number == 22)
  ) %>%
  arrange(plot_name, tree_number, visit_date) %>%
  print(width = Inf)

# final select to match the database order 
treeVisit <- treeVisit %>%
  select(
    plot_name,
    tree_number,
    year,
    month,
    day,
    canopy_position,
    dbh,
    height,
    fire_notes,
    beetles,
    wpbr_extra_notes,
    wpbr_active_branch,
    wpbr_inactive_branch,
    wpbr_active_bole,
    wpbr_inactive_bole,
    flags,
    dead_top,
    p_dead_rg,
    p_dead_full,
    tree_visit_notes,
    tree_waypoint_number,
    easting,
    tree_northing,
    position_accuracy_ft,
    slope_tree,
    aspect_tree,
    tree_elevation_m,
    visit_date,
    tree_easting,
    tree_accuracy_ft,
    species_code
  )

# check for trees with the same plotid, treeid, species, and visit date (3 B cores)
treeVisit %>%
  count(plot_name, tree_number, species_code, visit_date) %>%
  filter(n > 1)

# make sure other info for B cores matches
treeVisit %>%
  filter(
    (plot_name == "PICO6_SummitLake" &
       tree_number == 10 &
       species_code == "PICO") |
      (plot_name == "PICO6a_TwinLakesTrail" &
         tree_number == 9 &
         species_code == "PICO") |
      (plot_name == "PIMO1c_steep_PICO_KernPlateau" &
         tree_number == 22 &
         species_code == "PICO")
  ) %>%
  arrange(plot_name, tree_number, visit_date) %>%
  print(n = Inf, width = Inf)

# QA before deduplication
nrow(treeVisit)

treeVisit %>%
  count(plot_name, tree_number, species_code, visit_date) %>%
  filter(n > 1)

# inspect duplicate visit rows if needed
treeVisit %>%
  group_by(plot_name, tree_number, species_code, visit_date) %>%
  filter(n() > 1) %>%
  arrange(plot_name, tree_number, visit_date) %>%
  print(n = Inf, width = Inf)

# distinct visits
treeVisit <- treeVisit %>%
  distinct()

# final tree visit QA
nrow(treeVisit)

treeVisit %>%
  count(plot_name, tree_number, species_code, visit_date) %>%
  filter(n > 1)

# look at repeats to check resurveys
treeVisit %>%
  distinct(plot_name, tree_number, species_code, visit_date) %>%
  count(plot_name, tree_number, species_code, name = "n_visits") %>%
  filter(n_visits > 1) %>%
  arrange(desc(n_visits), plot_name, tree_number) %>%
  print(n = Inf)

write_csv(
  treeVisit,
  "/Users/jennifercribbs/Documents/R-Projects/MultipleDisturbances/Data/CleanData/treeVisit.csv",
  na = ""
)

