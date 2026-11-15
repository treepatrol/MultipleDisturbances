# Title: core_physical_inventory.R 
# Input: treesAndCores.csv mostrosity with mixed grain data entry (trees cored and not cored, cores A and B) 
# Output: 1 CSV to facilitate physical inventory
# Author: Jenny Cribbs (with ChatGPT suggestions and review)
# Date Added: 2026-10-03
# Updated: 2026-10-04

library(dplyr)
library(readr)
library(DBI)
library(RPostgres)
library(here)
library(stringr)

# bring in the treesAndCores monstrosity
treesAndCores <- read_csv(
  here("Data", "RawData", "SEKI_Data", "SEKI_treesAndCores.csv"),
  show_col_types = FALSE
) %>%
  janitor::clean_names()

# connect to database, 
con <- dbConnect(
  RPostgres::Postgres(),
  dbname = "phd_project"
)

# pull plot data from database
plot_lookup <- DBI::dbGetQuery(
  con,
  "
  SELECT
      p.plot_name AS source_plot_name,
      p.plot_id,
      p.plot_name AS canonical_plot_name,
      p.target_species,
      p.target_stratum
  FROM forest_health.plot AS p
  WHERE p.site_id = '1'

  UNION

  SELECT
      pa.source_plot_name,
      p.plot_id,
      p.plot_name AS canonical_plot_name,
      p.target_species,
      p.target_stratum
  FROM forest_health.plot_alias AS pa
  JOIN forest_health.plot AS p
      ON pa.plot_id = p.plot_id
  WHERE p.site_id = '1';
  "
)
# join plot look up with trees and cores data
core_deposition <- treesAndCores %>% 
  left_join(plot_lookup, 
            by = c("plot_name" = "source_plot_name"),
            relationship = "many-to-one"
            )
# stop if plot names are unmatched
stopifnot(!anyNA(core_deposition$plot_id))
#_________________

# create a spreadsheet of possible cores to get started with physical inventory
core_candidates <- core_deposition %>%
  mutate(
    core_height_num = suppressWarnings(parse_number(core_height)),
    core_dbh_num    = suppressWarnings(parse_number(core_dbh)),
    
    has_core_measurement =
      !is.na(core_height_num) |
      !is.na(core_dbh_num),
    
    has_core_label = core %in% c("A", "B"),
    
    inventory_reason = case_when(
      has_core_label & has_core_measurement ~ "label + measurement",
      has_core_label                        ~ "label only",
      has_core_measurement                  ~ "measurement only",
      TRUE                                  ~ NA_character_
    )
  ) %>%
  filter(
    has_core_label | has_core_measurement,
    coalesce(core != "Stuck", TRUE)
  )

# expand to check notes
# 58 records of not cored or notes on prior coring
check <- treesAndCores %>%
  mutate(
    core_height_num = suppressWarnings(parse_number(core_height)),
    core_dbh_num    = suppressWarnings(parse_number(core_dbh)),
    
    has_core_measurement =
      !is.na(core_height_num) |
      !is.na(core_dbh_num),
    
    has_core_label = core %in% c("A", "B"),
    
    current_candidate =
      has_core_label | has_core_measurement
  ) %>%
  filter(
    !current_candidate,
    !is.na(notes),
    str_detect(
      notes,
      regex("\\bcore|cored|coring\\b", ignore_case = TRUE)
    )
  ) %>%
  select(
    plot_name, tree_number, species,
    tucson_id, core, core_height, core_dbh,
    core_depth, dbh, notes
  )

# final list for physical inventory   
core_inventory <- core_deposition %>%
  mutate(
    # Parse numeric measurements but preserve raw fields too
    core_height_num = suppressWarnings(parse_number(core_height)),
    core_dbh_num    = suppressWarnings(parse_number(core_dbh)),
    
    has_core_measurement =
      !is.na(core_height_num) |
      !is.na(core_dbh_num),
    
    has_core_label = core %in% c("A", "B"),
    
    inventory_reason = case_when(
      has_core_label & has_core_measurement ~ "label + measurement",
      has_core_label                        ~ "label only",
      has_core_measurement                  ~ "measurement only",
      TRUE                                  ~ NA_character_
    ),
    
    # Flag records that deserve extra attention during inventory
    verify_core = case_when(
      core == "Not_cored" & has_core_measurement ~ "CHECK",
      has_core_label & !has_core_measurement      ~ "CHECK",
      TRUE                                        ~ ""
    ),
    
    # Temporary variable used only for sorting
    target_species_order = match(
      target_species,
      c("PILA", "PIMO", "PICO", "PIBA", "PIAL")
    )
  ) %>%
  
  # Candidate cores, excluding the known-lost stuck core
  filter(
    has_core_label | has_core_measurement,
    is.na(core) | core != "Stuck"
  ) %>%
  
  # Keep each plot together
  arrange(
    target_species_order,
    target_stratum,
    plot_id,
    plot_name,
    tree_number,
    core
  ) %>%
  
  transmute(
    # Plot/tree identity
    target_species, 
    target_stratum, 
    plot_id,
    plot_name, 
    tree_number, 
    species,
    
    # Existing raw core information
    core,
    dbh,
    core_height_raw = core_height,
    core_height_num,
    core_dbh_raw = core_dbh,
    core_dbh_num,
    core_depth,
    tucson_id,
    
    # Audit information
    inventory_reason,
    verify_core,
    
    # Fill during physical inventory
    core_present = NA_character_,
    straw_dbh = NA_real_,
    straw_label = NA_character_,
    physical_pieces = NA_character_,
    inventory_notes = NA_character_,
    
    # Preserve full original notes
    notes
  )

# check number of candidate cores
nrow(core_inventory)

# look at groupings
plot_check <- core_inventory %>%
  count(plot_name, inventory_reason) %>% 
  arrange(plot_name, inventory_reason)

# write out CSV
core_inventory_export <- core_inventory %>% arrange(plot_name, inventory_reason)
write_csv(
  core_inventory_export,
  here("core_inventory_export.csv"),
  na = ""
)

# close database connection
dbDisconnect(con)
