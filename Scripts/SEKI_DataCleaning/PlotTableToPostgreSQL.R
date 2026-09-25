# ===============================
# Clean & Export Plot Table to PostgresSQL Database (Static Plot + Plot Visit Tables)
# ===============================

# load required packages
library(dplyr)    # for tabular data
library(janitor)   # for clean_names()
library(readr)     # for read_csv() and write_csv()
library(stringr)  # for data cleaning requiring string detection

# Input: plot data from 2022 and 2024 entered via Google Sheets, and combined with manual review in Excel on my Mac. 
# Code Description: reads in data, uses the janitor package to convert all column names to snake case, and uses the transmute command to select and order columns to match database schema. Mutate further cleans up some spatial data entry. 
# Output: two CSV files one for static plot attributes (plot talbe) and one for dynamic plot attributes (plot visit) that change from visit to visit. Ideally the code will simultaneously push of both to the relational database (PGAdmin). 

# ---- Step 1: Read raw data ----
# plot data
raw <- read_csv('/Users/jennifercribbs/Documents/TreePatrol.org/Analysis/SEKIanalysis/SEKI_all_PlotData_20250821.csv')

# raw1 and raw2 look identical--should formally check 
#raw2 <- read_csv('/Users/jennifercribbs/Documents/R-Projects/MultipleDisturbances/Data/RawData/SEKI_Data/SEKIplots.csv')

# ---- Step 2: Clean column names ----
# Converts to snake_case and strips weird characters
df <- raw %>%
  janitor::clean_names()

# ---- Step 3: static plot table columns ----
# select only the columns needed for the static plot table
# rename them to match schema, and order correctly

plot <- df %>%
  transmute(
    plot_name,         
    site_id = 1,
    sampling_protocol = "random_species_precip",
    date_established = lubridate::make_date(year, month, day),
    plot_route,
    target_species_code = target_species_id,
    primary_species_code = species1,
    associated_tree_species,
    other_associated_species,
    soil = soil_types,
    general_plot_notes = general_plot_observations
    # geom will be added later in PostGIS or R sf
  )

# ---- Step 4: dynamic plot visit table columns ----
# select only the columns needed for the dynamic plot table
# rename them to match schema, and order correctly 
plot_visit <- df %>%
  transmute(
    plot_name, 
    visit_date = lubridate::make_date(year, month, day),
    survey_type = "tree",
    crew,
    
    waypoint_number_beg = paste(gps, waypoint_number_beg, sep = "_"),
    beg_northing,
    beg_easting,
    beg_accuracy_ft,
    
    waypoint_number_end  = paste(gps, waypoint_number_end, sep = "_"),
    end_northing,
    end_easting,
    end_accuracy_ft = end_accuracy,
    transect_field_width_m,
    transect_length_m,
    # Define overall plot width
    transect_width_m = case_when(
      plot_name == "PIMO_EX2_PIBA1c_KernPlateau" ~ 38, # asymmetrical plot
      TRUE ~ as.numeric(transect_field_width_m) * 2), # normal plots
    
    azimuth,
    slope_beg,
    slope_end,
    aspect_beg,
    aspect_end,
    plot_elevation_m,
    
    percent_rock,
    alternate_hosts,
    plot_visit_notes = general_plot_observations,
    general_vegetation_notes = general_vegetation_observations
    # geom will be added later in PostGIS or R sf
  )


# ---- Step 4: Write clean CSVs ----

# static plot table
write_csv(plot, "plot_clean.csv")
# dynamic plot table
write_csv(plot_visit, "plot_visit_clean.csv")

# ---- Step 5: Load into PostGreSQL ----

# Database write operations are commented out during script development.
# Uncomment only when intentionally loading data into PostgreSQL.

# load packages to connect to PostGreSQL
library(DBI)
library(RPostgres)

# connect to database
con <- dbConnect(
  RPostgres::Postgres(),
  dbname = "phd_project",
  host = "localhost",
  user = "jennifercribbs",
  password = ""
)

# test the connection
dbGetQuery(
  con,
  "SELECT current_database(), current_user;"
)

# connect to the plot table and save column names as a list
db_plot_cols <- dbListFields(
  con,
  DBI::Id(schema = "forest_health", table = "plot")
)

# store column names from r object
r_plot_cols <- names(plot)

# cross check column names
setdiff(r_plot_cols, db_plot_cols)
setdiff(db_plot_cols, r_plot_cols)

r_plot_cols <- names(plot)

db_plot_cols <- dbListFields(
  con,
  DBI::Id(schema = "forest_health", table = "plot")
)

setdiff(r_plot_cols, db_plot_cols)
setdiff(
  db_plot_cols,
  c(r_plot_cols, "plot_id")
)

# check that plot table is empty
dbGetQuery(
  con,
  "SELECT COUNT(*) AS n FROM forest_health.plot;"
)
# check on row per plot
nrow(plot)

plot %>%
  count(plot_name) %>%
  filter(n > 1)

# load the plot table
# dbWriteTable(
#   con,
#   DBI::Id(schema = "forest_health", table = "plot"),
#   plot,
#   row.names = FALSE,
#   append = TRUE
# )

# check columns
r_visit_cols <- names(plot_visit)

db_visit_cols <- dbListFields(
  con,
  DBI::Id(schema = "forest_health", table = "plot_visit")
)

setdiff(
  r_visit_cols,
  db_visit_cols
)

setdiff(
  db_visit_cols,
  c(r_visit_cols, "plot_visit_id")
)

# look at plot IDs
plot_lookup <- dbGetQuery(
  con,
  "
  SELECT plot_id, plot_name
  FROM forest_health.plot;
  "
)

# join by plot name to get plot_ids
plot_visit_load <- plot_visit %>%
  left_join(plot_lookup, by = "plot_name")

head(plot_lookup)

# drop plot name before loading
plot_visit_load <- select(plot_visit_load, -plot_name)

# load the plot visit table
# dbWriteTable(
#   con,
#   DBI::Id(schema = "forest_health", table = "plot_visit"),
#   plot_visit_load,
#   row.names = FALSE,
#   append = TRUE
# )

# disconnect from database
dbDisconnect(con)
