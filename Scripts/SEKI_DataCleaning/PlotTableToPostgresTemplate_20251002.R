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
# Output: two CSV files (one for static plot attributes) and one for dynamic plot attribues that change from visit to visit. Ideally the code will simultaneously push of both to the relational database (PGAdmin). 

# ---- Step 1: Read raw data ----
# plot data
raw <- read_csv('/Users/jennifercribbs/Documents/TreePatrol.org/Analysis/SEKIanalysis/SEKI_all_PlotData_20250821.csv')

# ---- Step 2: Clean column names ----
# Converts to snake_case and strips weird characters
df <- raw %>%
  janitor::clean_names()

# ---- Step 3: static plot table columns ----
# select only the columns needed for the static plot table
# rename them to match schema, and order correctly

plots <- df %>%
  transmute(
    plot_name,         
    site_id = 1,
    plot_route,
    target_species_id
  )

# ---- Step 4: dynamic plot visit table columns ----
# select only the columns needed for the dynamic plot table
# rename them to match schema, and order correctly 
plot_visit <- df %>%
  transmute(
    plot_id,         
    visit_date = lubridate::make_date(year, month, day),
    crew,
    transect_length_m,
    transect_field_width_m,
    #azimuth,
    slope_beg,
    slope_end,
    aspect_beg,
    aspect_end,
    soil_types,
    percent_rock,
    alternate_hosts,
    general_plot_notes = general_plot_observations,
    general_vegetation_notes = general_vegetation_observations
    # geom will be added later in PostGIS or R sf
  )

plots <- df %>%
  transmute(
    plot_name,         
    site_id = 1,
    plot_route,
    waypoint_number_beg = paste(gps, waypoint_number_beg, sep = "_"),
    beg_northing,
    beg_easting,
    beg_accuracy_ft,
    waypoint_number_end  = paste(gps, waypoint_number_end, sep = "_"),
    end_northing,
    end_easting,
    #end_accuracy_ft      = end_accuracy,
    plot_elevation_m,
    transect_length_m,
    transect_field_width_m,
    azimuth,
    slope_beg,
    slope_end,
    aspect_beg,
    aspect_end,
    avg_slope = (slope_beg + slope_end) / 2,
    avg_aspect = (aspect_beg + aspect_end) / 2,
    target_species_id,
    dominant_species_id = species1,
    associated_tree_species,
    #other_associated_species,
    #soil_types,
    #percent_rock,
    #alternate_hosts,
    general_plot_notes = general_plot_observations,
    general_vegetation_notes = general_vegetation_observations
    # geom will be added later in PostGIS or R sf
  )

# Cleaning and plot area calculations
plots <- plots %>%
  mutate(
    # Handle asymmetric widths by coercing to numeric then cleaning up
    transect_field_width_m = as.numeric(transect_field_width_m),
    # Define overall plot width
    transect_width_m = case_when(
      plot_name == "PIMO_EX2_PIBA1c_KernPlateau" ~ 10 + 28, # asymmetrical plot
      TRUE ~ transect_field_width_m * 2),
    # Compute design area 
    design_area_m2 = case_when(
      !is.na(transect_length_m) & !is.na(transect_width_m) ~ transect_length_m * transect_width_m, 
      TRUE ~ NA_real_
    )
  )

# ---- Step 4: Write clean CSVs ----
# static plot table
write_csv(plots, "data/clean/plots_clean.csv")
# dynamic plot table
write_csv(plot_visit, "data/clean/plot_visit_clean.csv")

# ---- Step 5: (Optional) Write to Postgres directly ----
# library(RPostgres)
# con <- DBI::dbConnect(RPostgres::Postgres(),
#                       dbname   = "phd_project",
#                       host     = "localhost",
#                       user     = "your_username",
#                       password = "your_password")
#
# DBI::dbWriteTable(con, "phd_project.plot", df_clean,
#                   row.names = FALSE, append = TRUE)
#
# DBI::dbDisconnect(con)
