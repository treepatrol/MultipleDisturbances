# ===============================
# Template: Clean & Export Table
# ===============================

library(dplyr)
library(janitor)   # for clean_names()
library(readr)     # for read_csv() and write_csv()

# ---- Step 1: Read raw data ----
# Replace with your file path
raw <- read_csv("data/raw/plots_2024.csv")

# ---- Step 2: Clean column names ----
# Converts to snake_case and strips weird characters
df <- raw %>%
  janitor::clean_names()

# ---- Step 3: Standardize columns ----
# Example: select only the columns you need,
# rename them to match schema, and order correctly

df_clean <- df %>%
  transmute(
    # match schema order exactly
    plot_name            = plotid,         # or whatever the raw col was called
    site_id              = sitecode,
    date                 = lubridate::ymd(surveydate),
    crew                 = crew,
    plot_route           = route,
    waypoint_number_beg  = wp_start,
    beg_northing         = northing_start,
    beg_easting          = easting_start,
    beg_accuracy_ft      = accuracy_start,
    waypoint_number_end  = wp_end,
    end_northing         = northing_end,
    end_easting          = easting_end,
    end_accuracy_ft      = accuracy_end,
    transect_length_m    = tran_length,
    transect_width_m     = tran_width,
    azimuth              = azimuth,
    slope_beg            = slope_start,
    slope_end            = slope_end,
    aspect_beg           = aspect_start,
    aspect_end           = aspect_end,
    plot_elevation_m     = elevation,
    associated_tree_species = assoc_species,
    other_associated_species = other_species,
    soil_types           = soil,
    percent_rock         = pct_rock,
    alternate_hosts      = hosts,
    general_plot_notes   = plot_notes,
    general_vegetation_notes = veg_notes
    # geom will be added later in PostGIS or R sf
  )

# ---- Step 4: Write clean CSV ----
write_csv(df_clean, "data/clean/plots_clean.csv")

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
