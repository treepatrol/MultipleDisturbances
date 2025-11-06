# ===============================
# Clean & Export Plot Table to PostgresSQL Database
# ===============================

library(dplyr)
library(janitor)   # for clean_names()
library(readr)     # for read_csv() and write_csv()
library(stringr)  # for data cleaning requiring string detection

# ---- Step 1: Read raw data ----
# plot data
raw <- read_csv('/Users/jennifercribbs/Documents/TreePatrol.org/Analysis/SEKIanalysis/SEKI_all_PlotData_20250821.csv')

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
    plot_name,         
    site_id              = 1,
    date                 = lubridate::make_date(year, month, day),
    crew                 = crew,
    plot_route,
    waypoint_number_beg  = paste(gps, waypoint_number_beg, sep = "_"),
    beg_northing,
    beg_easting,
    beg_accuracy_ft,
    waypoint_number_end  = paste(gps, waypoint_number_end, sep = "_"),
    end_northing,
    end_easting,
    end_accuracy_ft      = end_accuracy,
    transect_length_m,
    transect_field_width_m,
    azimuth,
    slope_beg,
    slope_end,
    aspect_beg,
    aspect_end,
    plot_elevation_m,
    associated_tree_species,
    other_associated_species,
    soil_types,
    percent_rock,
    alternate_hosts,
    general_plot_notes = general_plot_observations,
    general_vegetation_notes = general_vegetation_observations
    # geom will be added later in PostGIS or R sf
  )

# Cleaning and plot area calculations
df_clean <- df_clean %>%
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
