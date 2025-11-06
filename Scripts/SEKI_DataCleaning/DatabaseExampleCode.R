## Example plot table import script
# Authors: Jenny Cribbs & Chat GPT

library(tidyverse)
library(dplyr)
library(janitor)
library(DBI)
library(RPostgres)

# 1️ Read your SEKI plot Excel file
seki_raw <- read_csv("path/to/SEKI_plot_data.xlsx", sheet = "Plot") %>%
  clean_names() # converts to snake_case

# 2️ Check column names and adjust if needed
glimpse(seki_raw)

# Example renaming if needed:
seki_clean <- seki_raw %>%
  rename(
    plot_name = plotid,    # if your Excel column is "PlotID"
    crew = crew_names,     # adjust to match your DB
    # Add additional renames here
  ) %>%
  mutate(
    site_id = 1,  # assuming SEKI = 1 in your site table
    date = as.Date(date)   # ensure correct date type
  )

# 3️ Connect to your PostgreSQL database
con <- dbConnect(
  RPostgres::Postgres(),
  dbname = "your_database_name",
  host = "localhost",
  port = 5432,
  user = "your_username",
  password = "your_password"
)

# 4️ Write to the "plot" table in your schema
dbWriteTable(
  con,
  name = "plot",
  value = seki_clean,
  append = TRUE,   # add data to existing table
  row.names = FALSE
)

# 5️ Verify import
dbGetQuery(con, "SELECT COUNT(*) FROM plot;")
dbGetQuery(con, "SELECT * FROM plot LIMIT 5;")

# 6️ Disconnect when finished
dbDisconnect(con)
