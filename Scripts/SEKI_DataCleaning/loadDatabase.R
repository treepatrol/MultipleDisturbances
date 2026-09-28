library(tidyverse)
library(DBI)
library(RPostgres)

con <- dbConnect(
  Postgres(),
  dbname = "phd_project",
  host = "localhost",
  port = 5432,
  user = "jennifercribbs"
)

# --- Choose Tables To Load ----
load_plot <- FALSE
load_plot_visit <- FALSE

load_tree <- TRUE
load_tree_visit <- FALSE
load_tree_core <- FALSE

# --- Load Plot Table ----

# --- Load Plot Visit Table ----

# --- Load Tree Table ----



# if load tree is TRUE above
if (load_tree) {
# load tree data into R evironment  
  tree <- read_csv(
    "/Users/jennifercribbs/Documents/R-Projects/MultipleDisturbances/Data/CleanData/treeData.csv"
  )
# write it to Postgres database  
  dbWriteTable(
    con,
    Id(schema = "forest_health", table = "tree"),
    tree,
    append = TRUE
  )
# check the number of rows  
  n_db <- dbGetQuery(
    con,
    "SELECT COUNT(*) AS n FROM forest_health.tree"
  )
  
  print(n_db)
}

# --- Load Tree Visit Table ----
if (load_tree_visit) {
  # load tree_visit.csv
}

# --- Load Tree Core Table ----
if (load_tree_core) {
  # load tree_core.csv
}

# validation checks

# close database connection when done
dbDisconnect(con)