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
if (load_tree) {
  # load tree.csv
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