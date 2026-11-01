library(tidyverse)
library(janitor)
library(here)
library(DBI)
library(RPostgres)

# Read the Alta Peak sheet
alta_raw <- read_csv(
  here("Data", "RawData", "SEKI_Data", "AltaPeak.csv"),
  show_col_types = FALSE
) %>%
  clean_names()

# Use tree identity fields only.
# The same PIMOs appearing in 2022 and 2026 become one row each.
alta_tree <- alta_raw %>%
  transmute(
    plot_id = 52L,
    tree_number,
    species_code = str_to_upper(str_trim(species))
  ) %>%
  distinct()

# Check identities before connecting
stopifnot(
  nrow(alta_tree) == 20L,
  !anyNA(alta_tree),
  all(alta_tree$tree_number > 0),
  all(alta_tree$tree_number == floor(alta_tree$tree_number)),
  anyDuplicated(alta_tree$tree_number) == 0L,
  all(alta_tree$species_code %in% c("PIBA", "PIMO"))
)

alta_tree <- alta_tree %>%
  mutate(
    tree_number = as.integer(tree_number),
    species_id = case_when(
      species_code == "PIBA" ~ 2L,
      species_code == "PIMO" ~ 5L
    )
  )

# Review before running the database section below
alta_tree %>% count(species_code)
print(alta_tree, n = Inf)

# Load 20 trees from Alta Peak
load_alta_trees <- function(alta_tree) {
  con <- dbConnect(
    RPostgres::Postgres(),
    dbname = "phd_project"
  )
  on.exit(dbDisconnect(con), add = TRUE)
  
  dbWithTransaction(con, {
    # Confirm the intended plot exists
    plot_check <- dbGetQuery(
      con,
      "SELECT plot_id, plot_name
       FROM forest_health.plot
       WHERE plot_id = 52"
    )
    
    stopifnot(
      nrow(plot_check) == 1L,
      str_detect(plot_check$plot_name, fixed("AltaPeak"))
    )
    
    # This is a first-load script: stop if the plot already has trees
    existing <- dbGetQuery(
      con,
      "SELECT tree_id
       FROM forest_health.tree
       WHERE plot_id = 52"
    )
    
    if (nrow(existing) > 0L) {
      stop("Plot 52 already has trees; nothing was inserted.")
    }
    
    # Omit tree_id so PostgreSQL generates it.
    # Geometry and tree notes are left NULL.
    dbAppendTable(
      con,
      Id(schema = "forest_health", table = "tree"),
      alta_tree %>%
        select(plot_id, tree_number, species_id)
    )
    
    # Verify the inserted identities before committing
    loaded <- dbGetQuery(
      con,
      "SELECT tree_id, plot_id, tree_number, species_id
       FROM forest_health.tree
       WHERE plot_id = 52
       ORDER BY tree_number"
    )
    
    expected <- alta_tree %>%
      select(plot_id, tree_number, species_id)
    
    actual <- loaded %>%
      select(plot_id, tree_number, species_id)
    
    stopifnot(
      nrow(loaded) == 20L,
      nrow(anti_join(expected, actual,
                     by = names(expected))) == 0L,
      nrow(anti_join(actual, expected,
                     by = names(expected))) == 0L
    )
    
    loaded
  })
}

alta_loaded <- load_alta_trees(alta_tree)
print(alta_loaded, n = Inf)
