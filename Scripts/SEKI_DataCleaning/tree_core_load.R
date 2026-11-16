library (dplyr)
library(DBI)
library(RPostgres)

# read in full core inventory
cores <- read_csv(
  here("Data", "core_inventory_updated20261115.csv")
)
# check for 817 cores
nrow(cores)

# check for repeated Tucson IDs
cores %>%
  filter(!is.na(tucson_id), trimws(tucson_id) != "") %>%
  count(tucson_id, name = "n") %>%
  filter(n > 1)

# check for repeated cores
cores %>%
  count(
    plot_id, tree_number, species, core,
    year, month, day,
    name = "n"
  ) %>%
  filter(n > 1)

# connect to the database to look up tree visit dates
con <- dbConnect(
  RPostgres::Postgres(),
  dbname = "phd_project"
)

visit_lookup <- dbGetQuery(
  con,
  "
  SELECT
      t.plot_id,
      t.tree_number,
      s.species_code AS species,
      tv.tree_visit_id,
      tv.plot_visit_id,
      pv.start_date AS visit_date
  FROM forest_health.tree AS t
  JOIN forest_health.species AS s
      ON t.species_id = s.species_id
  JOIN forest_health.tree_visit AS tv
      ON tv.tree_id = t.tree_id
  JOIN forest_health.plot_visit AS pv
      ON tv.plot_visit_id = pv.plot_visit_id
  WHERE pv.plot_id = t.plot_id;
  "
)

dbDisconnect(con)

# Count available observations for each tree
visit_counts <- visit_lookup %>%
  count(plot_id, tree_number, species, name = "n_visits")

# Attach counts without multiplying inventory rows
cores_visit_check <- cores %>%
  left_join(
    visit_counts,
    by = c("plot_id", "tree_number", "species"),
    relationship = "many-to-one"
  )

# Summary: missing matches, single visits, and multiple visits
cores_visit_check %>%
  count(n_visits, name = "n_cores")

cores_visit_check %>%
  filter(is.na(n_visits)) %>%
  print(n = Inf, width = Inf)

# check two trees with weird species
con <- dbConnect(
  RPostgres::Postgres(),
  dbname = "phd_project"
)

uncertain_trees <- dbGetQuery(
  con,
  "
  SELECT
      t.tree_id,
      t.plot_id,
      t.tree_number,
      s.species_code,
      tv.tree_visit_id,
      tv.plot_visit_id,
      pv.start_date
  FROM forest_health.tree AS t
  LEFT JOIN forest_health.species AS s
      ON t.species_id = s.species_id
  LEFT JOIN forest_health.tree_visit AS tv
      ON t.tree_id = tv.tree_id
  LEFT JOIN forest_health.plot_visit AS pv
      ON tv.plot_visit_id = pv.plot_visit_id
  WHERE t.plot_id = 21
    AND t.tree_number IN (2, 4)
  ORDER BY t.tree_number, pv.start_date;
  "
)

dbDisconnect(con)

as_tibble(uncertain_trees) %>%
  print(n = Inf)

# fixed an uncertain species issue using update in SQL

# update to include tree id
con <- dbConnect(
  RPostgres::Postgres(),
  dbname = "phd_project"
)

visit_lookup <- dbGetQuery(
  con,
  "
  SELECT
      t.tree_id,
      t.plot_id,
      t.tree_number,
      s.species_code AS species,
      tv.tree_visit_id,
      tv.plot_visit_id,
      pv.start_date AS visit_date
  FROM forest_health.tree AS t
  JOIN forest_health.species AS s
      ON t.species_id = s.species_id
  JOIN forest_health.tree_visit AS tv
      ON tv.tree_id = t.tree_id
  JOIN forest_health.plot_visit AS pv
      ON tv.plot_visit_id = pv.plot_visit_id
  WHERE pv.plot_id = t.plot_id;
  "
)

dbDisconnect(con)

# One identity row per database tree
tree_lookup <- visit_lookup %>%
  distinct(plot_id, tree_number, species, tree_id)

# Match ordinary species entries, then handle the two exceptions
cores_matched <- cores %>%
  left_join(
    tree_lookup,
    by = c("plot_id", "tree_number", "species"),
    relationship = "many-to-one"
  ) %>%
  mutate(
    tree_id = case_when(
      plot_id == 21 & tree_number == 2 ~ 1187L,
      plot_id == 21 & tree_number == 4 ~ 1189L,
      TRUE ~ tree_id
    )
  )

stopifnot(
  nrow(cores_matched) == nrow(cores),
  !anyNA(cores_matched$tree_id)
)

# Count available visits using the resolved tree identity
visit_counts <- visit_lookup %>%
  count(tree_id, name = "n_visits")

cores_visit_check <- cores_matched %>%
  left_join(
    visit_counts,
    by = "tree_id",
    relationship = "many-to-one"
  )

cores_visit_check %>%
  count(n_visits, name = "n_cores")

cores_visit_check %>%
  filter(n_visits == 2) %>%
  select(
    plot_id, tree_number, species, core,
    year, month, day, tree_id
  ) %>%
  left_join(
    visit_lookup %>%
      select(tree_id, tree_visit_id, plot_visit_id, visit_date),
    by = "tree_id",
    relationship = "many-to-many"
  ) %>%
  arrange(plot_id, tree_number, core, visit_date) %>%
  print(n = Inf)

# checks
# Assign a temporary ID to each inventory row
core_candidates <- cores_matched %>%
  mutate(inventory_row = row_number()) %>%
  left_join(
    visit_lookup %>%
      select(
        tree_id, tree_visit_id, plot_visit_id, visit_date
      ),
    by = "tree_id",
    relationship = "many-to-many"
  ) %>%
  mutate(
    visit_year = lubridate::year(visit_date)
  )

# Keep candidates consistent with the recorded collection year
core_candidates <- core_candidates %>%
  filter(is.na(year) | year == visit_year)

# Every inventory row must have exactly one matching visit
match_check <- cores_matched %>%
  mutate(inventory_row = row_number()) %>%
  select(inventory_row, plot_id, tree_number, species, core, year) %>%
  left_join(
    core_candidates %>%
      count(inventory_row, name = "n_matches"),
    by = "inventory_row"
  ) %>%
  mutate(n_matches = coalesce(n_matches, 0L))

match_check %>%
  filter(n_matches != 1L) %>%
  print(n = Inf)

stopifnot(all(match_check$n_matches == 1L))

# One resolved row per core
cores_resolved <- core_candidates %>%
  arrange(inventory_row)

stopifnot(nrow(cores_resolved) == nrow(cores_matched))

# final checks and create resolved object
stopifnot(all(match_check$n_matches == 1L))

cores_resolved <- core_candidates %>%
  arrange(inventory_row)

stopifnot(
  nrow(cores_resolved) == nrow(cores),
  nrow(cores_resolved) == 817L
)
cores_resolved %>%
  count(tree_visit_id, core, name = "n") %>%
  filter(n > 1)
names(cores_resolved)

# ---------------------------------------------------------
# Prepare reviewed inventory for the core-table upload
# ---------------------------------------------------------

cores_upload <- cores_resolved %>%
  transmute(
    tree_visit_id,
    tucson_id = na_if(trimws(as.character(tucson_id)), ""),
    core_label = as.character(core),
    
    # Preserve the original core-level entries
    core_height = as.character(core_height_raw),
    core_dbh = as.character(core_dbh_raw),
    core_depth = as.character(core_depth),
    
    field_notes = as.character(notes),
    straw_label = as.character(straw_label),
    straw_dbh = as.character(straw_dbh),
    physical_pieces,
    inventory_notes = as.character(inventory_notes)
  )

# Check piece counts before converting to integer
stopifnot(
  nrow(cores_upload) == 817L,
  !anyNA(cores_upload$tree_visit_id),
  !anyNA(cores_upload$core_label),
  all(nzchar(trimws(cores_upload$core_label))),
  anyDuplicated(
    cores_upload[c("tree_visit_id", "core_label")]
  ) == 0L,
  all(
    is.na(cores_upload$physical_pieces) |
      (
        cores_upload$physical_pieces >= 0 &
          cores_upload$physical_pieces ==
          floor(cores_upload$physical_pieces)
      )
  )
)

cores_upload <- cores_upload %>%
  mutate(physical_pieces = as.integer(physical_pieces))

# Review before inserting
cores_upload %>%
  select(
    tree_visit_id, core_label, core_height,
    core_dbh, straw_dbh, physical_pieces, inventory_notes
  ) %>%
  print(n = 20, width = Inf)


# ---------------------------------------------------------
# Load the reviewed core inventory
# ---------------------------------------------------------

load_core_inventory <- function(cores_upload) {
  con <- dbConnect(
    RPostgres::Postgres(),
    dbname = "phd_project"
  )
  on.exit(dbDisconnect(con), add = TRUE)
  
  dbWithTransaction(con, {
    # This script is for the initial load only
    existing <- dbGetQuery(
      con,
      "SELECT count(*) AS n
       FROM forest_health.tree_core"
    )
    
    if (existing$n != 0) {
      stop("tree_core is not empty; nothing was inserted.")
    }
    
    stopifnot(
      nrow(cores_upload) == 817L,
      !anyNA(cores_upload$tree_visit_id),
      anyDuplicated(
        cores_upload[c("tree_visit_id", "core_label")]
      ) == 0L
    )
    
    # PostgreSQL generates tree_core_id.
    # Omitted lab fields remain NULL.
    dbAppendTable(
      con,
      Id(schema = "forest_health", table = "tree_core"),
      cores_upload
    )
    
    # Read back and compare all uploaded fields
    loaded <- dbGetQuery(
      con,
      "SELECT *
       FROM forest_health.tree_core"
    ) %>%
      as_tibble()
    
    actual <- loaded %>%
      select(all_of(names(cores_upload)))
    
    stopifnot(
      nrow(loaded) == 817L,
      !anyNA(loaded$tree_core_id),
      anyDuplicated(loaded$tree_core_id) == 0L,
      anyDuplicated(
        loaded[c("tree_visit_id", "core_label")]
      ) == 0L,
      nrow(anti_join(
        cores_upload, actual,
        by = names(cores_upload),
        na_matches = "na"
      )) == 0L,
      nrow(anti_join(
        actual, cores_upload,
        by = names(cores_upload),
        na_matches = "na"
      )) == 0L
    )
    
    loaded
  })
}

cores_loaded <- load_core_inventory(cores_upload)

cores_loaded %>%
  select(
    tree_core_id, tree_visit_id, tucson_id,
    core_label, core_height, core_dbh, physical_pieces
  ) %>%
  print(n = 20, width = Inf)
