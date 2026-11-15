library(tidyverse)
library(janitor)
library(here)
library(DBI)
library(RPostgres)

# read the PICO8b_TwinLakes sheet
pico8b_raw <- read_csv(
  here("Data", "RawData", "SEKI_Data", "PICO8b_TwinLakes.csv"),
  show_col_types = FALSE
) %>%
  clean_names()

# check for 1 row per tree
pico8b_tree <- pico8b_raw %>%
  transmute(
    plot_id = 50L,
    tree_number,
    species_code = str_to_upper(str_trim(species))
  ) %>%
  distinct()

# Check identities before connecting
stopifnot(
  nrow(pico8b_tree) == 22L,
  !anyNA(pico8b_tree),
  all(pico8b_tree$tree_number > 0),
  all(pico8b_tree$tree_number == floor(pico8b_tree$tree_number)),
  anyDuplicated(pico8b_tree$tree_number) == 0L,
  all(pico8b_tree$species_code %in% c("PICO", "PIMO"))
)

pico8b_tree <- pico8b_tree %>%
  mutate(
    tree_number = as.integer(tree_number),
    species_id = case_when(
      species_code == "PICO" ~ 3L,
      species_code == "PIMO" ~ 5L
    )
  )

# Review before running the database section below
pico8b_tree %>% count(species_code)
print(pico8b_tree, n = Inf)

# Load 22 trees from PICO8b Twin Lakes
load_pico8b_trees <- function(add_trees) {
  con <- dbConnect(
    RPostgres::Postgres(),
    dbname = "phd_project"
  )
  on.exit(dbDisconnect(con), add = TRUE)
  
  dbWithTransaction(con, {
    # Check the input belongs to this plot
    stopifnot(
      nrow(add_trees) == 22L,
      !anyNA(add_trees),
      all(add_trees$plot_id == 50L),
      anyDuplicated(add_trees$tree_number) == 0L
    )
    
    # Confirm the intended plot exists
    plot_check <- dbGetQuery(
      con,
      "SELECT plot_id, plot_name
       FROM forest_health.plot
       WHERE plot_id = 50"
    )
    
    stopifnot(
      nrow(plot_check) == 1L,
      str_detect(plot_check$plot_name, fixed("TwinLakes"))
    )
    
    # Stop if this plot already has tree identities
    existing <- dbGetQuery(
      con,
      "SELECT tree_id
       FROM forest_health.tree
       WHERE plot_id = 50"
    )
    
    if (nrow(existing) > 0L) {
      stop("Plot 50 already has trees; nothing was inserted.")
    }
    
    expected <- add_trees %>%
      select(plot_id, tree_number, species_id)
    
    # PostgreSQL generates tree_id
    dbAppendTable(
      con,
      Id(schema = "forest_health", table = "tree"),
      expected
    )
    
    # Verify before committing
    loaded <- dbGetQuery(
      con,
      "SELECT tree_id, plot_id, tree_number, species_id
       FROM forest_health.tree
       WHERE plot_id = 50
       ORDER BY tree_number"
    )
    
    actual <- loaded %>%
      select(plot_id, tree_number, species_id)
    
    stopifnot(
      nrow(loaded) == 22L,
      anyDuplicated(loaded$tree_number) == 0L,
      nrow(anti_join(
        expected, actual, by = names(expected)
      )) == 0L,
      nrow(anti_join(
        actual, expected, by = names(expected)
      )) == 0L
    )
    
    as_tibble(loaded)
  })
}

pico8b_loaded <- load_pico8b_trees(pico8b_tree)
print(pico8b_loaded, n = Inf)

pico8b_raw %>%
  count(year, month, day)

pico8b_raw %>%
  count(wpbr, sort = TRUE)

pico8b_raw %>%
  select(p_dead_rg, p_dead_full, fire) %>%
  distinct() %>%
  print(n = Inf)

# ---------------------------------------------------------
# Prepare PICO8b tree observations
# ---------------------------------------------------------

# Stop rather than silently discard non-numeric entries
number <- function(x) {
  x <- str_trim(as.character(x))
  x[x %in% c("", "NA", "N/A")] <- NA_character_
  
  result <- suppressWarnings(as.numeric(x))
  bad <- !is.na(x) & is.na(result)
  
  if (any(bad)) {
    stop("Review numeric entries: ",
         paste(unique(x[bad]), collapse = ", "))
  }
  
  result
}

# Check the WPBR notation before applying this batch-specific mapping
wpbr_text <- str_squish(as.character(pico8b_raw$wpbr))
wpbr_text[wpbr_text == ""] <- NA_character_

stopifnot(
  nrow(pico8b_raw) == 22L,
  sum(is.na(wpbr_text)) == 21L,
  sum(wpbr_text == "1 BoC, flags", na.rm = TRUE) == 1L,
  all(is.na(number(pico8b_raw$p_dead_full)))
)

pico8b_visits <- pico8b_raw %>%
  transmute(
    tree_number = number(tree_number),
    species_code = str_to_upper(str_trim(species)),
    observation_date = lubridate::make_date(
      number(year), number(month), number(day)
    ),
    plot_visit_id = 50L,
    
    canopy_position = as.character(canopy_position),
    dbh = number(dbh),
    height = number(height),
    beetles = as.character(beetles),
    tree_visit_notes = as.character(notes),
    
    tree_waypoint_number = as.character(waypoint_number),
    tree_northing = number(northing),
    tree_easting = number(easting),
    tree_accuracy_ft = number(position_accuracy_ft),
    slope_tree = number(slope),
    aspect_tree = number(
      if_else(
        str_to_lower(str_trim(as.character(aspect))) == "flat",
        NA_character_,
        as.character(aspect)
      )
    ),
    tree_elevation_m = number(elevation_m),
    
    # No other cankers observed; one bole canker has unknown activity
    wpbr_active_branch = 0,
    wpbr_inactive_branch = 0,
    wpbr_active_bole = 0,
    wpbr_inactive_bole = 0,
    wpbr_unspecified_branch = 0L,
    
    wpbr_unspecified_bole = if_else(
      coalesce(wpbr_text == "1 BoC, flags", FALSE),
      1L,
      0L
    ),
    
    # Flags were noted on one tree but not counted
    flags = if_else(
      coalesce(wpbr_text == "1 BoC, flags", FALSE),
      NA_integer_,
      0L
    ),
    
    wpbr_extra_notes = wpbr_text,
    
    # Single percent-dead assessment, recorded in red/green style.
    # Use the agreed legacy label and store the value in p_dead.
    p_dead = number(p_dead_rg),
    assessment_protocol = "legacy_2022"
  )

stopifnot(
  !anyNA(pico8b_visits$tree_number),
  all(pico8b_visits$tree_number > 0),
  all(
    pico8b_visits$tree_number ==
      floor(pico8b_visits$tree_number)
  ),
  anyDuplicated(pico8b_visits$tree_number) == 0L,
  !anyNA(pico8b_visits$observation_date),
  n_distinct(pico8b_visits$observation_date) == 1L,
  all(
    is.na(pico8b_visits$p_dead) |
      between(pico8b_visits$p_dead, 0, 100)
  )
)

pico8b_visits <- pico8b_visits %>%
  mutate(tree_number = as.integer(tree_number))

# Review before running the loading section
print(pico8b_visits, n = Inf, width = Inf)

# ---------------------------------------------------------
# Load PICO8b observations into tree_visit
# ---------------------------------------------------------

load_pico8b_visits <- function(observations) {
  con <- dbConnect(
    RPostgres::Postgres(),
    dbname = "phd_project"
  )
  on.exit(dbDisconnect(con), add = TRUE)
  
  dbWithTransaction(con, {
    # Retrieve the existing tree identities
    trees <- dbGetQuery(
      con,
      "SELECT tree_id, tree_number, species_id
       FROM forest_health.tree
       WHERE plot_id = 50"
    )
    
    # Confirm visit 50 belongs to plot 50
    visit <- dbGetQuery(
      con,
      "SELECT plot_visit_id, plot_id, start_date
       FROM forest_health.plot_visit
       WHERE plot_visit_id = 50"
    )
    
    stopifnot(
      nrow(observations) == 22L,
      nrow(trees) == 22L,
      anyDuplicated(trees$tree_number) == 0L,
      nrow(visit) == 1L,
      visit$plot_id == 50L,
      !anyNA(observations$observation_date),
      all(observations$plot_visit_id == 50L),
      all(observations$observation_date == visit$start_date)
    )
    
    # Match observations to existing trees
    matched <- observations %>%
      left_join(
        trees,
        by = "tree_number",
        relationship = "many-to-one"
      ) %>%
      mutate(
        expected_species_id = case_when(
          species_code == "PICO" ~ 3L,
          species_code == "PIMO" ~ 5L,
          TRUE ~ NA_integer_
        )
      )
    
    stopifnot(
      nrow(matched) == 22L,
      !anyNA(matched$tree_id),
      !anyNA(matched$expected_species_id),
      all(matched$species_id == matched$expected_species_id),
      anyDuplicated(
        matched[c("tree_id", "plot_visit_id")]
      ) == 0L
    )
    
    # Stop if observations have already been loaded
    existing <- dbGetQuery(
      con,
      "SELECT tree_visit_id
       FROM forest_health.tree_visit
       WHERE plot_visit_id = 50"
    )
    
    if (nrow(existing) > 0L) {
      stop("Visit 50 already has tree observations; nothing inserted.")
    }
    
    # Keep only destination-table attributes
    to_insert <- matched %>%
      select(
        -tree_number,
        -species_code,
        -observation_date,
        -species_id,
        -expected_species_id
      )
    
    # PostgreSQL generates tree_visit_id
    dbAppendTable(
      con,
      Id(schema = "forest_health", table = "tree_visit"),
      to_insert
    )
    
    # Verify the stored values before committing
    loaded <- dbGetQuery(
      con,
      "SELECT *
       FROM forest_health.tree_visit
       WHERE plot_visit_id = 50"
    )
    
    actual <- loaded %>%
      select(all_of(names(to_insert)))
    
    stopifnot(
      nrow(loaded) == 22L,
      anyDuplicated(loaded$tree_id) == 0L,
      nrow(anti_join(
        to_insert, actual,
        by = names(to_insert),
        na_matches = "na"
      )) == 0L,
      nrow(anti_join(
        actual, to_insert,
        by = names(to_insert),
        na_matches = "na"
      )) == 0L
    )
    
    loaded %>%
      as_tibble() %>%
      left_join(
        trees %>% select(tree_id, tree_number),
        by = "tree_id",
        relationship = "many-to-one"
      ) %>%
      select(
        tree_visit_id, tree_number, tree_id, plot_visit_id,
        dbh, height, p_dead, wpbr_extra_notes,
        wpbr_unspecified_bole, flags
      ) %>%
      arrange(tree_number)
  })
}

# Define the function above first, then run the import
pico8b_visits_loaded <- load_pico8b_visits(pico8b_visits)

print(pico8b_visits_loaded, n = Inf)

