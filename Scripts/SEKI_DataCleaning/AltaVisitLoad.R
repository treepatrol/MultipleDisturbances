library(tidyverse)
library(janitor)
library(here)
library(DBI)
library(RPostgres)

# ---------------------------------------------------------
# 1. Read Alta Peak observations
# ---------------------------------------------------------

# Read as text so unexpected entries can be checked explicitly
alta_raw <- read_csv(
  here("Data", "RawData", "SEKI_Data", "AltaPeak.csv"),
  col_types = cols(.default = col_character()),
  na = c("", "NA", "N/A"),
  show_col_types = FALSE
) %>%
  clean_names()

# Strict numeric conversion: stop rather than silently lose text
number <- function(x) {
  result <- suppressWarnings(as.numeric(x))
  
  if (any(!is.na(x) & is.na(result))) {
    stop(
      "Unexpected numeric entry: ",
      paste(unique(x[!is.na(x) & is.na(result)]), collapse = ", ")
    )
  }
  
  result
}

# Translate explicit fire observations; blanks remain unknown
fire_value <- function(x) {
  x <- str_to_lower(str_trim(x))
  
  unexpected <- !is.na(x) &
    !x %in% c("none", "no", "false", "0", "yes", "true", "1")
  
  if (any(unexpected)) {
    stop(
      "Review fire entries before loading: ",
      paste(unique(x[unexpected]), collapse = ", ")
    )
  }
  
  case_when(
    x %in% c("none", "no", "false", "0") ~ FALSE,
    x %in% c("yes", "true", "1") ~ TRUE,
    TRUE ~ NA
  )
}

# This batch was recorded with no WPBR symptoms.
# Stop if a symptom entry appears rather than bypassing its parsing.
stopifnot(
  nrow(alta_raw) == 22L,
  all(!is.na(alta_raw$wpbr)),
  all(str_to_lower(str_trim(alta_raw$wpbr)) == "none"),
  all(is.na(alta_raw$p_dead_full))
)

# ---------------------------------------------------------
# 2. Prepare visit attributes
# ---------------------------------------------------------

alta_visits <- alta_raw %>%
  transmute(
    tree_number = number(tree_number),
    species_code = str_to_upper(str_trim(species)),
    observation_date = lubridate::make_date(
      number(year), number(month), number(day)
    ),
    
    plot_visit_id = case_when(
      number(year) == 2022 ~ 52L,
      number(year) == 2026 ~ 59L,
      TRUE ~ NA_integer_
    ),
    
    canopy_position,
    dbh = number(dbh),
    height = number(height),
    fire_notes = fire,
    fire = fire_value(fire),
    beetles,
    tree_visit_notes = notes,
    tree_waypoint_number = waypoint_number,
    tree_northing = number(northing),
    tree_easting = number(easting),
    tree_accuracy_ft = number(position_accuracy_ft),
    slope_tree = number(slope),
    aspect_tree = number(
      if_else(
        str_to_lower(str_trim(aspect)) == "flat",
        NA_character_,
        aspect
      )
    ),
    tree_elevation_m = number(elevation_m),
    
    # Explicit "none" observations for this batch
    wpbr_active_branch = 0,
    wpbr_inactive_branch = 0,
    wpbr_active_bole = 0,
    wpbr_inactive_bole = 0,
    wpbr_unspecified_branch = 0L,
    wpbr_unspecified_bole = 0L,
    flags = 0L,
    wpbr_extra_notes = wpbr,
    
    # Single percent-dead assessment, despite source column header.
    # legacy_2022 denotes the method, including its use in 2026.
    p_dead = number(p_dead_rg),
    assessment_protocol = "legacy_2022"
  )

# ---------------------------------------------------------
# Add the 2026 bole-char assessment for all 20 trees
# ---------------------------------------------------------

# Add observed zero char to the two fully measured 2026 trees
alta_visits <- alta_visits %>%
  mutate(
    bole_char = if_else(plot_visit_id == 59L, 0, NA_real_),
    
    tree_visit_notes = if_else(
      plot_visit_id == 59L,
      str_c(
        coalesce(tree_visit_notes, ""),
        "Bole char assessed in 2026: none observed.",
        sep = " | "
      ),
      tree_visit_notes
    )
  )

# Identify the other 18 trees, using their 2022 identities
char_only <- alta_visits %>%
  filter(plot_visit_id == 52L) %>%
  select(tree_number, species_code) %>%
  anti_join(
    alta_visits %>%
      filter(plot_visit_id == 59L) %>%
      select(tree_number),
    by = "tree_number"
  ) %>%
  mutate(
    observation_date = as.Date("2026-08-07"),
    plot_visit_id = 59L,
    bole_char = 0,
    tree_visit_notes = paste(
      "Bole char assessed in 2026: none observed.",
      "No other tree attributes recorded during this revisit."
    )
  )

stopifnot(nrow(char_only) == 18L)

# Missing columns in char_only become NA.
# This leaves WPBR, percent dead, DBH, etc. unknown for those 18.
alta_visits <- bind_rows(alta_visits, char_only)

# ---------------------------------------------------------
# Check the completed 40-row dataset
# ---------------------------------------------------------

# Every observation must identify a tree, date, and visit
stopifnot(
  !anyNA(alta_visits$tree_number),
  !anyNA(alta_visits$species_code),
  !anyNA(alta_visits$observation_date),
  !anyNA(alta_visits$plot_visit_id),
  all(alta_visits$tree_number > 0),
  all(alta_visits$tree_number == floor(alta_visits$tree_number))
)

# Twenty unique trees, each with one observation in each year
stopifnot(
  nrow(alta_visits) == 40L,
  n_distinct(alta_visits$tree_number) == 20L,
  sum(alta_visits$plot_visit_id == 52L) == 20L,
  sum(alta_visits$plot_visit_id == 59L) == 20L,
  anyDuplicated(
    alta_visits[c("tree_number", "plot_visit_id")]
  ) == 0L
)

# Char was unrecorded in 2022 and observed as zero in 2026
stopifnot(
  all(is.na(
    alta_visits$bole_char[alta_visits$plot_visit_id == 52L]
  )),
  all(
    alta_visits$bole_char[alta_visits$plot_visit_id == 59L] == 0
  )
)

# Recorded percent-dead values must be between 0 and 100
stopifnot(
  all(
    is.na(alta_visits$p_dead) |
      between(alta_visits$p_dead, 0, 100)
  )
)

# Only trees 16 and 17 received detailed measurements in 2026
measured_2026 <- alta_visits %>%
  filter(
    plot_visit_id == 59L,
    assessment_protocol == "legacy_2022"
  )

stopifnot(
  nrow(measured_2026) == 2L,
  setequal(measured_2026$tree_number, c(16, 17)),
  all(measured_2026$species_code == "PIMO")
)

alta_visits <- alta_visits %>%
  mutate(tree_number = as.integer(tree_number))

# Review counts and observations before loading
alta_visits %>%
  group_by(plot_visit_id) %>%
  summarise(
    observations = n(),
    recorded_dbh = sum(!is.na(dbh)),
    recorded_bole_char = sum(!is.na(bole_char)),
    .groups = "drop"
  ) %>%
  print()

alta_visits %>%
  select(
    tree_number, species_code, observation_date,
    plot_visit_id, dbh, height, bole_char,
    p_dead, assessment_protocol, tree_visit_notes
  ) %>%
  arrange(plot_visit_id, tree_number) %>%
  print(n = Inf, width = Inf)

# ---------------------------------------------------------
# Load Alta Peak tree visits
# ---------------------------------------------------------

load_alta_visits <- function(alta_visits) {
  con <- dbConnect(
    RPostgres::Postgres(),
    dbname = "phd_project"
  )
  on.exit(dbDisconnect(con), add = TRUE)
  
  dbWithTransaction(con, {
    # Existing tree identities
    trees <- dbGetQuery(
      con,
      "SELECT tree_id, tree_number, species_id
       FROM forest_health.tree
       WHERE plot_id = 52"
    )
    
    # Existing plot visits
    visits <- dbGetQuery(
      con,
      "SELECT plot_visit_id, plot_id, start_date
       FROM forest_health.plot_visit
       WHERE plot_visit_id IN (52, 59)"
    )
    
    stopifnot(
      nrow(trees) == 20L,
      anyDuplicated(trees$tree_number) == 0L,
      nrow(visits) == 2L,
      all(visits$plot_id == 52L)
    )
    
    # Match observations to existing trees and plot visits.
    # Each observation must find exactly one matching identity.
    matched <- alta_visits %>%
      left_join(
        trees,
        by = "tree_number",
        relationship = "many-to-one"
      ) %>%
      left_join(
        visits,
        by = "plot_visit_id",
        relationship = "many-to-one"
      ) %>%
      mutate(
        expected_species_id = case_when(
          species_code == "PIBA" ~ 2L,
          species_code == "PIMO" ~ 5L,
          TRUE ~ NA_integer_
        )
      )
    
    stopifnot(
      nrow(matched) == 40L,
      !anyNA(matched$tree_id),
      !anyNA(matched$start_date),
      !anyNA(matched$expected_species_id),
      all(matched$species_id == matched$expected_species_id),
      all(matched$observation_date == matched$start_date),
      anyDuplicated(
        matched[c("tree_id", "plot_visit_id")]
      ) == 0L
    )
    
    # Remove lookup fields that do not belong in tree_visit
    to_insert <- matched %>%
      select(
        -tree_number,
        -species_code,
        -observation_date,
        -species_id,
        -plot_id,
        -start_date,
        -expected_species_id
      )
    
    # Stop if observations were already loaded
    existing <- dbGetQuery(
      con,
      "SELECT tv.tree_id, tv.plot_visit_id
       FROM forest_health.tree_visit AS tv
       JOIN forest_health.tree AS t
         ON t.tree_id = tv.tree_id
       WHERE t.plot_id = 52
         AND tv.plot_visit_id IN (52, 59)"
    )
    
    if (nrow(existing) > 0L) {
      stop(
        "Alta Peak observations already exist for these visits. ",
        "Nothing was inserted."
      )
    }
    
    # PostgreSQL generates tree_visit_id
    dbAppendTable(
      con,
      Id(schema = "forest_health", table = "tree_visit"),
      to_insert
    )
    
    # Read back every inserted attribute for verification
    loaded <- dbGetQuery(
      con,
      "SELECT tv.*
       FROM forest_health.tree_visit AS tv
       JOIN forest_health.tree AS t
         ON t.tree_id = tv.tree_id
       WHERE t.plot_id = 52
         AND tv.plot_visit_id IN (52, 59)
       ORDER BY tv.plot_visit_id, tv.tree_id"
    )
    
    actual <- loaded %>%
      select(all_of(names(to_insert)))
    
    stopifnot(
      nrow(loaded) == 40L,
      anyDuplicated(
        loaded[c("tree_id", "plot_visit_id")]
      ) == 0L,
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
      left_join(
        trees %>% select(tree_id, tree_number),
        by = "tree_id",
        relationship = "many-to-one"
      ) %>%
      select(
        tree_visit_id, tree_number, tree_id, plot_visit_id,
        dbh, height, bole_char, p_dead, assessment_protocol
      ) %>%
      arrange(plot_visit_id, tree_number)
  })
}

# Run the import
alta_visits_loaded <- load_alta_visits(alta_visits)

alta_visits_loaded %>%
  as_tibble() %>%
  print(n = Inf, width = Inf)
