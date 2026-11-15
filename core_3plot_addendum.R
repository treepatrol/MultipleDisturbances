library(dplyr)
library(here)
library(readr)

# load results of physical core inventory
core_inventory <- read_csv(here("Data", "RawData", "SEKI_Data","core_inventory_check.csv"))
# check column names
names(core_inventory)
# load trees from 3 plots to add and review
twinLakes <- read_csv(here("Data", "RawData", "SEKI_Data", "PICO8b_TwinLakes.csv"))
alta <- read_csv(here("Data", "RawData", "SEKI_Data", "AltaPeak.csv"))
pimo7c <- read_csv(here("Data", "RawData", "SEKI_Data", "PIMO7c.csv"))

# add plot ids for addendum plots
twinLakes <- twinLakes %>% 
  mutate(
    plot_id = 50L, 
    target_species = "PICO", 
    target_stratum = 8L)
alta     <- alta %>% mutate(
  plot_id = 52L, 
  target_species = "PIBA", 
  target_stratum = 7L)
pimo7c   <- pimo7c %>% mutate(
  plot_id = 49L, 
  target_species = "PIMO", 
  target_stratum = 7L)

# bind 3 plots into an addendum 
addendum <- rbind(twinLakes, alta, pimo7c) %>% 
  janitor::clean_names()
# column names for addendum
names(addendum)

# data wrangling to make the columns names the same
addendum <- addendum %>%
  rename(
    core_height_raw = core_height,
    core_dbh_raw = core_dbh
  )
# add the addendum 
core_inventory_add3plots <- bind_rows(core_inventory, addendum) %>%
  select(all_of(names(core_inventory)))

# write out updated inventory sheet for manual review
write_csv(core_inventory_add3plots,
          here ("Data", "core_inventory_updated20261115"), 
          na = "")
