library(dplyr)
library(here)

# load results of physical core inventory
core_inventory <- read_csv(here("Data", "RawData", "SEKI_Data","core_inventory_check.csv"))
# check column names
names(core_inventory)
# load trees from 3 plots to add and review
twinLakes <- read_csv(here("Data", "RawData", "SEKI_Data", "PICO8b_TwinLakes.csv"))
alta <- read_csv(here("Data", "RawData", "SEKI_Data", "AltaPeak.csv"))
pimo7c <- read_csv(here("Data", "RawData", "SEKI_Data", "PIMO7c.csv"))
# bind 3 plots into an addendum 
addendum <- rbind(twinLakes, alta, pimo7c)
# column names for addendum
names(addendum)

# data wrangling to make the columns the same

# add the addendum 
core_inventory_add3plots <- bind_rows(core_inventory, addendum)
