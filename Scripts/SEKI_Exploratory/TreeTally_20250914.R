library(tidyverse)

plots <- read_csv("/Users/jennifercribbs/Documents/TreePatrol.org/Analysis/SEKIanalysis/SEKI_all_PlotData_20250821.csv")

trees2022 <- read_csv("/Users/jennifercribbs/Documents/TreePatrol.org/Analysis/SEKIanalysis/SEKI_2022_TreeFieldData.csv")

trees2024 <- read_csv("/Users/jennifercribbs/Documents/TreePatrol.org/Analysis/SEKIanalysis/SEKI_2024_TreeFieldData_20241208.csv")

# rename columns for consistency
# 2022 data
trees2022 <- trees2022 %>%
  rename(
    plot_id = Plot,
    Core_Height = Core_1_Height,
    Core_DBH = Core_1_DBH,
    Core_Depth = Core_1_depth
  ) %>%
  filter(!is.na(plot_id))

# 2024 data
trees2024 <- trees2024 %>%
  rename(
    plot_id = Plot_Name
  ) %>%
  select(-Plot_ID) %>%   # drop the redundant column
  filter(!is.na(plot_id))

# create a cored flag column
# coercing to numeric forces not_cored to NA
# Then numeric values are T and NAs are F
trees2022 <- trees2022 %>%
  mutate(
    cored = case_when(
      suppressWarnings(!is.na(as.numeric(Core_Height))) ~ TRUE,
      suppressWarnings(!is.na(as.numeric(Core_DBH))) ~ TRUE,
      suppressWarnings(!is.na(as.numeric(Core_Depth))) ~ TRUE,
      TRUE ~ FALSE
    )
  )

trees2024 <- trees2024 %>%
  mutate(
    cored = case_when(
      suppressWarnings(!is.na(as.numeric(Core_Height))) ~ TRUE,
      suppressWarnings(!is.na(as.numeric(Core_DBH))) ~ TRUE,
      suppressWarnings(!is.na(as.numeric(Core_Depth))) ~ TRUE,
      TRUE ~ FALSE
    )
  )


# count all trees by plot and species for 2022
counts_2022 <- trees2022 %>%
  count(plot_id, Species, name = "n_trees")

# count all trees by plot and species for 2024
counts_2024 <- trees2024 %>%
  count(plot_id, Species, name = "n_trees")

counts_all <- rbind(counts_2022, counts_2024) %>% arrange(plot_id, Species)

# count cored trees by plot and species for 2022
counts_cored_2022 <- trees2022 %>%
  filter(cored) %>%
  count(plot_id, Species, name = "n_cored")

# count cored trees by plot and species for 2024
counts_cored_2024 <- trees2024 %>%
  filter(cored) %>%
  count(plot_id, Species, name = "n_cored")

# stack them
counts_cored_all <- bind_rows(counts_cored_2022, counts_cored_2024) %>%
  mutate(
    species = toupper(Species),
    plot_id = trimws(plot_id)
  ) %>%
  arrange(plot_id, Species)

# final summary table
summary_all <- counts_all %>%
  left_join(counts_cored_all, by = c("plot_id", "Species")) %>%
  mutate(n_cored = coalesce(n_cored_2022, n_cored_2024, 0)) %>%
  select(plot_id, Species, n_trees, n_cored) %>% 
  arrange(plot_id, Species)
  


