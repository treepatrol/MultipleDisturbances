library(ggplot2)
library(tidyr)
library(dplyr)
library(readr)
library(patchwork)  # for combining plots

ype74livePILAs <- read_csv("/Users/jennifercribbs/Documents/Graduate School/ECL290_GGE/YPE74livePILAs.csv")

# Tidy it up if needed
df_long <- df %>%
  pivot_longer(cols = starts_with("NDVI"):starts_with("GRVI"), 
               names_to = "Index", values_to = "Value") %>%
  mutate(TreeStatus = ifelse(grepl("Live", RegionName), "Live", "Dead"),
         BandType = ifelse(Index %in% c("NDVI", "GCC", "GRVI"), "VI", "Raw Band"))

# Plot with facets
p <- ggplot(df_long, aes(x = Date, y = Value, color = Index)) +
  geom_line(size = 1) +
  facet_grid(TreeStatus ~ BandType) +
  theme_minimal(base_size = 14) +
  labs(title = "Vegetation Patterns Over Time")

ggsave("4panel.png", p, width = 10, height = 6)
