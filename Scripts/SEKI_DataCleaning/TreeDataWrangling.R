
# Input: Tree level data entered from paper data sheets into Google Sheet
# Code Description: harmonize data entered primarily at the tree core level to accurately represent tree core, tree, and tree visit attributes. 
# Output: 3 CSV files for tree, tree_visit, and tree_core

# load data wrangling packages
library(tidyverse)
library(readxl)
library(janitor)

# load raw data
raw <- read_excel("/Users/jennifercribbs/Documents/R-Projects/MultipleDisturbances/Data/RawData/SEKI_Data/SEKI_2024_TreeFieldData.xlsx")

# select columns for tree table
tree_core <- raw %>% select()

names(tree_core)
glimpse(tree_core)

tree_core %>%
  count(plot_name, tree_num, name = "n_rows") %>%
  count(n_rows)

# adapt this to check the number of multiple cores per tree records
tree_core %>%
  filter(n_rows > 1)