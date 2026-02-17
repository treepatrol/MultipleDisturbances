
# Input: Tree level data entered from paper data sheets into Google Sheet
# Code Description: load raw tree data and use select to separate columns into a static tree table (species, location, etc.), a tree visit table (growth and health measurements), and a tree core table (1 row per core rather than 1 row per tree)
# Output: 3 CSV files for tree table, tree visit table, and tree core table

# load data wrangling packages
library(tidyverse)

# load raw data
trees <- read_csv("/Users/jennifercribbs/Documents/R-Projects/MultipleDisturbances/Data/RawData/SEKI_Data/SEKI_2024_TreeFieldData.xlsx")
