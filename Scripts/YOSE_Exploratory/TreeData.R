
## ---------------------------
##
## Script name: Associated Tree Data Import Script
## Will probably be step 6 or 7 after the understory scripts
##
## Author: Jenny Cribbs and Dr. Joan Dudney
##
## Date Created: 2024-06-15
##
## Copyright (c) 2024 Jenny Cribbs
## Email: jecribbs@ucdavis.edu
##
## ---------------------------
## Input: Raw associated tree data field data (Treedata Google Sheet)
## 
## This script reads in the raw data using a for loop then checks NAs for each variable. I made corrections to missing values when possible based on a review of the original paper data sheets and entered data. In the case of clear typos (e.g. sign error for dSideL or entering the dSide in the incorrect column), I made the correction in the Google Sheet. For one plot all dSideL values were entered as positive, so I left this in the Google sheet, but changed these values to negative as part of this script. For other variables, I changed clear errors in the Google Sheet and documented them in the Data Documentation Word Doc. In the case one tree with estimated DBH where numbers were entered with ~, I left this in the entered data, but replaced with the numeric value in this script. For one blank dOut value, I again left the entered data alone, but replaced that value with an estimate since the two adjacent trees were close--full documentation for this imputation is in my notes and will go in the manuscript. 
##
## Output: Clean csv file for associated trees with only 2 NAs 
## ---------------------------

# Load initial packages
library(tidyverse)
library(readxl)
library(VIM)

# setting the directory for data extraction
datadir <- "/Users/jennifercribbs/Documents/YOSE/Analysis/MultipleDisturbances/Data/RawData/YPE_Data"

# list all file names in the data directory
files <- list.files(datadir)
# provide path for files in datadir
folders <- list.dirs(datadir)[-c(1,4)]
# create a blank dataframe 
tree_list <- data.frame()
# bring in selected variables from excel files in datadir
for (folder in folders) {
  files = list.files(folder, pattern = "Treedata") # associated trees
  for(file in files) {
    xlsfile <- read_excel(paste0(folder,"/",file)) %>% 
      dplyr::select(plot, date, crew, treeNum, species, dOut_m, dSideR_m, dSideL_m, DBH_cm, height_m, percentLive, damageCodes, notes
                    )
    tree_list <- rbind(tree_list, xlsfile)
  }
}

# change data type to numeric--in the future do this after an NA check 
tree_list$DBH_cm <- as.numeric(tree_list$DBH_cm)
tree_list$height_m <- as.numeric(tree_list$height_m)
tree_list$percentLive <- as.numeric(tree_list$percentLive)
tree_list$dOut_m <- as.numeric(tree_list$dOut_m) 
tree_list$dSideR_m <- as.numeric(tree_list$dSideR_m)
tree_list$dSideL_m <- as.numeric(tree_list$dSideL_m)

# summarizie data
summary(tree_list)

# visualize missing data
summary(aggr(tree_list)) 

# Combining dSideR_m and dSideL_m into a single dSide column with checks
tree_list <- tree_list %>%
  mutate(dSide = case_when(
    !is.na(dSideR_m) & dSideR_m >= 0 ~ dSideR_m,
    !is.na(dSideL_m) & dSideL_m <= 0 ~ dSideL_m,
    TRUE ~ NA_real_
  ))

summary(tree_list)

na_dSide_records <- tree_list %>% filter(is.na(dSide))
print(na_dSide_records)

incorrect_dSideL <- tree_list %>%
  filter(!is.na(dSideL_m) & dSideL_m > 0)
print(incorrect_dSideL)

library(dplyr)

# Correcting the positive dSideL_m values
tree_list <- tree_list %>%
  mutate(dSideL_m = if_else(!is.na(dSideL_m) & dSideL_m > 0, -dSideL_m, dSideL_m))

# Reapply the transformation to create the dSide column
tree_list <- tree_list %>%
  mutate(dSide = case_when(
    !is.na(dSideR_m) & dSideR_m >= 0 ~ dSideR_m,
    !is.na(dSideL_m) & dSideL_m <= 0 ~ dSideL_m,
    TRUE ~ NA_real_
  ))

# Check if there are still any NA values in dSide
na_records <- tree_list %>% filter(is.na(dSide))
print(na_records) # no more NAs for dSide

summary(tree_list)

na_dOut_records <- tree_list %>% filter(is.na(dOut_m))
print(na_dOut_records) 

# trees on either side ar 48.2 and 49.2, so 48.7 should be a good guess
# Estimate the missing dOut value based on adjacent trees
# Assuming the missing dOut value for tree_id 3 should be 30

tree_list <- tree_list %>%
  mutate(dOut_m = if_else(plot == 74 & treeNum == 13, 48.7, dOut_m))

# check missing DBH
na_DBH_records <- tree_list %>% filter(is.na(DBH_cm))
print(na_DBH_records) 

# plot 7 tree 6 had estimates for DBH and height that converted to NAs 
# used estimates from datasheet rather than leaving blank
tree_list <- tree_list %>%
  mutate(DBH_cm = if_else(plot == 7 & treeNum == 6, 50, DBH_cm)) %>% 
  mutate(height_m = if_else(plot == 7 & treeNum == 6, 10, height_m))
# tree 3 in plot 21 entered as 154.4., so did not convert to numeric--corrected in Google Sheet

# check missing height
na_height_records <- tree_list %>% filter(is.na(height_m))
print(na_height_records) 
# no height or species recorded in the field--exclude? or leave as is?

# check percent live
na_live_records <- tree_list %>% filter(is.na(percentLive))
print(na_live_records) # missed in the field

summary(tree_list)

summary(aggr(tree_list))

# check species field
unique(tree_list$species)
na_species_records <- tree_list %>% filter(is.na(species))
print(na_species_records) # no NAs
# transform species list to consolidate repeats
tree_list <- tree_list %>%
  mutate(species = case_when(
    species == "PYGE" ~ "PIJE",
    species %in% c("unknown", "UNK", "UNKNOWN", "Charcol", "Unknown") ~ "UNKNOWN",
    TRUE ~ species
  ))
# re-check
unique(tree_list$species) # list is cleaner
na_species_records <- tree_list %>% filter(is.na(species))
print(na_species_records) # still no NAs

# write results to a csv
write.csv(tree_list, "CleanTreeList.csv")
