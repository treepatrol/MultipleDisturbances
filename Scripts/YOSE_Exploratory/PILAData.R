
# Plot and PILA Data ---------------------
# Author: Jenny Cribbs
# Date: 20 May 2024

# Input: The initial for loop brings in data from the YPE_Data folder
# Code Description: Pulls in relevant PILA and plot variables, creates  fire severity (unburned, low, medium, high)  variable with a value representing fire severity (0-3) or 0 unburned, joins plot data with prism and mtbs fire severity data
# Output: A csv file with PILA and plot level data

# --------------------------------------------------------------------

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
  newfiles = list.files(folder, pattern = "PILAdata") # PILA data only
  for(file in newfiles) {
    xlsfile <- read_excel(paste0(folder,"/",file)) %>% 
      dplyr::select(plotID, plot_type, trans_length, width, slope, aspect, plot_type,
                    fireseverity_50m, fireseverity_100m, fireseverity_150m, fireseverity_200m, 
                    ribes_50m, ribes_100m, ribes_150m, ribes_200m, 
                    DBH_cm, height_m, 
                    pitchTubes, exitHoles, 
                    activeBranchCanker, inactiveBranchCanker, 
                    activeBoleCanker, inactiveBoleCanker, 
                    DTOP, flags, percentLive, notes, plot_notes)
    tree_list <- rbind(tree_list, xlsfile)
  }
}

# consider using this instead of read_excel
#read.table("your_file.csv", header = TRUE, na.strings = "NA")

## summarizie data
summary(tree_list)
# visualize missing data
summary(aggr(tree_list))

# change data type to numeric
# conversion shouldn't be necessary with new verion of for loop
tree_list$trans_length <- as.numeric(tree_list$trans_length)
tree_list$width <- as.numeric(tree_list$width)
tree_list$slope <- as.numeric(tree_list$slope)
tree_list$aspect <- as.numeric(tree_list$aspect)
tree_list$DBH_cm <- as.numeric(tree_list$DBH_cm)
tree_list$height_m <- as.numeric(tree_list$height_m)
tree_list$flags <- as.numeric(tree_list$flags)
tree_list$percentLive <- as.numeric(tree_list$percentLive)
tree_list$activeBoleCanker <- as.numeric(tree_list$activeBoleCanker) 
tree_list$inactiveBoleCanker <- as.numeric(tree_list$inactiveBoleCanker)
tree_list$activeBranchCanker <- as.numeric(tree_list$activeBranchCanker)
tree_list$inactiveBranchCanker <- as.numeric(tree_list$inactiveBranchCanker)

## summarizie data
summary(tree_list)
# visualize missing data
summary(aggr(tree_list))

# create new columns combining active and inactive bole and branch cankers 
tree_list <- tree_list %>% 
  mutate(bole_canker = activeBoleCanker + inactiveBoleCanker,
         branch_canker = activeBranchCanker + inactiveBranchCanker) %>% 
  mutate(canker_count = branch_canker + bole_canker) %>% 
  mutate(infected = ifelse(bole_canker|branch_canker>0,1,0))

# Final version will use synthetic fire severity variable 
# code fire severity variable as a factor and combine some categories
# Mixed should not be higher than high, so recode mixed as moderate
# Recode fireseverity_50m
tree_list <- tree_list %>% 
  mutate(fireseverity_50m = recode(fireseverity_50m,
                                   "N" = 0,
                                   "unburned" = 0,
                                   "None" = 0,
                                   "low" = 1,
                                   "low_mod" = 2,
                                   "mod" = 2,
                                   "moderate" = 2,
                                   "mod_high" = 3,
                                   "high" = 3,
                                   "high/mixed" = 3,
                                   "mixed" = 2))

# Recode fireseverity_100m
tree_list <- tree_list %>%
  mutate(fireseverity_100m = recode(fireseverity_100m,
                                    "N" = 0,
                                    "unburned" = 0,
                                    "None" = 0,
                                    "low" = 1,
                                    "low_mod" = 2,
                                    "mod" = 2,
                                    "moderate" = 2,
                                    "mod_high" = 3,
                                    "high" = 3,
                                    "high/mixed" = 3,
                                    "mixed" = 2))
# Recode fireseverity_150m
tree_list <- tree_list %>% 
  mutate(fireseverity_150m = recode(fireseverity_150m,
                                    "N" = 0,
                                    "None" = 0,
                                    "unburned" = 0,
                                    "low" = 1,
                                    "low_mod" = 2,
                                    "mod" = 2,
                                    "moderate" = 2,
                                    "mod_high" = 3,
                                    "high" = 3,
                                    "high/mixed" = 3,
                                    "mixed" = 2))
# Recode fireseverity_200m
tree_list <- tree_list %>% 
  mutate(fireseverity_200m = recode(fireseverity_200m,
                                    "N" = 0,
                                    "None" = 0,
                                    "unburned" = 0,
                                    "low" = 1,
                                    "low_mod" = 2,
                                    "mod" = 2,
                                    "moderate" = 2,
                                    "mod_high" = 3,
                                    "high" = 3,
                                    "high/mixed" = 3,
                                    "mixed" = 2))

library(dplyr)
library(ggplot2)
library(gridExtra)

# Create an average fire severity variable for each plot
tree_list <- tree_list %>%
  mutate(fireseverity = rowMeans(select(., starts_with("fireseverity")), na.rm = TRUE))
# round result to maintain the 3 categories 
tree_list$fireseverity <- round(tree_list$fireseverity)
# check that it worked
unique(tree_list$fireseverity) # NaN is unknown fire?
# summarize plot-level data for fire severity
plot_fireseverity <- piladat %>%
  group_by(plotID) %>%
  summarize(plot_fireseverity = unique(fireseverity))

# Revist this later to combine with bole char data, plot photos, and remote sensing data
# Fire = plot_type highseverity AND fireseverity is high, moderate, or mixed (not unburned or low)

# write results to a csv file
write.csv(tree_list, "PILAdata.csv")
