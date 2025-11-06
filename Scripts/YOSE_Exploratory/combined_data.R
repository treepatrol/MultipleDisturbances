
# Plot Level Data Script ---------------------
# Author: Jenny Cribbs
# Date: 20 May 2024

# Input: The initial for loop brings in data from the YPE_Data folder
# Code Description: Pulls in relevant plot variables, creates  fire severity (unburned, low, medium, high)  variable with a value representing fire severity (0-3) or 0 unburned, joins plot data with prism and mtbs fire severity data
# Output: A csv file with plot level data

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
pila_list <- data.frame()
# bring in selected variables from excel files in datadir
for (folder in folders) {
  newfiles = list.files(folder, pattern = "PILAdata") # PILA data only
  for(file in newfiles) {
    xlsfile <- read_excel(paste0(folder,"/",file)) %>% 
      dplyr::select(plotID, plot_type, GPSdevice, waypoint_beg, plot_beg_UTM_E, plot_beg_UTM_N, estimatedAccuracy_beg_ft, waypoint_end, plot_end_UTM_E, plot_end_UTM_N, estimatedAccuracy_end_ft, trans_length, width, plot_azimuth, slope, aspect, plot_elevation_ft,
                    fireseverity_50m, fireseverity_100m, fireseverity_150m, fireseverity_200m, 
                    ribes_50m, ribes_100m, ribes_150m, ribes_200m, 
                    seedlings_50m, seedlings_100m, seedlings_150m, seedlings_200m, plot_notes,
                    treeNum, species, dOut_m, dSideR_m, dSideL_m, est_dOut_m, est_dSideR, est_dSideL, PILA_waypoint, PILA_UTM_E, PILA_UTM_N, estimatedAccuracy_PILA_ft, 
                    DBH_cm, est_DBH_cm, height_m, est_height_m, 
                    pitchTubes, exitHoles, 
                    activeBranchCanker, inactiveBranchCanker, 
                    activeBoleCanker, inactiveBoleCanker, 
                    DTOP, flags, percentLive, resistance, 
                    damageCodes, fire_scar, notes)
    pila_list <- rbind(pila_list, xlsfile)
  }
}

# check for missing plotIDs
unique(pila_list$plotID)
na_plotID_records <- pila_list %>% filter(is.na(plotID))
print(na_plotID_records) # appears to be just a blank line
# remove row with all NAs
pila_list <- pila_list %>% filter(!is.na(plotID)) 

## summarize data
summary(pila_list)
# visualize missing data
summary(aggr(pila_list))

# check species field
unique(pila_list$species)
na_species_records <- pila_list %>% filter(is.na(species))
print(na_species_records) # no NAs, but NA listed as a species?
species_na <- pila_list %>% filter(species == "NA") # no tree info
# remove the lines with no tree info from plot 29, 7, and 8
pila_list <- pila_list %>% filter(species != "NA")

# prior to coercing numeric variable check for ~ or notes in place of values
na_length <- pila_list %>% filter(is.na(trans_length)) # none
na_width <- pila_list %>% filter(is.na(width)) # none
na_slope <- pila_list %>% filter(is.na(slope)) # none
na_aspect <- pila_list %>% filter(is.na(aspect)) # none
na_length <- pila_list %>% filter(is.na(trans_length)) # none
na_flags <- pila_list %>% filter(is.na(flags)) # none
na_DTOP <- pila_list %>% filter(is.na(DTOP)) # none
na_inactiveBranch <- pila_list %>% filter(is.na(inactiveBranchCanker)) # none
na_activeBranch <- pila_list %>% filter(is.na(activeBranchCanker)) # none
na_inactiveBole <- pila_list %>% filter(is.na(inactiveBoleCanker)) # none
na_activeBole <- pila_list %>% filter(is.na(activeBoleCanker)) # none

# check missing DBH
na_DBH_records <- pila_list %>% filter(is.na(DBH_cm))
print(na_DBH_records$DBH_cm) # all 9 are NAs

# check missing height
na_height_records <- pila_list %>% filter(is.na(height_m))
print(na_height_records$height_m) # 4 NAs
# no height or species recorded in the field--exclude? or leave as is?

# check percent live
na_live_records <- pila_list %>% filter(is.na(percentLive))
print(na_live_records) # none!

# change data type to numeric 
pila_list$dOut_m <- as.numeric(pila_list$dOut_m)
pila_list$dSideL_m <- as.numeric(pila_list$dSideL_m)
pila_list$dSideR_m <- as.numeric(pila_list$dSideR_m)
pila_list$est_dOut_m <- as.numeric(pila_list$est_dOut_m)
pila_list$est_dSideL <- as.numeric(pila_list$est_dSideL)
pila_list$est_dSideR <- as.numeric(pila_list$est_dSideR)
pila_list$trans_length <- as.numeric(pila_list$trans_length)
pila_list$width <- as.numeric(pila_list$width)
pila_list$slope <- as.numeric(pila_list$slope)
pila_list$aspect <- as.numeric(pila_list$aspect)
pila_list$DBH_cm <- as.numeric(pila_list$DBH_cm)
pila_list$est_DBH_cm <- as.numeric(pila_list$est_DBH_cm)
pila_list$height_m <- as.numeric(pila_list$height_m)
pila_list$est_height_m <- as.numeric(pila_list$est_height_m)
pila_list$flags <- as.numeric(pila_list$flags)
pila_list$percentLive <- as.numeric(pila_list$percentLive)
pila_list$activeBoleCanker <- as.numeric(pila_list$activeBoleCanker) 
pila_list$inactiveBoleCanker <- as.numeric(pila_list$inactiveBoleCanker)
pila_list$activeBranchCanker <- as.numeric(pila_list$activeBranchCanker)
pila_list$inactiveBranchCanker <- as.numeric(pila_list$inactiveBranchCanker)

# Review NAs after data type conversion 
na_length <- pila_list %>% filter(is.na(trans_length)) # 5
print(na_length) # this is for the first 5 trees in YPE1, which are a separate group from those on the main transect 
na_width <- pila_list %>% filter(is.na(width)) # 611
print(na_width) # manual review?
na_slope <- pila_list %>% filter(is.na(slope)) # 34
print(na_slope) # all from plot 72
# no slope listed for plot 72, estimate from DEM? Recollection is 15ish degrees
na_aspect <- pila_list %>% filter(is.na(aspect)) # 34
# also all from plot 72
# azimuth for plot 72 is 118 could estimate slope as perpendicular so 208
na_flags <- pila_list %>% filter(is.na(flags)) # 3 
# plot 72 tree 22 left blank in the field, but % live is 99, inferring zero flags--fill this in with code
# plot 61 looks like tree 11 does not exist--just includes estimated DBH, height and dSide for tree 10--changed in Google Sheet
# plot 5 tree 1 is the only SD PILA and slightly negative dOut flags should be zero or this tree should not be counted

# check for missing flag assessment
print(na_flags)
na_DTOP <- pila_list %>% filter(is.na(DTOP)) # none

# check for missing canker assessment
na_inactiveBranch <- pila_list %>% filter(is.na(inactiveBranchCanker)) # 16
na_activeBranch <- pila_list %>% filter(is.na(activeBranchCanker)) # 16
na_inactiveBole <- pila_list %>% filter(is.na(inactiveBoleCanker)) # 20
na_activeBole <- pila_list %>% filter(is.na(activeBoleCanker)) # 19

# check missing DBH
na_DBH_records <- pila_list %>% filter(is.na(DBH_cm))
print(na_DBH_records$DBH_cm) # up to 75 from 9 before

# check missing height
na_height_records <- pila_list %>% filter(is.na(height_m))
print(na_height_records$height_m) # up to 65 from 4 NAs

# need to decide whether to use estimated height and DBH

# check percent live
na_live_records <- pila_list %>% filter(is.na(percentLive))
print(na_live_records) # none

# Combining dSideR_m and dSideL_m into a single dSide column with checks
pila_list <- pila_list %>%
  mutate(dSide = case_when(
    !is.na(dSideR_m) & dSideR_m >= 0 ~ dSideR_m,
    !is.na(dSideL_m) & dSideL_m <= 0 ~ dSideL_m,
    TRUE ~ NA_real_
  ))

summary(pila_list)

na_dSide_records <- pila_list %>% filter(is.na(dSide))
print(na_dSide_records) # 386 at first glance most of these have GPS waypoints

# check for dSideL errors
incorrect_dSideL <- pila_list %>%
  filter(!is.na(dSideL_m) & dSideL_m > 0)
print(incorrect_dSideL) # 48 some may be data entry errors others seem to be waypoints
# the first 31 trees in plot 39 were entered with positive dSideL values on the datasheet and in the Google Sheet
# Trees 2-6 in plot 10 were entered with positive values, but datasheet clearly has negative values and in the L column
# no way to check plot 14, but appears to be the same problem of forgetting the negative during entry

library(dplyr)

# Correcting the positive dSideL_m values
pila_list <- pila_list %>%
  mutate(dSideL_m = if_else(!is.na(dSideL_m) & dSideL_m > 0, -dSideL_m, dSideL_m))

# reapply the transformation
pila_list <- pila_list %>%
  mutate(dSide = case_when(
    !is.na(dSideR_m) & dSideR_m >= 0 ~ dSideR_m,
    !is.na(dSideL_m) & dSideL_m <= 0 ~ dSideL_m,
    TRUE ~ NA_real_
  ))

# double check for data entry errors
incorrect_side <- pila_list %>%
  filter(dSideL_m > 0 | dSideR_m <0) # none
# Check if there are still any NA values in dSide

na_records <- pila_list %>% filter(is.na(dSide))
print(na_records) # 338 don't have a dSide--hopefully mostly waypoints

summary(pila_list)

na_dOut_records <- tree_list %>% filter(is.na(dOut_m))
print(na_dOut_records) 

# trees on either side ar 48.2 and 49.2, so 48.7 should be a good guess
# Estimate the missing dOut value based on adjacent trees
# Assuming the missing dOut value for tree_id 3 should be 30

tree_list <- tree_list %>%
  mutate(dOut_m = if_else(plot == 74 & treeNum == 13, 48.7, dOut_m))


# plot 7 tree 6 had estimates for DBH and height that converted to NAs 
# used estimates from datasheet rather than leaving blank
tree_list <- tree_list %>%
  mutate(DBH_cm = if_else(plot == 7 & treeNum == 6, 50, DBH_cm)) %>% 
  mutate(height_m = if_else(plot == 7 & treeNum == 6, 10, height_m))
# tree 3 in plot 21 entered as 154.4., so did not convert to numeric--corrected in Google Sheet

summary(tree_list)

summary(aggr(tree_list))

## summarizie data
summary(pila_list)
# visualize missing data
summary(aggr(pila_list))

# create new columns combining active and inactive bole and branch cankers 
pila_list <- pila_list %>% 
  mutate(bole_canker = activeBoleCanker + inactiveBoleCanker,
         branch_canker = activeBranchCanker + inactiveBranchCanker) %>% 
  mutate(canker_count = branch_canker + bole_canker) %>% 
  mutate(infected = ifelse(bole_canker|branch_canker>0,1,0))

# manual review still needed for cankers 
# decide whether to use estimated height and DBH and position
# manual review needed for length and width
# need to incorporate waypoints into stem map 
 


# rename tree plot to plotID
# select tree-level columns 
# add columns 
# order should be plotID, treeNum, species, dOut_m, dSide, DBH_cm, est_DBH_cm, height_m, est_height_m, pitchTubes, exitHoles, activeBranchCanker, inactiveBranchCanker, activeBoleCanker, inactiveBoleCanker

pila_list <- pila_list %>% select(plotID, length, width, treeNum, species, dOut_m, dSide, DBH_cm, height_m, percentLive, damageCodes, notes)

# read in associated tree data
tree_list <- read.csv("/Users/jennifercribbs/Documents/YOSE/Analysis/MultipleDisturbances/Data/CleanData/CleanTreeList.csv", stringsAsFactors = FALSE)

tree_list <- tree_list %>% select(plot, treeNum, species, dOut_m, dSide, DBH_cm, height_m, percentLive, damageCodes, notes) %>% rename(plotID = plot)

# Ensure both data frames have the same column order
combined_data <- pila_list %>%
  select(names(tree_list))

# Combine the data frames using rbind
combined_data <- rbind(pila_list, tree_list) # R filled in NAs for extra columns

# Check the combined data
summary(combined_data)

# looks good except treeNum became character
combined_data$treeNum <- as.numeric(combined_data$treeNum)

# read in manually reviewed composite fire severity variable 

# read in plot-level mtbs, predicted cbi, and prism climate data
fire <- read.csv("/Users/jennifercribbs/Documents/YOSE/Analysis/MultipleDisturbances/predictedCBI_ground_check.csv") %>% select(plotID, Fire_Severity_Beg, Fire_Severity_plotAverage, mtbs_severity, predicted_CBI)
  
prism <- read.csv("/Users/jennifercribbs/Documents/YOSE/Analysis/MultipleDisturbances/prismFireRichness.csv") %>% select(plotID, vpdmax, ppt, tmean, richness, year, time_since)

test <- left_join(combined_data, fire) 
test2 <- left_join(test, prism)

# write results to a csv file
write.csv(combined_data, "OverstoryTrees.csv")

################ not sure if needed
# tally infected pila and calculate percent infected by plot
summarydat <- piladat %>% 
  group_by(plotID) %>% 
  summarize(numtrees = length(unique(treeNum, na.rm=T)), suminf = sum(infected),
            per_infected = (suminf/numtrees)*100) %>% 
  na.omit() %>% 
  ungroup() 

# overall percent infected is just under 3%
perinf <- summarydat %>% 
  summarize(perin = mean(per_infected))

# plot summary results and save object
perfig <- summarydat %>% 
  ggplot(aes(x=strata, y=per_infected, color = "infected", fill="infected"))+
  geom_jitter(height = 0.02, width = 0.05)+ geom_smooth(method = "lm")+ 
  ylab("WPBR Extent (%)") + xlab("Climate Strata") + 
  #scale_x_discrete(guide = guide_axis(angle = 90)) +
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE) +
  guides(fill=F, color=F)

# display summary results
perfig

# is there a correlation between beetles and wpbr?
piladat$pt <- ifelse(tree_list$pitchTubes == "N", 0, 1)
piladat$eh <- ifelse(tree_list$exitHoles == "N", 0, 1)

# convert 0s and 1s to numeric data type
#tree_list$pt <- as.numeric(tree_list$pt) 
#tree_list$eh <- as.numeric(tree_list$eh)

# create new column combining pitch tubes and exit holes
piladat <- piladat %>% 
  mutate(beetles = ifelse(pt|eh>0,1,0))

# tally beetle infested trees and calculate percent infested by plot
summarydatbeetles <- piladat %>% 
  group_by(plotID, strata) %>% 
  summarize(numtrees = length(unique(treeNum, na.rm=T)), sumbeetles = sum(beetles),
            per_beetles = (sumbeetles/numtrees)*100) %>% 
  na.omit() %>% 
  ungroup() 

# overall percent infected is around 26%
perbeetles <- summarydatbeetles %>% 
  summarize(perbeetles = mean(per_beetles))
perbeetles <- summarydatbeetles %>% 
  summarize(perbeetles = (per_beetles))

# plot summary results and save object
perfigb <- summarydatbeetles %>% 
  ggplot(aes(x=strata, y=per_beetles, color = "beetles", fill="beetles"))+
  geom_jitter(height = 0.02, width = 0.05)+ geom_smooth(method = "lm")+
  ylab("Beetle Extent (%)") + xlab("Climate Strata") + 
  #scale_x_discrete(guide = guide_axis(angle = 90)) +
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE) +
  guides(fill=F, color=F)

# display summary results
perfigb

perfig + perfigb