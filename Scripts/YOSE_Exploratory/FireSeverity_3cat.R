
# Fire Severity Script (Binary and 3 fire categories) ---------------------
# Author: Jenny Cribbs
# Date: 11 May 2024

# Input: The initial for loop brings in data from the YPE_Data folder
# Code Description: Creates  fire severity (unburned, low, medium, high) and fire binary (0/1) objects with a value representing fire severity (0-3) or fire presence/abscence (0/1) for each plot
# Output: A csv file for fire severity and a csv file fire binary

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
      dplyr::select(plotID, trans_length, width, slope, aspect, plot_type,
                    fireseverity_50m, fireseverity_100m, fireseverity_150m, fireseverity_200m, 
                    ribes_50m, ribes_100m, ribes_150m, ribes_200m, 
                    DBH_cm, height_m, 
                    pitchTubes, exitHoles, 
                    activeBranchCanker, inactiveBranchCanker, 
                    activeBoleCanker, inactiveBoleCanker, 
                    DTOP, flags, percentLive, notes)
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
piladat <- tree_list %>% 
  mutate(bole_canker = activeBoleCanker + inactiveBoleCanker,
         branch_canker = activeBranchCanker + inactiveBranchCanker) %>% 
  mutate(canker_count = branch_canker + bole_canker) %>% 
  mutate(infected = ifelse(bole_canker|branch_canker>0,1,0))

# EDA for on the ground fire variable
unique(piladat$fireseverity_50m)
unique(piladat$fireseverity_100m)
unique(piladat$fireseverity_150m)
unique(piladat$fireseverity_200m)

# bar chart to look at fire severity ratings
piladat %>% 
  group_by(fireseverity_100m) %>%
  summarize (n = n()) %>%
  mutate(total = sum(n),
         freq = n / total) %>%
  ggplot() +
  geom_bar(mapping=aes(x=fireseverity_100m, y=freq),stat="identity") +
  xlab("Fire Severity Rating") +
  ylab("Proportion")

# code variable as a factor and combine some categories
# Mixed should not be higher than high, so recode mixed
# Recode fireseverity_50m
piladat <- piladat %>% 
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
piladat <- piladat %>%
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
piladat <- piladat %>% 
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
piladat <- piladat %>% 
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
# bar chart to look at fire severity ratings again

library(dplyr)
library(ggplot2)
library(gridExtra)

# Create individual plots for each fire severity variable
firesev50 <- piladat %>% 
  group_by(fireseverity_50m) %>%
  summarize(n = n()) %>%
  mutate(total = sum(n),
         freq = n / total) %>%
  ggplot() +
  geom_bar(mapping = aes(x = fireseverity_50m, y = freq), stat = "identity") +
  xlab("Fire Severity Rating") +
  ylab("Proportion") +
  ggtitle("Fire Severity 50m")

firesev100 <- piladat %>% 
  group_by(fireseverity_100m) %>%
  summarize(n = n()) %>%
  mutate(total = sum(n),
         freq = n / total) %>%
  ggplot() +
  geom_bar(mapping = aes(x = fireseverity_100m, y = freq), stat = "identity") +
  xlab("Fire Severity Rating") +
  ylab("Proportion") +
  ggtitle("Fire Severity 100m")

firesev150 <- piladat %>% 
  group_by(fireseverity_150m) %>%
  summarize(n = n()) %>%
  mutate(total = sum(n),
         freq = n / total) %>%
  ggplot() +
  geom_bar(mapping = aes(x = fireseverity_150m, y = freq), stat = "identity") +
  xlab("Fire Severity Rating") +
  ylab("Proportion") +
  ggtitle("Fire Severity 150m")

firesev200 <- piladat %>% 
  group_by(fireseverity_200m) %>%
  summarize(n = n()) %>%
  mutate(total = sum(n),
         freq = n / total) %>%
  ggplot() +
  geom_bar(mapping = aes(x = fireseverity_200m, y = freq), stat = "identity") +
  xlab("Fire Severity Rating") +
  ylab("Proportion") +
  ggtitle("Fire Severity 200m")

# Combine plots using facet_wrap
combined_plots <- list(firesev50, firesev100, firesev150, firesev200)
facet_wrap_plots <- lapply(combined_plots, function(p) p + theme(axis.text.x = element_text(angle = 0, hjust = 1)))

# Arrange the plots in a 2x2 grid
grid.arrange(grobs = facet_wrap_plots, ncol = 2)

# Create an average fire severity variable for each plot
piladat <- piladat %>%
  mutate(fireseverity = rowMeans(select(., starts_with("fireseverity")), na.rm = TRUE))

# bar chart to look at fire severity ratings
piladat %>% 
  group_by(fireseverity) %>%
  summarize (n = n()) %>%
  mutate(total = sum(n),
         freq = n / total) %>%
  ggplot() +
  geom_bar(mapping=aes(x=fireseverity, y=freq),stat="identity") +
  xlab("Plot Fire Severity Rating") +
  ylab("Proportion")

# consider rounding to maintain the 3 categories 

# Recode as fire or no fire
# Revist this later to combine with bole char data, plot photos, and remote sensing data
# Fire = plot_type highseverity AND fireseverity is high, moderate, or mixed (not unburned or low)
# starting with just the 50m on the ground rating due to NAs (but still has 55 NAs)
piladat <- piladat %>% 
  mutate(firebinary = ifelse(plot_type == "highseverity" & fireseverity >= 1, 1, 0))

summary(piladat)

hist(piladat$firebinary)

piladat %>% 
  group_by(firebinary) %>%
  summarize (n = n()) %>%
  mutate(total = sum(n),
         freq = n / total) %>%
  ggplot() +
  geom_bar(mapping=aes(x=firebinary, y=freq),stat="identity") +
  xlab("Plot Fire Severity Rating") +
  ylab("Proportion")

# summarize plot-level data for fire versus no fire
plot_firebinary <- piladat %>%
  group_by(plotID) %>%
  summarize(plot_firebinary = unique(firebinary))

print(plot_firebinary)

# summarize plot-level data for fire versus no fire
plot_fireseverity <- piladat %>%
  group_by(plotID) %>%
  summarize(plot_fireseverity = unique(fireseverity))

# check results
print(plot_fireseverity)

# write results to a csv file
write.csv(plot_fireseverity, "plot_fireseverity.csv")
write.csv(plot_firebinary, "plot_firebinary.csv")
