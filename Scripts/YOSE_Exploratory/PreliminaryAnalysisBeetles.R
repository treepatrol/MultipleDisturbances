
## ---------------------------
##
## Script name: YOSE WPBR Incidence and interactions with beetles and fire
##
## Author: Dr. Joan Dudney and Jenny Cribbs
##
## Date Created: 2023-11-24
##
## Copyright (c) Joan Dudney, 2023
## Email: dudney@ucsb.edu
##
## ---------------------------
##
## Notes: using new version with librarian package
##   
##
## ---------------------------
if (!require("librarian")) install.packages("librarian")
librarian::shelf(tidyverse, readxl, viridis, patchwork)

theme_set(
  theme_bw(base_size = 12)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
)

## setting up data on your computer
setwd("/Users/jennifercribbs/Documents/YOSE/Analysis/Test/")
files <- list.files()
outdir <- "/Users/jennifercribbs/Documents/YOSE/Analysis/Test/"

## for loop to unzip files on your computer
# for (file in files) {
#   unzip(paste0(outdir,"/", file))
# }


## reading in all data
folders <- list.dirs(outdir)[-c(1,4)]

tree_list <- data.frame()

for (folder in folders) {
  newfiles = list.files(folder, pattern = "PILAdata")
    for(file in newfiles) {
      xlsfile <- read_excel(paste0(folder,"/",file)) %>% 
        dplyr::select(plotID, strata, plot_type, slope, aspect, plot_elevation_ft, fireseverity_50m, fireseverity_100m, fireseverity_150m, fireseverity_200m, ribes_50m, ribes_100m, ribes_150m, ribes_200m, 
                      treeNum, DBH_cm, height_m, pitchTubes, exitHoles, activeBranchCanker, inactiveBranchCanker, 
                      activeBoleCanker, inactiveBoleCanker, DTOP, flags, percentLive, damageCodes, notes)
      tree_list <- rbind(tree_list, xlsfile)
  }
}


## summarizing data

# change data type to numeric 
tree_list$activeBoleCanker <- as.numeric(tree_list$activeBoleCanker) 
tree_list$inactiveBoleCanker <- as.numeric(tree_list$inactiveBoleCanker)
tree_list$activeBranchCanker <- as.numeric(tree_list$activeBranchCanker)
tree_list$inactiveBranchCanker <- as.numeric(tree_list$inactiveBranchCanker)
tree_list$DBH_cm <- as.numeric(tree_list$DBH_cm)
tree_list$height_m <- as.numeric(tree_list$height_m)
tree_list$percentLive <- as.numeric(tree_list$percentLive)

# create new columns combining active and inactive bole and branch cankers 
piladat <- tree_list %>% 
  mutate(bole_canker = activeBoleCanker + inactiveBoleCanker,
         branch_canker = activeBranchCanker + inactiveBranchCanker) %>% 
  mutate(infected = ifelse(bole_canker|branch_canker>0,1,0))


# tally infected trees and calculate percent infected by plot
summarydat <- piladat %>% 
  group_by(plotID, strata) %>% 
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

# is there a correlation between beetles and wpbr
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


# figure out a way to consider correlation given the two variables are not continuous

# display percent infected for high severity and random plots
# subset data into high severity and random plots
# piladat <- piladat %>% mutate(HS = ifelse(plot_type == "highseverity" | plot_type == "high severity", 1, 0))
# hs2 <- filter(piladat, plot_type == "high severity")
# piladat <- mutate(piladat, random = ifelse(plot_type == "random", 1, 0))









# logistic glmm model 

library(MASS)
library(nlme)
model1 <- glm(infected ~ plot_type + DBH_cm + 1|plotID, family = binomial, data = piladat)
summary(lme(infected ~ plot_type + DBH_cm + 1|plotID, family = binomial, data = piladat))


