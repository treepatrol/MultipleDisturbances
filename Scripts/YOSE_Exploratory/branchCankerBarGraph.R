
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
  theme_bw(base_size = 17)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
)

## unzipping files on your computer
setwd("/Users/jennifercribbs/Documents/YOSE/Analysis/YPE_Data")
files <- list.files()
outdir <- "/Users/jennifercribbs/Documents/YOSE/Analysis/YPE_Data"

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
        dplyr::select(plotID, plot_type, slope, aspect, fireseverity_50m, fireseverity_100m, fireseverity_150m, fireseverity_200m, ribes_50m, ribes_100m, ribes_150m, ribes_200m, 
                      treeNum, DBH_cm, height_m, pitchTubes, exitHoles, activeBranchCanker, inactiveBranchCanker, 
                      activeBoleCanker, inactiveBoleCanker, DTOP, flags, percentLive, damageCodes, notes, plot_elevation_ft)
      tree_list <- rbind(tree_list, xlsfile)
  }
}


## summarizing data

# change data type to numeric 
tree_list$activeBoleCanker <- as.numeric(tree_list$activeBoleCanker) 
tree_list$inactiveBoleCanker <- as.numeric(tree_list$inactiveBoleCanker)
tree_list$activeBranchCanker <- as.numeric(tree_list$activeBranchCanker)
tree_list$inactiveBranchCanker <- as.numeric(tree_list$inactiveBranchCanker)

# create new columns combining active and inactive bole and branch cankers 
piladat <- tree_list %>% 
  mutate(bole_canker = activeBoleCanker + inactiveBoleCanker,
         branch_canker = activeBranchCanker + inactiveBranchCanker) %>% 
  mutate(infected = ifelse(bole_canker|branch_canker>0,1,0))

# plot branch cankers
piladat %>% 
  ggplot(aes(y=branch_canker, fill=branch_canker))+
  geom_bar()

# plot bole cankers
piladat %>% 
  ggplot(aes(y=bole_canker, fill=bole_canker))+
  geom_bar()

# tally infected trees and calculate percent infected by plot
summarydat <- piladat %>% 
  rename(elevation = plot_elevation_ft) %>% 
  group_by(plotID, elevation) %>% 
  summarize(numtrees = length(unique(treeNum, na.rm=T)), suminf = sum(infected),
            per_infected = (suminf/numtrees)*100) %>% 
  na.omit() %>% 
  ungroup() %>% 
  mutate(plot = row_number(), plot2 = as.character(plot))

# overall percent infected is just under 3%
perinf <- summarydat %>% 
  summarize(perin = mean(per_infected))

# plot summary results and save object
perfig <- summarydat %>% 
  ggplot(aes(x=elevation, y=per_infected, color = "infected", fill="infected"))+
  geom_point()+ geom_smooth(method="lm")+
  ylab("% infected") + xlab("Elevation (m)") + 
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE) +
  guides(fill=F, color=F)

# display summary results
perfig

numfig <- summarydat %>%
  ggplot(aes(x=plot, y=per_infected, color = plot, fill=plot))+
  geom_bar(stat = "identity")+
  ylab("% infected") + xlab("Plot")+
  scale_color_viridis()+
  scale_fill_viridis()+
  #scale_color_brewer(palette = "Dark2") +
  #scale_fill_brewer(palette = "Dark2")+
  guides(fill=F, color=F)+
  ylim(0,10)

numfig + perfig

# is there a correlation between beetles and wpbr
tree_list$pt <- ifelse(tree_list$pitchTubes == "N", 0, 1)
tree_list$eh <- ifelse(tree_list$exitHoles == "N", 0, 1)

# convert 0s and 1s to numeric data type
#tree_list$pt <- as.numeric(tree_list$pt) 
#tree_list$eh <- as.numeric(tree_list$eh)

# create new column combining pitch tubes and exit holes
piladat <- tree_list %>% 
  mutate(beetles = ifelse(pt|eh>0,1,0))

# tally beetle infested trees and calculate percent infested by plot
summarydatbeetles <- piladat %>% 
  rename(elevation = plot_elevation_ft) %>% 
  group_by(plotID, elevation) %>% 
  summarize(numtrees = length(unique(treeNum, na.rm=T)), sumbeetles = sum(beetles),
            per_beetles = (sumbeetles/numtrees)*100) %>% 
  na.omit() %>% 
  ungroup() %>% 
  mutate(plot = row_number(), plot2 = as.character(plot))

# overall percent infected is around 26%
perbeetles <- summarydatbeetles %>% 
  summarize(perbeetles = mean(per_beetles))

# plot summary results and save object
perfigb <- summarydatbeetles %>% 
  ggplot(aes(x=elevation, y=per_beetles, color = "beetles", fill="beetles"))+
  geom_point()+ geom_smooth(method="lm")+
  ylab("% beetle infested") + xlab("Elevation (m)") + 
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE) +
  guides(fill=F, color=F)

# display summary results
perfigb

numfigb <- summarydatbeetles %>%
  ggplot(aes(x=plot, y=per_beetles, color = plot, fill=plot))+
  geom_bar(stat = "identity")+
  ylab("% beetle infested") + xlab("Plot") +
  scale_color_viridis()+
  scale_fill_viridis()+
  #scale_color_brewer(palette = "Dark2") +
  #scale_fill_brewer(palette = "Dark2")+
  guides(fill=F, color=F)+
  ylim(0,10)

numfigb + perfigb

# figure out a way to consider correlation given the two variables are not continuous

# display percent infected for high severity and random plots
# subset data into high severity and random plots
hs <- filter(piladat, plot_type == "highseverity")
random <- filter(piladat, plot_type == "random")

# tally infected trees and calculate percent infected by plot
hs <- hs %>% 
  rename(elevation = plot_elevation_ft) %>% 
  group_by(plotID, elevation) %>% 
  summarize(numtrees = length(unique(treeNum, na.rm=T)), suminf = sum(infected),
            per_infected = (suminf/numtrees)*100) %>% 
  na.omit() %>% 
  ungroup() %>% 
  mutate(plot = row_number(), plot2 = as.character(plot))

# overall percent infected is around 0.4%
perinfhs <- hs %>% 
  summarize(perinhs = mean(per_infected))

# plot summary results and save object
perfighs <- hs %>% 
  ggplot(aes(x=elevation, y=per_infected, color = "infected", fill="infected"))+
  geom_point()+ geom_smooth(method="lm")+
  ylab("% infected") + xlab("Elevation (m)") + 
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE) +
  guides(fill=F, color=F)

# display summary results
perfighs

numfighs <- hs %>%
  ggplot(aes(x=plot, y=per_infected, color = plot, fill=plot))+
  geom_bar(stat = "identity")+
  ylab("% infected") + xlab("Plot")+
  scale_color_viridis()+
  scale_fill_viridis()+
  #scale_color_brewer(palette = "Dark2") +
  #scale_fill_brewer(palette = "Dark2")+
  guides(fill=F, color=F)+
  ylim(0,10)

numfighs + perfighs

# Random plots
random <- random %>% 
  rename(elevation = plot_elevation_ft) %>% 
  group_by(plotID, elevation) %>% 
  summarize(numtrees = length(unique(treeNum, na.rm=T)), suminf = sum(infected),
            per_infected = (suminf/numtrees)*100) %>% 
  na.omit() %>% 
  ungroup() %>% 
  mutate(plot = row_number(), plot2 = as.character(plot))

# overall percent infected is just under 3%
perinfr <- random %>% 
  summarize(perinr = mean(per_infected))

# plot summary results and save object
perfigr <- random %>% 
  ggplot(aes(x=elevation, y=per_infected, color = "infected", fill="infected"))+
  geom_point()+ geom_smooth(method="lm")+
  ylab("% infected") + xlab("Elevation (m)") + 
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE) +
  guides(fill=F, color=F)

# display summary results
perfigr

numfigr <- random %>%
  ggplot(aes(x=plot, y=per_infected, color = plot, fill=plot))+
  geom_bar(stat = "identity")+
  ylab("% infected") + xlab("Plot")+
  scale_color_viridis()+
  scale_fill_viridis()+
  #scale_color_brewer(palette = "Dark2") +
  #scale_fill_brewer(palette = "Dark2")+
  guides(fill=F, color=F)+
  ylim(0,10)

numfigr + perfigr
