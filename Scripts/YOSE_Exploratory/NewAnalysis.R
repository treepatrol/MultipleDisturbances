
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
  theme_bw(base_size = 15)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
)

## unzipping files on your computer
setwd("/Users/jennifercribbs/Documents/YOSE/Analysis/Test")
files <- list.files()
outdir <- "/Users/jennifercribbs/Documents/YOSE/Analysis/Test"

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
        dplyr::select(plotID, plot_type, strata, slope, aspect, plot_elevation_ft,
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
tree_list$plot_elevation_ft <- as.numeric(tree_list$plot_elevation_ft)

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

# tally infected trees and calculate percent infected by plot and elevation
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

# plot summary results for infection rate
perfigelevation <- piladat %>% 
  ggplot(aes(x=plot_elevation_ft, y=infected, color = "infected", fill="infected"))+
  geom_point()+ geom_smooth(method = "lm")+
  ylab("WPBR Infection Rate (%)") + xlab("Elevation (ft)") + 
  scale_x_continuous(guide = guide_axis(angle = 90)) +
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE) +
  guides(fill=F, color=F)

# plot summary results for extent save object
extent_elevation <- summarydat %>% 
  ggplot(aes(x=elevation, y=per_infected, color = "infected", fill="infected"))+
  geom_jitter()+ geom_smooth(method = "lm")+
  ylab("WPBR Extent (%)") + xlab("Elevation (ft)") + 
  scale_x_continuous(guide = guide_axis(angle = 90)) +
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE) +
  guides(fill=F, color=F)

# Now for climate strata
# tally infected trees and calculate percent infected by plot and elevation
summarydat <- piladat %>% 
  group_by(plotID, strata) %>% 
  summarize(numtrees = length(unique(treeNum, na.rm=T)), suminf = sum(infected),
            per_infected = (suminf/numtrees)*100) %>% 
  na.omit() %>% 
  ungroup() %>% 
  mutate(plot = row_number(), plot2 = as.character(plot))

# overall percent infected is just under 3%
perinf <- summarydat %>% 
  summarize(perin = mean(per_infected))

# plot summary results for extent
perfigstrata <- summarydat %>% 
  ggplot(aes(x=strata, y=per_infected, color = "per_infected", fill="per_infected"))+
  geom_jitter()+ geom_smooth(method = "lm")+
  ylab("WPBR Extent (%)") + xlab("Climate Strata") + 
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE) +
  guides(fill=F, color=F)
perfigstrata
# plot summary results for infection rate and save object
irstratafig <- piladat %>% 
  ggplot(aes(x=strata, y=infected, color = "infected", fill="infected"))+
  geom_jitter()+ geom_smooth(method = "lm")+
  ylab("WPBR Infection Rate (%)") + xlab("Climate Strata") + 
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE) +
  guides(fill=F, color=F)


# slope and WPBR
summarydat2 <- piladat %>% 
  group_by(plotID, slope) %>% 
  summarize(numtrees = length(unique(treeNum, na.rm=T)), suminf = sum(infected),
            per_infected = (suminf/numtrees)*100) %>% 
  na.omit() %>% 
  ungroup() %>% 
  mutate(plot = row_number(), plot2 = as.character(plot))

perfig2 <- summarydat2 %>% 
  ggplot(aes(x=slope, y=per_infected, color = "infected", fill="infected"))+
  geom_point()+ geom_smooth(method="lm")+
  ylab("% infected WPBR") + xlab("Slope in degrees") + 
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE) +
  guides(fill=F, color=F)

perfig2

# aspect and WPBR
summarydat3 <- piladat %>% 
  group_by(plotID, aspect) %>% 
  summarize(numtrees = length(unique(treeNum, na.rm=T)), suminf = sum(infected),
            per_infected = (suminf/numtrees)*100) %>% 
  na.omit() %>% 
  ungroup() %>% 
  mutate(plot = row_number(), plot2 = as.character(plot))

perfig3 <- summarydat3 %>% 
  ggplot(aes(x=aspect, y=per_infected, color = "infected", fill="infected"))+
  geom_point()+ geom_smooth(method="lm")+
  ylab("% infected WPBR") + xlab("Slope in degrees") + 
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE) +
  guides(fill=F, color=F)

perfig3


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
  #rename(elevation = plot_elevation_ft) %>% 
  group_by(plotID, strata) %>% 
  summarize(numtrees = length(unique(treeNum, na.rm=T)), sumbeetles = sum(beetles),
            per_beetles = (sumbeetles/numtrees)*100) %>% 
  na.omit() %>% 
  ungroup() %>% 
  mutate(plot = row_number(), plot2 = as.character(plot))

# overall percent infected is around 26%
perbeetles <- summarydatbeetles %>% 
  summarize(meanbeetles = mean(per_beetles))

# plot summary results and save object
perfigb <- summarydatbeetles %>% 
  ggplot(aes(x=strata, y=per_beetles, color = "beetles", fill="beetles"))+
  geom_point()+ geom_smooth(method="lm")+
  ylab("Beetle Extent (%)") + xlab("Climate Strata") + 
  #scale_x_discrete(guide = guide_axis(angle = 90)) +
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE) +
  guides(fill=F, color=F)

# display summary results
perfigb

perfigelevation  + perfigb

# figure out a way to consider correlation given the two variables are not continuous

# display percent infected for high severity and random plots
# subset data into high severity and random plots

# calculate canker score
piladat <- piladat %>% 
  mutate (cs = 
            case_when(activeBoleCanker > 0 | inactiveBoleCanker > 0 ~ 5, 
                      activeBoleCanker == 0 & inactiveBoleCanker == 0 & activeBranchCanker == 0 & inactiveBranchCanker == 0 ~ 0,
                      activeBranchCanker > 0 & activeBranchCanker <= 3 | inactiveBranchCanker > 0 & inactiveBranchCanker <= 3 ~ 1,
                      activeBranchCanker > 3 & activeBranchCanker <= 9 | inactiveBranchCanker > 3 & inactiveBranchCanker <= 9 ~ 2
            )
  )
# calculate severity
piladat <- piladat %>% mutate(severity = 
                          case_when(cs == 0 ~ 0, 
                                    cs > 0 & DBH_cm <= 63.5 ~ cs + (63.5 - as.numeric(DBH_cm) / 5),
                                    cs > 0 & DBH_cm > 63.5 ~ cs + ((63.5 - 63.5) / 5)
                          )
)

# severity and infection rate histograms
severityHist <- piladat %>% ggplot(aes(x=severity))+ 
  geom_histogram() +
  xlab("WPBR Severity") +
  ggtitle ("Histogram of WPBR Severity")

IRhist <- summarydat %>% ggplot(aes(x= per_infected)) +
  geom_histogram() +
  xlab("WPBR Extent (%)") +
  ggtitle ("Histogram of WPBR Extent")

severityHist + IRhist
