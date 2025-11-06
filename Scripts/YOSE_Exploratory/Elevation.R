
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
  theme_bw(base_size = 10)+
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
        dplyr::select(plotID, strata, plot_elevation_ft, treeNum, activeBranchCanker, inactiveBranchCanker, 
                      DBH_cm, height_m, activeBoleCanker, inactiveBoleCanker, DTOP, flags, percentLive)
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
tree_list$plot_elevation_ft <- as.numeric(tree_list$plot_elevation_ft)

# create new columns combining active and inactive bole and branch cankers 
piladat <- tree_list %>% 
  mutate(bole_canker = activeBoleCanker + inactiveBoleCanker,
         branch_canker = activeBranchCanker + inactiveBranchCanker) %>% 
  mutate(infected = ifelse(bole_canker|branch_canker>0,1,0))


# tally infected trees and calculate infection rate
summarydat <- piladat %>% 
  group_by(plotID) %>% 
  summarize(numtrees = length(unique(treeNum, na.rm=T)), suminf = sum(infected),
            per_infected = (suminf/numtrees)*100) %>% 
  na.omit() %>% 
  ungroup() 

# Statistical significance testing
# install.packages("ggpubr")


# plot summary results and save object
perfigelevationtree <- piladat %>% 
  ggplot(aes(x=plot_elevation_ft, y=infected, color = "infected", fill="infected"))+
  geom_point()+ geom_smooth(method = "lm")+
  ylab("WPBR Infection Rate (%)") + xlab("Elevation (ft)") + 
  scale_x_continuous(guide = guide_axis(angle = 90)) +
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE) +
  guides(fill=F, color=F)

# display summary results
perfigelevationtree

# tally infected trees and calculate percent infected by plot and elevation
summarydat <- piladat %>% 
  rename(elevation = plot_elevation_ft) %>% 
  group_by(plotID, elevation) %>% 
  summarize(numtrees = length(unique(treeNum, na.rm=T)), suminf = sum(infected),
            per_infected = (suminf/numtrees)*100) %>% 
  na.omit() %>% 
  ungroup() %>% 
  mutate(plot = row_number(), plot2 = as.character(plot))

# plot summary results and save object
perfigelevation <- summarydat %>% 
  ggplot(aes(x=elevation, y=per_infected, color = "infected", fill="infected"))+
  geom_point()+ geom_smooth(method = "lm")+
  ylab("WPBR Extent (%)") + xlab("Elevation (ft)") + 
  scale_x_continuous(guide = guide_axis(angle = 90)) +
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE) +
  guides(fill=F, color=F)

# display summary results
perfigelevation

perfigelevationtree + perfigelevation
pila <- pila %>% 
  mutate (cs = 
            case_when(activeBoleCanker > 0 | inactiveBoleCanker > 0 ~ 5, 
                      activeBoleCanker == 0 & inactiveBoleCanker == 0 & activeBranchCanker == 0 & inactiveBranchCanker == 0 ~ 0,
                      activeBranchCanker > 0 & activeBranchCanker <= 3 | inactiveBranchCanker > 0 & inactiveBranchCanker <= 3 ~ 1,
                      activeBranchCanker > 3 & activeBranchCanker <= 9 | inactiveBranchCanker > 3 & inactiveBranchCanker <= 9 ~ 2
            )
  )

hist(pila$inactiveBranchCanker, xlab = "Severity", main = "Histogram of WPBR Severity")