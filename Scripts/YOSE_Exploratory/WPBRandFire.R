# An interaction analysis investigates whether the relationship between two variables varies across different levels of another variable
if (!require("librarian")) install.packages("librarian")
librarian::shelf(tidyverse, readxl, viridis, patchwork)

theme_set(
  theme_bw(base_size = 17)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
)

files <- list.files()
outdir <- "/Users/jennifercribbs/Documents/YOSE/Analysis/YPE_Data"

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
      dplyr::select(plotID, plot_type, DBH_cm, height_m, pitchTubes, exitHoles, activeBranchCanker, inactiveBranchCanker, 
                    activeBoleCanker, inactiveBoleCanker, DTOP, flags, percentLive)
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

hist(piladat$infected)
# display percent infected by plot type
boxplot(infected ~ plot_type, piladat, xlab = "High Fire Severity versus Random", ylab = "% WPBR")
# t.test(plot_type ~ infected, piladat)

library(ggpubr)
summarydat %>% 
  ggplot(aes(x=plot_type, y=per_infected, color = "infected", fill="infected"))+
  geom_boxplot() +
  ylab("% infected") + xlab("plot type") + 
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE) +
  guides(fill=F, color=F) +
  stat_compare_means( label = "p.signif", label.x = 1.5, label.y = 40)


# plot summary results and save object
perfighs <- hs %>% 
  ggplot(aes(x=strata, y=per_infected, color = "infected", fill="infected"))+
  geom_point()+ geom_smooth(method="lm")+
  ylab("% infected") + xlab("climate strata") + 
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE) +
  guides(fill=F, color=F)

boxplot(summarydat$random, summarydat$per_infected)


