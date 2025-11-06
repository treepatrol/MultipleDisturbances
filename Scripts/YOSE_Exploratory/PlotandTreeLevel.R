## March 3rd 2024
## Jenny Cribbs
## Code brings in data from YOSE database and plot level prism data from csv
## Averages fire severity variable
## Runs PCA for plot and prism variables
## Product: Sierra Nevada Science Symposium Supplemental Figures


# Set the working directory
setwd("/Users/jennifercribbs/Documents/YOSE/Analysis/MultipleDisturbances/Data/RawData/YPE_Data")

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
      dplyr::select(plotID, trans_length, width, plot_elevation_ft, slope, aspect, plot_type,
                    fireseverity_50m, fireseverity_100m, fireseverity_150m, fireseverity_200m, 
                    ribes_50m, ribes_100m, ribes_150m, ribes_200m, 
                    DBH_cm, height_m, 
                    pitchTubes, exitHoles, 
                    activeBranchCanker, inactiveBranchCanker, 
                    activeBoleCanker, inactiveBoleCanker, 
                    DTOP, flags, percentLive)
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
tree_list$plot_elevation_ft <- as.numeric(tree_list$plot_elevation_ft)
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
# Recode fireseverity_50m
piladat <- piladat %>% 
  mutate(fireseverity_50m = recode(fireseverity_50m,
                                   "N" = 0,
                                   "None" = 0,
                                   "NONE" = 0,
                                   "unburned" = 0,
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
                                    "None" = 0,
                                    "NONE" = 0,
                                    "unburned" = 0,
                                    "low" = 1,
                                    "low_mod" = 2,
                                    "mod" = 2,
                                    "moderate" = 2,
                                    "mod_high" = 3,
                                    "high" = 3,
                                    "high/mixed" = 3,
                                    "mixed" = 2))
# Recode fireseverity_150m
piladat <- piladat %>% mutate(fireseverity_150m = recode(fireseverity_150m,
                                  "N" = 0,
                                  "None" = 0,
                                  "NONE" = 0,
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
piladat <- piladat %>% mutate(fireseverity_200m = recode(fireseverity_200m,
                                  "N" = 0,
                                  "None" = 0,
                                  "NONE" = 0,
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

# create waypoints for export for prism data extraction 
waypoints <- read_csv("/Users/jennifercribbs/Documents/YOSE/Plot_Waypoints_UTMzone.csv")
plotlist <- unique(piladat$plotID)
plotlist %>% arrange(plotID)
waypoints$plotID


# bring in PRISM data
prism <- read_csv("/Users/jennifercribbs/Downloads/updatedPRISMdata.csv")

# aggregate average fire severity at the plot level
average_fire <- piladat %>%
  group_by(plotID) %>%
  summarize(avg_fire_severity = mean(fireseverity)) %>% 
  ungroup()

# join tree data and prism data on plotID 
climate_fire <- prism %>% left_join(average_fire, by = c("PlotID" = "plotID"))

# look at the distribution of climate variables 
hist(climate_fire$vpdmax)
hist(climate_fire$tmean)
hist(climate_fire$ppt)
hist(climate_fire$avg_fire_severity)

# aggregate elevation, slope, aspect at the plot level
average_plotVars <- piladat %>%
  group_by(plotID) %>%
  summarize(elevation = mean(plot_elevation_ft),
            aspect = mean(aspect),
            slope = mean(slope)) %>% 
  ungroup()

climate_fire <- climate_fire %>% left_join(average_plotVars, by = c("PlotID" = "plotID"))

# look at the distribution of the plot variables
climate_fire$elevation <- as.numeric(climate_fire$elevation)
hist(climate_fire$elevation)
hist(climate_fire$slope)
hist(climate_fire$aspect)

# write.csv(climate_fire, "Climate.Fire.csv")
# read back in after manual corrections for missing elevation, slope, and aspect
climate_fire <- read_csv("Climate.Fire.csv")


# aspect is circular, so convert to two linear variables
# Convert aspect to radians
climate_fire$aspect_rad <- (climate_fire$aspect / 180) * pi

# Create new linear variables for sine and cosine of aspect
climate_fire$aspect_sin <- sin(climate_fire$aspect_rad)
climate_fire$aspect_cos <- cos(climate_fire$aspect_rad)

# standardize all variables and run PCA
pcaplot <- prcomp(~ vpdmax + tmean + ppt + elevation + aspect_sin + aspect_cos + slope + avg_fire_severity,
                  center = TRUE, scale = TRUE, data = climate_fire, na.action = na.exclude)

class(pcaplot)
summary(pcaplot) # PC1 explains 42% of the variation PC 1-4 explains 83%
#the loadings of the first component
sort(pcaplot$rotation[,1]) # negative for ppt, elevation, slope; positive for tmean, vpdmax
sort(pcaplot$rotation[,2]) # negative for aspect and elevation; positive for ppt and fire severity
sort(pcaplot$rotation[,3]) # strongly negative for slope
sort(pcaplot$rotation[,4]) # fire severity, elevation, aspect 

# Make a table
library(flextable)

# Extract loadings 
loadings <- as.data.frame(pcaplot$rotation[,1:8])
# ensure that original variable names display in table
loadings <- cbind(Variables = rownames(pcaplot$rotation)[1:8], loadings)
# Convert to flextable
ft <- flextable(loadings)
# Remove row names
row.names(loadings) <- NULL
# Print the flextable
ft




#install.packages("factoextra")
library(factoextra)
# Visualize PC1 and PC2
fviz_pca_var(pcaplot, axes = c(1, 2), col.var = "steelblue", title = "Variables - PCA 1 and PCA 2")
fviz_pca_var(pcaplot, axes = c(1, 3), col.var = "orangered", title = "Variables - PCA 1 and PCA 3")
fviz_pca_var(pcaplot, axes = c(2, 3), col.var = "forestgreen", title = "Variables - PCA 2 and PCA 3")

# visualize in 3D
#install.packages("plotly")
library(plotly)

# Extract PCA results for the first five components
pc <- as.data.frame(pcaplot$x[,1:5])
# view in table
pc %>% flextable()

# Create a 3D scatterplot--cool, but what does it mean???
plot_ly(data = pc, x = ~PC1, y = ~PC2, z = ~PC3, type = "scatter3d", mode = "markers", marker = list(color = "steelblue"))

# Selecting PC components
# look for an elbow in the scree plot
fviz_eig(pcaplot) # no clear elbow--keep PC 1 and 2?
# Kaiser criterion 
eigen <- get_eigenvalue(pcaplot) # PC 1, 2, 3 > 1, but three just barely
# view in table
eigen %>% flextable()
# Need to retain 5 components to explain over 80% of the variation


