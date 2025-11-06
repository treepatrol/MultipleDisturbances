## Jenny Cribbs
## Code brings in data from YOSE database 
## Product: CSV file with full tree data


# Set the working directory
setwd("/Users/jennifercribbs/Documents/YOSE/Analysis/MultipleDisturbances/")

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
  newfiles = list.files(folder, pattern = "Treedata") # Tree data only
  for(file in newfiles) {
    xlsfile <- read_excel(paste0(folder,"/",file), na = "NA") %>% 
      dplyr::select(plot, date, crew, treeNum, species, 
                    dOut_m, dSideR_m, dSideL_m,
                    DBH_cm, height_m, percentLive, damageCodes, notes)
    tree_list <- rbind(tree_list, xlsfile)
  }
}
# check data types
summary(tree_list)
# still need to convert dOutL_m, DBH_cm, and height_m or figure out what is wrong

write_csv(tree_list, "Data/CleanData/TreeData.csv")

# implement a for loop to determine length and width for each associated tree plot
for (i in 1:59) {
  plot_data <- tree_list %>% filter(plot == i)
  treePlotLength[i] <- max(plot_data$dOut_m, na.rm = TRUE)
  treePlotWidth[i] <- max(plot_data$dSideR_m, na.rm = TRUE)
}

  