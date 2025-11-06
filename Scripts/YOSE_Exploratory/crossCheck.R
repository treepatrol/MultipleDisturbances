# read in UCD PILA and trees data from csv files
ucd <- read_csv("/Users/jennifercribbs/Documents/YOSE/YPE_PILA_UCDcopy.csv")
# read in UCSB PILA data

# set working directory
setwd("/Users/jennifercribbs/Documents/YOSE/Analysis/YPE_Data")

files <- list.files()
outdir <- "/Users/jennifercribbs/Documents/YOSE/Analysis/YPE_Data"

folders <- list.dirs(outdir)[-c(1,4)]

ucsb <- data.frame()

for (folder in folders) {
  newfiles = list.files(folder, pattern = "PILAdata")
  for(file in newfiles) {
    xlsfile <- read_excel(paste0(folder,"/",file)) %>% 
      dplyr::select(plotID, dOut_m, treeNum, DBH_cm, height_m, pitchTubes, exitHoles, activeBranchCanker, inactiveBranchCanker, 
                    activeBoleCanker, inactiveBoleCanker, DTOP, flags, percentLive)
    ucsb <- rbind(tree_list, xlsfile)
  }
}

nrow(ucd) == nrow(ucsb)

# how many plots in ucd?
glimpse(ucd)
unique(ucd$YPE_plot)
length(unique(ucd$YPE_plot))

# create new column plotID that takes the values from YPE_plot
# select variables to compare
ucd_clean <- ucd %>% mutate(plotID = YPE_plot) %>% 
  select(plotID, dOut_m, dSide_m, GPSpoint, UTM_N, UTM_E, estimatedAccuracy_ft, 
         treeNum, DBH_cm, height_m, # tree size
         activeBoleCanker, inactiveBoleCanker, # bole cankers
         activeBranchCanker, inactiveBranchCanker, # branch cankers
         DTOP, flags, percentLive, # other variable possibly related to wpbr
         pitchTubes, exitHoles, # beetle variables
         fireScar, damageCodes, notes) # other variables
# select variables to compare
ucsb_clean <- ucsb %>% 
  select(plotID, dOut_m,  
        treeNum, DBH_cm, height_m, 
        activeBoleCanker, inactiveBoleCanker, 
        activeBranchCanker, inactiveBranchCanker, 
        DTOP, flags, percentLive, 
        pitchTubes, exitHoles, 
        fireScar, damageCodes, notes)


  


# what about ucsb?
glimpse(ucsb)
unique(ucsb$plotID)
length(unique(ucsb$plotID))

# check that the ucd plots are a subset of the ucsb plots
# got code help from chatGPT, but verified with known examples
list_ucd <- unique(ucd$plotID)
list_ucsb <- unique(ucsb$plotID)  # Replace ... with the actual numbers

# Check if all numbers in list_ucd are present in list_ucsb
all_present <- all(list_ucd %in% list_ucsb)

# Print the result
print(all_present)

# break data into ucd plots


# Outline of a function from chatGPT
crossCheckData <- function(plotIDs, data1, data2) {
  # Input:
  # - plotIDs: Vector of plot IDs for which to perform the cross-check
  # - data1: Data frame or tibble for the first version of the data
  # - data2: Data frame or tibble for the second version of the data
  
  # Initialize an empty list to store discrepancies
  discrepancies <- list()
  
  # Loop through each plot ID
  for (plotID in plotIDs) {
    # Subset data for the current plot ID
    subsetData1 <- data1[data1$plot_ID == plotID, ]
    subsetData2 <- data2[data2$plot_ID == plotID, ]
    
    # Compare corresponding entries for each variable
    for (col in names(data1)) {
      # Check if the columns match
      if (!all(subsetData1[[col]] == subsetData2[[col]])) {
        # If not, store the discrepancy information
        discrepancyInfo <- list(
          plot_ID = plotID,
          column = col,
          data1_values = subsetData1[[col]],
          data2_values = subsetData2[[col]]
        )
        discrepancies[[length(discrepancies) + 1]] <- discrepancyInfo
      }
    }
  }
  
  # Return the list of discrepancies
  return(discrepancies)
}

# Example usage:
# Replace 'your_plot_ids', 'your_data1', and 'your_data2' with your actual data
plotIDs <- c("plot1", "plot2", "plot3")  # Replace with your plot IDs
discrepancies <- crossCheckData(plotIDs, your_data1, your_data2)

# Print or inspect the discrepancies
print(discrepancies)

######

summary(ucsb)
class(ucsb$DBH_cm)
as.numeric(ucsb$DBH_cm)
hist(ucsb$DBH_cm)
mean(ucsb$DBH_cm)

summary(ucd_clean)
