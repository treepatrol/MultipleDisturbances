

# (2) YOSE Understory Data Read In and Transformation -------------------------
# Authors: Jenny Cribbs, Andrew Latimer, Joan Dudney
# Date: 13 April 2024

# Inputs: Raw data from the YOSE understory Google sheets 

# Code description: This code chunk reads in understory data from each plot then applies the understory data carpentry function (step 1) to each plot. As described in the step 1 header, the function pulls out all the species and "association" observations and makes them into a single, long-form data frame. 
# This data frame could then be filtered if we just want the pin-location data, not the associations, or if there are some categories we want to remove (litter, rock etc.)

# Output: Species observations in a long-form data frame, with one row per location-species combination and 4 columns (plotID, location, pin/associated, and code for what was present). 

# Load initial packages
# Run chunk 1 first or check that the user-defined function convert_understory_data_to_long is in the Environment window 
library(readxl)
library(tidyverse)

# Set the working directory if not done in step 1
#setwd("/Users/jennifercribbs/Documents/YOSE/Analysis/MultipleDisturbances/")
setwd("C:/Users/tazli/Downloads/YOSE_SugarPine/MultipleDisturbances")

# Part 1: Bring in understory data for each plot in the YPE_Data folder -----------

# setting the directory for data extraction


datadir <- "dataSandbox/RawData/YPE_Data"

# provide path for files in datadir
folders <- list.dirs(datadir, full.names = TRUE)[-c(1,4)] # Ensure full path names are used

# Initialize an empty list to store data for each plot
understory_list <- list()

# Loop through each folder
for (folder in folders) {
  # list all files in the folder
  files <- list.files(folder, pattern = "Understory", full.names = TRUE) # Understory data only
  
  # Loop through each file
  for(file in files) {
    # Read the Excel file
    data <- read_excel(file, na = "NA")
    
    # Extract the plot number from the file name
    plot_number <- tools::file_path_sans_ext(basename(file))
    
    # Store the data along with plot number in understory list
    understory_list[[plot_number]] <- data
  }
}


# Part 2: Transform data from wide to long format ---------------------------------

# We can "lapply()" the data carpentry function from step 1 to the list of data frames from all the plots. 
all_plots_understory <- lapply(understory_list, convert_understory_data_to_long)

# stick all dataframes in understory list into one df 
all_plots_understory <- do.call(rbind, all_plots_understory)

#remove duplicated associated species rows:
all_plots_understory <- all_plots_understory %>% 
  filter(pin_vs_assoc == "pin" | (pin_vs_assoc == "assoc" & dOut_m == 2))

# save as a csv file in the working directory
write.csv(all_plots_understory, "dataSandbox/RawData/UnderstoryDataLong.csv", row.names = FALSE) # don't save first column

