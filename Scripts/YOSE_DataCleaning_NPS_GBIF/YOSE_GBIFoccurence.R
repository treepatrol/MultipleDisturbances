# GBIF: Occurence Tab
# Authors: Jenny Cribbs
# Date: 09 September 2024

# Overall Input: Read in PILA data from Google Sheets using for loop
# Overall Output: Clean csv with a row for each sugar pine with a unique occurrence ID and columns matching GBIF columns, so the output can be pasted into NPS template occurence tab

# Part1: YOSE PILA Data Import -------------------------

# Load initial packages
if (!require("librarian")) install.packages("librarian")
librarian::shelf(tidyverse, readxl, viridis, patchwork)
library(VIM)

# Set the working directory
setwd("/Users/jennifercribbs/Documents/YOSE/Analysis/MultipleDisturbances/")


# Bring in the PILA data for each plot in the YPE_Data folder -----------

# setting the directory for data extraction
datadir <- "/Users/jennifercribbs/Documents/YOSE/Analysis/MultipleDisturbances/Data/RawData/YPE_Data"

# provide path for files in datadir
folders <- list.dirs(datadir, full.names = TRUE)[-c(1,4)] # Ensure full path names are used

# initialize an empty list to store data for each plot
pila_list <- data.frame()

# loop through each folder
for (folder in folders) {
  # list all files in the folder
  files <- list.files(folder, pattern = "PILA") # choose PILA data only
  
  # Loop through each file
  for(file in files) {
    # Read the Excel file
    xlsfile <- read_excel(paste0(folder,"/",file), na = "NA") %>%
      mutate_if(is.numeric, as.numeric) %>% # keeps numbers numeric
      dplyr::select(plotID, plot_type, date, crew, trans_length, width, 
                    slope, aspect, plot_azimuth,
                    plot_beg_UTM_N, plot_beg_UTM_E, 
                    plot_end_UTM_N, plot_end_UTM_E, 
                    plot_notes, plot_elevation_ft, 
                    ribes_50m, ribes_100m, ribes_150m, ribes_200m, 
                    seedlings_50m, seedlings_100m, seedlings_150m, seedlings_200m,
                    treeNum, species, DBH_cm, est_DBH_cm, height_m, est_height_m,
                    pitchTubes, exitHoles,
                    activeBranchCanker, inactiveBranchCanker,
                    activeBoleCanker, inactiveBoleCanker, flags, DTOP,
                    percentLive, fire_scar, resistance, damageCodes, notes 
                    )
    pila_list <- rbind(pila_list, xlsfile)
    
  }
}

# check data types
summary(pila_list)
# check missing data 
summary(aggr(pila_list))

# remove row with no data
pila_list <- pila_list %>% filter(!is.na(plotID))

# rename columns to match template style
occurrenceData <- pila_list %>% 
  rename(eventID = plotID,
         recordedBy = crew,
         diameter = DBH_cm,
         height = height_m,
         deadTop = DTOP, 
         boleChar = fire_scar)

# add GBIF columns to match occurrence tab template
occurrenceData <- occurrenceData %>% 
  mutate(occurrenceID = paste(eventID, treeNum, sep = "_"), 
         basisOfRecord = "HumanObservation",
         taxonID = "PILA",
         scientificName = "Pinus lambertiana (Douglas, 1827)",
         recordedBy = gsub("_", "|", recordedBy),
         occurrenceStatus = "present", # change to absent for YPE7, 8, and 25
         individualCount = 1,
         organismQuantity = 1,
         organismQuantityType = "individual", 
         publicDisplay = "yes",
         dataAccess = "NA",
         lifeStage = "NA",
         sex = "NA",
         reproductiveCondition = if_else(str_detect(notes, "\\bcone(s)?\\b"), "cone-bearing", "no cones observed"),
         behavior = "NA",
         covariateSample = "NA",
         preparations = "NA",
         footprintWKT = "NA",
         footprintSRS = "NA", 
         associatedMedia = "NA",
         identifiedBy = recordedBy,
         dateIdentified = date,
         identificationReferences = "NA",
         identificationRemarks = "NA",
         identificationQualifier = "NA",
         identificationVerificationStatus = "NA",
         occurrenceRemarks = notes,
         materialSampleID = "NA",
         recordNumber = "NA",
         organismRemarks = "NA",
         identificationID = "NA")



# select columns for GBIF occurrence tab
gbifExcelOccurrence <- occurrenceData %>% 
  select(occurrenceID, eventID, basisOfRecord, taxonID, scientificName, recordedBy, occurrenceStatus, individualCount, organismQuantity, organismQuantityType, publicDisplay, dataAccess, lifeStage, sex, reproductiveCondition, behavior, covariateSample, preparations, identifiedBy, dateIdentified, identificationReferences, identificationRemarks, identificationQualifier, identificationVerificationStatus, occurrenceRemarks, materialSampleID, recordNumber, organismRemarks, identificationID, diameter, height, pitchTubes, exitHoles, activeBranchCanker, inactiveBranchCanker, activeBoleCanker, inactiveBoleCanker, deadTop, percentLive, boleChar)

# save as a csv file in the working directory
write.csv(gbifExcelOccurrence, "gbifExcelOccurence.csv", row.names = FALSE) # don't save first column



