# GBIF: Occurence Tab for Associated Trees
# Authors: Jenny Cribbs, edited 11/04/2024 Tazlina Dentinger
# Date: 09 September 2024

# Overall Input: Read in Associated Tree data from Excel files downloaded fromGoogle Sheets using for loop
# Overall Output: Clean csv with a row for each associated tree with a unique occurrence ID and columns matching GBIF columns, so the output can be pasted into NPS template occurence tab

# Part1: Associated tree Data Import and Cleaning -------------------------

# Load initial packages
if (!require("librarian")) install.packages("librarian")
librarian::shelf(tidyverse, readxl, viridis, patchwork)
library(VIM)

# Set the working directory
setwd("C:/Users/tazli/Downloads/YOSE_SugarPine")


# Bring in the tree data for each plot in the YPE_Data folder -----------

# setting the directory for data extraction
datadir <- "C:/Users/tazli/Downloads/YOSE_SugarPine/DataRaw/YPE_Data"

# provide path for files in datadir
folders <- list.dirs(datadir, full.names = TRUE)[-c(1,4)] # Ensure full path names are used

# initialize an empty list to store data for each plot
tree_list <- data.frame() #pila_list var changed to tree_list

# loop through each folder
for (folder in folders) {
  # list all files in the folder
  files <- list.files(folder, pattern = "Tree") # choose Tree data only
  
   # Loop through each file
   for(file in files) {
     # Read the Excel file
     xlsfile <- read_excel(paste0(folder,"/",file), na = "NA") %>%
       mutate_if(is.numeric, as.numeric) %>% # keeps numbers numeric
       dplyr::select(plot, date, crew,
                     treeNum, species, DBH_cm, height_m,
                     dOut_m, dSideR_m, dSideL_m,
                     percentLive, damageCodes, notes
                     )
     tree_list <- rbind(tree_list, xlsfile)

  }
}

# check data types
summary(tree_list)

#fix plotID naming
tree_list <- tree_list %>% 
  rename(plotID = plot)

# find rows with NAs in specified columns. The comma is important for some reason.
#tempNAheight <- tree_list[is.na(tree_list$height_m),]

#make DBH, height and dSideL numeric - shouldn't be necessary if can remove ~values in data
#tree_list$DBH_cm <- as.numeric(tree_list$DBH_cm)
#tree_list$height_m <- as.numeric(tree_list$height_m)
tree_list$dSideL_m <- as.numeric(tree_list$dSideL_m)

# check missing data 
summary(aggr(tree_list))

# Combining dSideR_m and dSideL_m into a single dSide column with checks
tree_list <- tree_list %>%
  mutate(dSide = case_when(
    !is.na(dSideR_m) & dSideR_m >= 0 ~ dSideR_m,
    !is.na(dSideL_m) & dSideL_m <= 0 ~ dSideL_m,
    TRUE ~ NA_real_
  ))
summary(tree_list)

#check on NAs in new dSide column
#tempNAdSide <- tree_list[is.na(tree_list$dSide),]

# check species field
unique(tree_list$species)

#find PILAs - Get that out of here
tempPILA <- filter(tree_list, species == "PILA")

# transform species list to consolidate repeats
tree_list <- tree_list %>%
  mutate(species = case_when(
    species == "PYGE" ~ "PIJE",
    species %in% c("unknown", "UNK", "UNKNOWN", "Charcol", "Unknown") ~ "UNKNOWN",
    TRUE ~ species
  ))

#remove SALIX and CONU because they should be shrubs
tree_list <- tree_list %>% 
  filter(species != "CONU") %>% 
  filter(species != "SALIX")

# check damage codes field
unique(tree_list$damageCodes)

#Uppercase???
# transform damage codes list to and standardize break char and damage codes to FHW codes
tree_list$damageCodes <- gsub('BROKE', 'BROK',
                gsub('BTOP', 'BROKE', 
                     gsub('_', '|', 
                          gsub(', ', '|', 
                               gsub('CROOK', 'CROK', tree_list$damageCodes)))))

# write results to a csv
write.csv(tree_list, "CleanTreeList.csv")

# Part2: Reorganization for GBIF -------------------------

# rename columns to match occurrence tab template
occurrenceData <- tree_list %>% 
  rename(eventID = plotID,
         recordedBy = crew,
         taxonID = species)

# add GBIF columns to match occurrence tab template
occurrenceData <- occurrenceData %>% 
  mutate(occurrenceID = paste(eventID, treeNum, sep = "-"), 
         basisOfRecord = "HumanObservation",
         scientificName = case_when(
           species == "PIJE" ~ "Pinus jeffreyi",
           species == "ABCO" ~ "Abies concolor (Lindley, 1861)"
           species == "ABMA" ~ "Abies magnifica"
           species == "CADE" ~ "Calocedrus decurrens"
           species == "PIPO" ~ "Pinus ponderosa (Douglas, 1836)"
           species == "QUKE" ~ "Quercus kelloggii (Newberry, 1857)"
           species == "PSME" ~ "Pseudotsuga menziesii"
           species == "UNKNOWN" ~ "Pinales"
           species == "QUCH" ~ "Quercus chrysolepis"
           species == "QUWI" ~ "Quercus wislizeni"
           species == "ACMA" ~ "Acer macrophyllum"
           species == "UMCA" ~ "Umbellularia californica"
           species == "PICO" ~ "Pinus contorta"
           species == "JUOC" ~ "Juniperus occidentalis"
           species == "Abies_sp" ~ "Abies"
           ) #needs dates and namers
         recordedBy = gsub("_", "|", recordedBy),
         occurrenceStatus = "present", # change to absent for 7 and a few others
         individualCount = 1,
         organismQuantity = 1,
         organismQuantityType = "individual", 
         publicDisplay = "yes",
         dataAccess = "NA",
         lifeStage = "NA",
         sex = "NA",
         reproductiveCondition = if_else(str_detect(notes, "\\bcone(s)?\\b"), "cone-bearing", "no cones observed"), #does this apply for assoc?
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
gbifOccurrence <- occurrenceData %>% select(occurrenceID, eventID, basisofRecord, taxonID, scientificName, recordedBy, occurenceStatus, individualCount, organismQuantity, organismQuantityType, publicDisplay, dataAccess, lifeStage, sex, reproductiveCondition, behavior, covariateSample, preparations, associatedMedia, identifiedBy, dataIdentified, identificationReferences, identificationRemarks, identificationQualifier, identificationVerificationStatus, occurenceRemarks, materialSampleID, recordNumber, organismRemarks, identificationID, DBH_cm, height_m, percentLive)

# save as a csv file in the working directory
write.csv(tree_list, "TreeData_04112024.csv", row.names = FALSE) # don't save first column



