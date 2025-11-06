# GBIF: Occurence Tab for Associated Trees
# Authors: Jenny Cribbs, Tazlina Dentinger
# Date created: 09 September 2024
# Updated: edited 11/04/2024 TMD, edited 12/04/2024 JEC

# Overall Input: Read in Associated Tree data from Excel files downloaded fromGoogle Sheets using for loop
# Overall Output: Clean csv with a row for each associated tree with a unique occurrence ID and columns matching GBIF columns, so the output can be pasted into NPS template occurence tab

# Part1: Associated tree Data Import and Cleaning -------------------------

# Load initial packages
if (!require("librarian")) install.packages("librarian")
librarian::shelf(tidyverse, readxl, viridis, patchwork)
library(VIM)

# Set the working directory
setwd("/Users/jennifercribbs/Documents/YOSE/Analysis/MultipleDisturbances/")


# Bring in the tree data for each plot in the YPE_Data folder -----------

# setting the directory for data extraction
datadir <- "/Users/jennifercribbs/Documents/YOSE/Analysis/MultipleDisturbances/Data/RawData/YPE_Data"

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

# fix plotID naming
tree_list <- tree_list %>% 
  rename(plotID = plot)
# convert dSide Left to numeric 
tree_list$dSideL_m <- as.numeric(tree_list$dSideL_m)

# combine dSideR_m and dSideL_m into a single dSide column with checks
tree_list <- tree_list %>%
  mutate(dSide = case_when(
    !is.na(dSideR_m) & dSideR_m >= 0 ~ dSideR_m,
    !is.na(dSideL_m) & dSideL_m <= 0 ~ dSideL_m,
    TRUE ~ NA_real_
  ))
summary(tree_list)

# check species field
unique(tree_list$species)

# transform species list to consolidate repeats
tree_list <- tree_list %>%
  mutate(species = case_when(
    species == "PYGE" ~ "PIJE", # correct misspelling in data entry
    species %in% c("unknown", "UNK", "UNKNOWN", "Charcol", "Unknown") ~ "UNKNOWN",
    TRUE ~ species
  ))

# check species field again
unique(tree_list$species)

# remove SALIX and CONU because they should be shrubs 
# Although some are tree-like, these species were likely not assessed as trees uniformly across the plots
tree_list <- tree_list %>% 
  filter(species != "CONU") %>% 
  filter(species != "SALIX") # removes only 2 records total (3309 to 3307)

# check damage codes field
unique(tree_list$damageCodes)

#add FIRE code, maybe others, based on notes using 

# then need to concatenate with damageCodes column

# make damage codes uppercase
tree_list <- tree_list %>% 
  mutate(damageCodes = toupper(damageCodes))

#start with empty column and use separate if_else statements for each term 
tree_list <- tree_list %>% 
  mutate(fireDamage = "")
#ensure not case sensitive
tree_list <- tree_list %>% 
  mutate(fireDamage = if_else(str_detect(notes, "char"), "FIRE", "", missing = ""),
         )
         # fireDamage = if_else(str_detect(notes, "crisp"), "FIRE", "", missing = ""),
         # fireDamage = if_else(str_detect(notes, "fire"), "FIRE", "", missing = ""),
         # fireDamage = if_else(str_detect(notes, "burn"), "FIRE", "", missing = ""),
         # fireDamage = if_else(str_detect(notes, " fs"), "FIRE", "", missing = ""),
         # fireDamage = if_else(str_detect(notes, "fs "), "FIRE", "", missing = ""),
         # fireDamage = if_else(str_detect(notes, "cat face"), "FIRE", "", missing = ""))


#check fire damage column - also need to check for false positive FIRE, then remove
unique(tree_list$fireDamage)

treeFire <- filter(tree_list, fireDamage == "FIRE")

# transform damage codes list to and standardize break char and damage codes to FHW codes
tree_list$damageCodes <- gsub('BROKE', 'BROK',
                gsub('BTOP', 'BROKE', 
                     gsub('_', '|', 
                          gsub(', ', '|', 
                               gsub('CROOK', 'CROK', tree_list$damageCodes)))))

# write results to a csv in case GBIF format is not desirable
write.csv(tree_list, "CleanTreeList.csv")

# Part2: Reorganization for GBIF -------------------------

# rename columns to match occurrence tab template
treeOccurrenceData <- tree_list %>% 
  rename(eventID = plotID,
         recordedBy = crew,
         taxonID = species,
         diameter = DBH_cm,
         height = height_m)

# add GBIF columns to match occurrence tab template
treeOccurrenceData <- treeOccurrenceData %>% 
  mutate(occurrenceID = paste("E", eventID, "-", "Tree", treeNum, sep = ""),  #may need to update occurrenceID so the PILA and Trees don't overlap--> adding "Tree" for now
         basisOfRecord = "HumanObservation",
         scientificName = case_when(
           taxonID == "PIJE" ~ "Pinus jeffreyi (Balf. 1853)",
           taxonID == "ABCO" ~ "Abies concolor (Lindley, 1861)",
           taxonID == "ABMA" ~ "Abies magnifica",
           taxonID == "CADE" ~ "Calocedrus decurrens",
           taxonID == "PIPO" ~ "Pinus ponderosa (Douglas, 1836)",
           taxonID == "QUKE" ~ "Quercus kelloggii (Newberry, 1857)",
           taxonID == "PSME" ~ "Pseudotsuga menziesii",
           taxonID == "UNKNOWN" ~ "Pinales", # check that we're not mislabeling any oaks etc. 
           taxonID == "QUCH" ~ "Quercus chrysolepis",
           taxonID == "QUWI" ~ "Quercus wislizeni",
           taxonID == "ACMA" ~ "Acer macrophyllum",
           taxonID == "UMCA" ~ "Umbellularia californica",
           taxonID == "PICO" ~ "Pinus contorta",
           taxonID == "JUOC" ~ "Juniperus occidentalis",
           taxonID == "Abies_sp" ~ "Abies"), #needs dates and names (GBIF)
         recordedBy = gsub("_", "|", recordedBy),
         occurrenceStatus = "present",
         individualCount = 1,
         organismQuantity = 1,
         organismQuantityType = "individual", 
         publicDisplay = "yes",
         dataAccess = "",
         lifeStage = "",
         sex = "",
         reproductiveCondition = "",
         behavior = "",
         covariateSample = "",
         preparations = "",
         identifiedBy = recordedBy,
         dateIdentified = date,
         identificationReferences = "",
         identificationRemarks = "",
         identificationQualifier = "",
         identificationVerificationStatus = "",
         occurrenceRemarks = notes,
         materialSampleID = "",
         recordNumber = "",
         organismRemarks = "",
         identificationID = "",
         pitchTubes = "",
         exitHoles = "",
         activeBranchCanker = "",
         inactiveBranchCanker = "",
         deadTop = if_else((str_detect(damageCodes, "\\bDTOP\\b") | (str_detect(damageCodes, "\\bSD\\b"))), "Y", "N", missing = "N"),
         boleChar = if_else(str_detect(damageCodes, "\\bFIRE\\b"), "Y", "N", missing = "N"))


# select columns for GBIF occurrence tab
gbifTreeOccurrence <- treeOccurrenceData %>% 
  select(occurrenceID, eventID, basisOfRecord, taxonID, scientificName, recordedBy, occurrenceStatus, individualCount, organismQuantity, organismQuantityType, publicDisplay, dataAccess, lifeStage, sex, reproductiveCondition, behavior, covariateSample, preparations, identifiedBy, dateIdentified, identificationReferences, identificationRemarks, identificationQualifier, identificationVerificationStatus, occurrenceRemarks, materialSampleID, recordNumber, organismRemarks, identificationID, diameter, height, 
         pitchTubes, exitHoles, activeBranchCanker, inactiveBranchCanker, damageCodes, deadTop, percentLive, boleChar)

# save as a csv file in the working directory
write.csv(gbifTreeOccurrence, "TreeData_20241204.csv", row.names = FALSE) # don't save first column



