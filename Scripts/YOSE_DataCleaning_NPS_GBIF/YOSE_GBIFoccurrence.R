# GBIF: Occurrence Tab for PILA and Associated Trees
# Authors: Jenny Cribbs, Tazlina Dentinger
# Date created: 06 December 2024
# Updated: 10 February 2025, TMD

# Overall Input: Read in PILA and Associated Tree data from Excel files downloaded from Google Sheets using for loop
# Code Description: clean data with a row for each tree with a unique occurrence ID and columns matching GBIF columns, so the output can be pasted into NPS template occurrence tab
# Overall Output:

# Part1: YOSE PILA Data Import -------------------------

# Load initial packages
if (!require("librarian")) install.packages("librarian")
librarian::shelf(tidyverse, readxl, viridis, patchwork)
library(VIM)
library(dplyr)
library(tidyverse)
library(lubridate)

# Set the working directory
setwd("/Users/jennifercribbs/Documents/YOSE/Analysis/MultipleDisturbances/")
#setwd("/Users/tazli/Downloads/YOSE_SugarPine/MultipleDisturbances")

# Bring in the PILA data for each plot in the project folder from Google Sheets
# setting the directory for data extraction--change to your local data directory
datadir <- here("dataSandbox/RawData/YPE_Data")

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
      dplyr::select(plotID, crew, date, treeNum, species, 
                    DBH_cm, est_DBH_cm, height_m, est_height_m,
                    pitchTubes, exitHoles,
                    activeBranchCanker, inactiveBranchCanker,
                    activeBoleCanker, inactiveBoleCanker, 
                    flags, DTOP,
                    percentLive, fire_scar, resistance,
                    damageCodes, notes
      )
    pila_list <- rbind(pila_list, xlsfile)
    
  }
}

# check data types
summary(pila_list)

pila_WPBR_na <- pila_list %>% filter(is.na(activeBoleCanker))

# Part2: PILA Data Wrangling -------------------------

# Ensure all values include a time
pila_list <- pila_list %>%
  mutate(date_time = ifelse(grepl(":", date), date, paste0(date, " 00:00")))

# rename columns to match template style
pila_list <- pila_list %>% 
  rename(eventID = plotID,
         recordedBy = crew,
         diameter = DBH_cm,
         height = height_m,
         deadTop = DTOP, 
         boleChar = fire_scar)

#------DAMAGE CODES-------- 
#scan notes for damage codes and indicators thereof, create new columns for each code, then paste into the damageCodes column in tree_list

# make damage codes uppercase
pila_list <- pila_list %>% 
  mutate(damageCodes = toupper(damageCodes))

# transform damage codes list to and standardize break char and damage codes to FHW codes
pila_list$damageCodes <- gsub('BROKE', 'BROK',
                              gsub('BTOP', 'BROK', # change to BROK after verifying BTOPs
                                   gsub('_', '|', 
                                        gsub(', ', '|', 
                                             gsub('CROOK', 'CROK', pila_list$damageCodes)))))

#create empty columns for damage codes that need adding or changing 
pila_list <- mutate(pila_list,
         birdDamage = "",  brokDamage = "",  bromDamage = "",
         crokDamage = "",  fireDamage = "",  forkDamage = "",   
         leanDamage = "",  mammDamage = "",  mechDamage = "",   
         sdDamage = "",    sparDamage = "",  twinDamage = "")

#add damage codes that are not accounted for in the column based on the notes
pila_list <- pila_list %>% 
  mutate(birdDamage = if_else(str_detect(notes, "(?i)sap\\s+sucker") | str_detect(notes, "(?i)sucker") | (str_detect(notes, "(?i)wp") & !str_detect(notes, "WPBR")) | str_detect(notes, "(?i)w\\s+p") | str_detect(notes, "(?i)bird") | str_detect(notes, "(?i)peck"), "BIRD", "", missing = ""),
    brokDamage = if_else(str_detect(notes, "(?i)brok") | str_detect(notes, "(?i)btop"), "BROK", "", missing = ""),
    bromDamage = if_else(str_detect(notes, "(?i)broom"), "BROM", "", missing = ""),
    crokDamage = if_else(str_detect(notes, "(?i)crok") | str_detect(notes, "(?i)crook"), "CROK", "", missing = ""),
    fireDamage = if_else(str_detect(notes, "(?i)char") | str_detect(notes, "(?i)torched") | str_detect(notes, "(?i)crisp") | str_detect(notes, "(?i)fire") | str_detect(notes, "(?i)burn") | str_detect(notes, "(?i)fs") | str_detect(notes, "(?i)cat\\s+face"), "FIRE", "", missing = ""),
    forkDamage = if_else(str_detect(notes, "(?i)fork"), "FORK", "", missing = ""), 
    mammDamage = if_else(str_detect(notes, "(?i)rodent") | str_detect(notes, "(?i)bear" ), "MAMM", "", missing = ""),
    sdDamage   = if_else(str_detect(notes, "(?i)sd"), "SD", "", missing = ""),
    sparDamage = if_else(str_detect(notes, "(?i)spars") | str_detect(notes, "(?i) thin") | str_detect(notes, "(?i)thin ") | str_detect(notes, "(?i)thin,"), "SPAR", "", missing = ""),
    twinDamage = if_else(str_detect(notes, "(?i)twin"), "TWIN", "", missing = "")
    )

pila_list <- pila_list %>% 
  mutate(fireDamage = case_when(
    suppressWarnings(as.numeric(boleChar) > 0) ~ "FIRE",
    TRUE ~ fireDamage
  ))


#remove false positives in damage column, e.g. "offshoot", "no fire scar", "yellow pitch"
pila_list <- pila_list %>%
  mutate(damageCodes = case_when(
    (eventID == 12 & treeNum == 22) ~ "",
    (eventID == 76 & treeNum == 4) ~ "", #change row that had the character string "NA" and "N"
    TRUE ~ damageCodes),
    birdDamage = case_when(
      (eventID == 3 & treeNum == 27) ~ "",     (eventID == 4 & treeNum == 15) ~ "",
      (eventID == 6 & treeNum == 31) ~ "",     (eventID == 10 & treeNum == 21) ~ "",
      (eventID == 18 & treeNum == 5) ~ "",     (eventID == 19 & treeNum == 14) ~ "",
      (eventID == 25 & treeNum == 54) ~ "",    (eventID == 25 & treeNum == 55) ~ "",
      (eventID == 34 & treeNum == 29) ~ "",    (eventID == 35 & treeNum == 2) ~ "",
      (eventID == 36 & treeNum == 63) ~ "",    (eventID == 56 & treeNum == 15) ~ "",
      (eventID == 68 & treeNum == 40) ~ "",    (eventID == 70 & treeNum == 5) ~ "", 
      (eventID == 74 & treeNum == 2) ~ "",     (eventID == 74 & treeNum == 21) ~ "",
      TRUE ~ birdDamage),
    brokDamage = case_when(
      (eventID == 30 & treeNum == 1) ~ "",     (eventID == 32 & treeNum == 9) ~ "",
      (eventID == 54 & treeNum == 11) ~ "",    (eventID == 55 & treeNum == 16) ~ "",
      (eventID == 56 & treeNum == 10) ~ "",    (eventID == 59 & treeNum == 9) ~ "",
      (eventID == 74 & treeNum == 8) ~ "",     (eventID == 74 & treeNum == 41) ~ "",
      (eventID == 76 & treeNum == 1) ~ "",     (eventID == 76 & treeNum == 24) ~ "",
      (eventID == 9) ~ "",
      TRUE ~ brokDamage),
    crokDamage = case_when(
      (eventID == 34 & treeNum == 21) ~ "",
      TRUE ~ crokDamage),
    fireDamage = case_when(
      (eventID == 49 & treeNum == 32) ~ "",   (eventID == 56 & treeNum == 16) ~ "",
      str_detect(notes, "(?i)no\\s+fire\\s+scar") ~ "",
      TRUE ~ fireDamage),
    sparDamage = case_when(
      (eventID == 17 & treeNum == 25) ~ "",   (eventID == 21 & treeNum == 5) ~ "",
      (eventID == 25 & treeNum == 6) ~ "",    (eventID == 25 & treeNum == 46) ~ "",
      (eventID == 55 & treeNum == 7) ~ "",    (eventID == 55 & treeNum == 8) ~ "",
      (eventID == 59 & treeNum == 15) ~ "",
      TRUE ~ sparDamage),
    twinDamage = case_when(
      (eventID == 25 & treeNum == 52) ~ "",   (eventID == 28 & treeNum == 4) ~ "",
      TRUE ~ twinDamage)
  )

#add in handful of missed codes 
pila_list <- pila_list %>%
  mutate(
    forkDamage = case_when(
      (eventID == 25 & treeNum == 52) ~ "LEAN",
      TRUE ~ forkDamage),
    leanDamage = case_when(
      (eventID == 13 & treeNum == 10) ~ "LEAN",   (eventID == 19 & treeNum == 37) ~ "LEAN",
      (eventID == 30 & treeNum == 4) ~ "LEAN",
      TRUE ~ leanDamage),
    mechDamage = case_when(
      (eventID == 18 & treeNum == 26) ~ "MECH",   (eventID == 19 & treeNum == 35) ~ "MECH",
      (eventID == 25 & treeNum == 8) ~ "MECH",    (eventID == 25 & treeNum == 39) ~ "MECH",
      (eventID == 39 & treeNum == 37) ~ "MECH",   (eventID == 56 & treeNum == 33) ~ "MECH",
      (eventID == 72 & treeNum == 27) ~ "MECH",   (eventID == 76 & treeNum == 19) ~ "MECH",
      TRUE ~ mechDamage),
    sdDamage = case_when(
      (eventID == 15 & treeNum == 13) ~ "SD",     (eventID == 25 & treeNum == 15) ~ "SD",
      (eventID == 56 & treeNum == 21) ~ "SD",
      TRUE ~ sdDamage)
  )

#paste values from separate damage columns into damageCodes
#this currently creates a few duplicate codes in the damageCodes column. This could eventually be fixed with some conditionals.
pila_list <- pila_list %>% 
  mutate(damageCodes = case_when(
    (birdDamage != "") ~ paste(damageCodes, birdDamage, sep = "|"),
    (brokDamage != "") ~ paste(damageCodes, brokDamage, sep = "|"),
    (bromDamage != "") ~ paste(damageCodes, bromDamage, sep = "|"),
    (crokDamage != "") ~ paste(damageCodes, crokDamage, sep = "|"),
    (fireDamage != "") ~ paste(damageCodes, fireDamage, sep = "|"),
    (forkDamage != "") ~ paste(damageCodes, forkDamage, sep = "|"),
    (mammDamage != "") ~ paste(damageCodes, mammDamage, sep = "|"),
      (sdDamage != "") ~ paste(damageCodes,   sdDamage, sep = "|"),
    (sparDamage != "") ~ paste(damageCodes, sparDamage, sep = "|"),
    (twinDamage != "") ~ paste(damageCodes, twinDamage, sep = "|"),
    TRUE ~ damageCodes))

#add DTOPs from deadTop column to the damageCodes column. 
pila_list_temp <- pila_list %>% 
  mutate(damageCodes = case_when(
    (deadTop %in% c("1", "1.0", "Y")) & (!str_detect(damageCodes, "DTOP")) ~ paste(damageCodes, "DTOP", sep = "|"),
    TRUE ~ damageCodes
  ))


#------BOLE CHAR COLUMN--------

#based on the current boleChar column, create boleChar_text and boleChar_numeric
pila_list <- mutate(pila_list,
                    boleChar_text = "",
                    boleChar_numeric = "")

#boleCharText is Y if num >0 or if Y; N if N or num = 0; NA if NA, else ?
pila_list <- pila_list %>%
  mutate(
    boleChar_numeric = case_when(
      suppressWarnings(!is.na(as.numeric(boleChar))) ~ suppressWarnings(as.numeric(boleChar)),
      boleChar == "N" ~ as.numeric(0),
      boleChar == "<10" ~ as.numeric(10),
      TRUE ~ NA),
    boleChar_text = case_when(
      !is.na(boleChar_numeric) & boleChar_numeric > 0 ~ "Y",
      !is.na(boleChar_numeric) & boleChar_numeric == 0 ~ "N",
      boleChar == "Y" ~ "Y",
      boleChar == "N" ~ "N",
      boleChar == "<10" ~ "Y",
      is.na(boleChar) ~ NA_character_,
      TRUE ~ NA_character_
    )
  ) 

## ----- Estimated DBH and Heights --------
#check est columns
pila_list_est <- pila_list %>% 
  select(eventID, treeNum, diameter, est_DBH_cm, height, est_height_m) %>% 
  filter(!is.na(est_DBH_cm) | !is.na(est_height_m))

#coalesce prioritizing measured values
pila_list <- pila_list %>% 
  mutate(diameter_final = case_when(!is.na(diameter) ~ diameter,
                                !is.na(est_DBH_cm) ~ as.numeric(est_DBH_cm),
                                TRUE ~ NA_real_),
         height_final = case_when(!is.na(height) ~ height,
                                  !is.na(est_height_m) ~ as.numeric(est_height_m),
                                  TRUE ~ NA_real_))
#add flag columns for data type to diameter and height
pila_list <- pila_list %>%  
  mutate(
    diameter_flag = case_when(
      !is.na(diameter) ~ "measured",
      !is.na(est_DBH_cm) ~ "estimated",
      TRUE ~ NA_character_  
    ),
    height_flag = case_when(
      !is.na(height) ~ "measured",
      !is.na(est_height_m) ~ "estimated",
      TRUE ~ NA_character_
    ))

pila_nas <- pila_list %>% 
  filter(is.na(diameter_final) | is.na(height_final)) %>% 
  select(eventID, treeNum, diameter, est_DBH_cm, diameter_final, height, est_height_m, height_final, percentLive, damageCodes, notes)


# -------GBIF---------
# add GBIF columns to match occurrence tab template
pila_list <- pila_list %>% 
  mutate(occurrenceID = paste(occurrenceID = paste("E", eventID, "-", "PILA", treeNum, sep = "")), 
         basisOfRecord = "HumanObservation",
         taxonID = "PILA",
         scientificName = "Pinus lambertiana (Douglas, 1827)",
         recordedBy = gsub("_", "|", recordedBy),
         occurrenceStatus = ifelse(eventID %in% c(5, 7, 8, 25), "absent", "present"), # change to absent for YPE 5, 7, 8, and 25
         individualCount = ifelse(eventID %in% c(5, 7, 8, 25), 0, 1),
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
         footprintWKT = "",
         footprintSRS = "", 
         associatedMedia = "",
         identifiedBy = recordedBy,
         dateIdentified = date_time,
         identificationReferences = "",
         identificationRemarks = "",
         identificationQualifier = "",
         identificationVerificationStatus = "",
         occurrenceRemarks = notes,
         materialSampleID = "",
         recordNumber = "",
         organismRemarks = "",
         identificationID = "", 
         height = height_final,
         diameter = diameter_final)

# select columns for GBIF occurrence tab
cleanPILAdata <- pila_list %>% 
  select(occurrenceID, eventID, basisOfRecord, taxonID, scientificName, 
         recordedBy, occurrenceStatus, individualCount, 
         organismQuantity, organismQuantityType, publicDisplay, dataAccess, 
         lifeStage, sex, reproductiveCondition, behavior, 
         covariateSample, preparations, identifiedBy, dateIdentified, 
         identificationReferences, identificationRemarks, 
         identificationQualifier, identificationVerificationStatus, 
         occurrenceRemarks, materialSampleID, recordNumber, 
         organismRemarks, identificationID, 
         diameter, diameter_flag, height, height_flag, pitchTubes, exitHoles, 
         activeBranchCanker, inactiveBranchCanker, 
         activeBoleCanker, inactiveBoleCanker, 
         deadTop, percentLive, boleChar_text, boleChar_numeric, damageCodes)

# Part3: YOSE PILA Data Export ------------------------------------------------------------

# save as a csv file in the dataSandbox
write.csv(cleanPILAdata, "dataSandbox/CleanData/YOSE_cleanPILAList.csv", row.names = FALSE) # don't save first column

# Part4: Associated Tree Data Import --------------------------------------------------------------
# Bring in the tree data for each plot in the YPE_Data folder

# provide path for files in datadir
folders <- list.dirs(datadir, full.names = TRUE)[-c(1,4)] # Ensure full path names are used

# initialize an empty list to store data for each plot
tree_list <- data.frame()

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

# Part4: Associated Tree Data Wrangling -------------------------

# fix plotID naming
tree_list <- tree_list %>% 
  rename(plotID = plot)

# convert dSide Left to numeric 
tree_list$dSideL_m <- as.numeric(tree_list$dSideL_m) # likely dash entry error

# combine dSideR_m and dSideL_m into a single dSide column with checks
tree_list <- tree_list %>%
  mutate(dSide = case_when(
    !is.na(dSideR_m) & dSideR_m >= 0 ~ dSideR_m,
    !is.na(dSideL_m) & dSideL_m <= 0 ~ dSideL_m,
    TRUE ~ NA_real_
  ))
summary(tree_list)

#correct misspellings and inconsistencies in tree_list
tree_list <- tree_list %>%
  mutate(species = case_when(
    species == "PYGE" ~ "PIJE",
    species %in% c("unknown", "UNK", "UNKNOWN", "Charcol", "Unknown") ~ "Pinales",
    TRUE ~ species
  ))

# check species field again
unique(tree_list$species)

#remove SALIX and CONU -- should be shrubs 
tree_list <- tree_list %>% 
  filter(species != "CONU") %>% 
  filter(species != "SALIX") # removes only 2 records total (3309 to 3307)

##DAMAGE CODES: scan notes for damage codes and indicators thereof, create new columns for each code, then paste into the damageCodes column in tree_list

# check damage codes field
unique(tree_list$damageCodes)

# make damage codes uppercase
tree_list <- tree_list %>% 
  mutate(damageCodes = toupper(damageCodes))

# transform damage codes list to and standardize break char and damage codes to FHW codes
tree_list$damageCodes <- gsub('BROKE', 'BROK',
                              gsub('BTOP', 'BROK',
                                   gsub('_', '|', 
                                        gsub(', ', '|', 
                                             gsub('CROOK', 'CROK', tree_list$damageCodes)))))

#start with empty column and use separate if_else statements for each term 
tree_list <- tree_list %>% 
  mutate(birdDamage = "",  brokDamage = "",   crokDamage = "",
         bromDamage = "",  fireDamage = "",   forkDamage = "",
         leanDamage = "",  mammDamage = "",   mechDamage = "",
         mistDamage = "",  sparDamage = "",   sdDamage = "", 
         twinDamage = "",)

#add damage codes that are not accounted for in the column based on the notes
#commented lines were checked  - for these columns, all cases where the code found damage codes in the notes already had the code in the column
tree_list <- tree_list %>% 
  mutate(birdDamage = if_else(str_detect(notes, "(?i)sap\\s+sucker") | str_detect(notes, "(?i)sucker") | str_detect(notes, "(?i)wp") | str_detect(notes, "(?i)w\\s+p") | str_detect(notes, "(?i)bird") | str_detect(notes, "(?i)peck"), "BIRD", "", missing = ""),
         brokDamage = if_else(str_detect(notes, "(?i)brok") | str_detect(notes, "(?i)btop"), "BROK", "", missing = ""),
         bromDamage = if_else(str_detect(notes, "(?i)broom"), "BROM", "", missing = ""),
         crokDamage = if_else(str_detect(notes, "(?i)crok") | str_detect(notes, "(?i)crook"), "CROK", "", missing = ""),
         fireDamage = if_else(str_detect(notes, "(?i)char") | str_detect(notes, "(?i)torched") | str_detect(notes, "(?i)crisp") | str_detect(notes, "(?i)fire") | str_detect(notes, "(?i)burn") | str_detect(notes, "(?i)fs") | str_detect(notes, "(?i)cat\\s+face"), "FIRE", "", missing = ""),
         forkDamage = if_else(str_detect(notes, "(?i)fork"), "FORK", "", missing = ""), #why so few of these?
         mammDamage = if_else(str_detect(notes, "(?i)rodent") | str_detect(notes, "(?i)bear" ), "MAMM", "", missing = ""),
         mistDamage = if_else(str_detect(notes, "(?i)mist"), "MIST", "", missing = ""), 
         sparDamage = if_else(str_detect(notes, "(?i)spars") | str_detect(notes, "(?i)thin"), "SPAR", "", missing = ""),
         twinDamage = if_else(str_detect(notes, "(?i)twin"), "TWIN", "", missing = "")
         )

#remove false positives in fireDamage column, e.g. "offshoot" and "no fire scar"
tree_list <- tree_list %>%
  mutate(fireDamage = case_when(
    (plotID == 12 & treeNum == 6) ~ "",   (plotID == 74 & treeNum == 7) ~ "",
    (plotID == 74 & treeNum == 9) ~ "",   (plotID == 44 & treeNum == 3) ~ "",
    str_detect(notes, "(?i)no\\s+fire\\s+scar") ~ "",
    TRUE ~ fireDamage),
    mistDamage = case_when(
      str_detect(notes, "(?i)possible\\s+mist") ~ "",    str_detect(notes, "(?i)propbably\\s+mist") ~ "",  #misspelling intentional; matches misspelling in notes
      str_detect(notes, "(?i)probably\\s+mist") ~ "",
      TRUE ~ mistDamage),
    twinDamage = case_when(
      (plotID == 28 & treeNum == 60) ~ "",
      TRUE ~ twinDamage)
    )

#add in handful of missed codes 
tree_list <- tree_list %>%
  mutate(leanDamage = case_when(
    (plotID == 35 & treeNum == 195) ~ "LEAN",
    TRUE ~ leanDamage),
    mechDamage = case_when(
      (plotID == 18 & treeNum == 5) ~ "MECH",    (plotID == 19 & treeNum == 91) ~ "MECH",
      (plotID == 35 & treeNum == 193) ~ "MECH",  (plotID == 49 & treeNum == 52) ~ "MECH",
      TRUE ~ mechDamage),
    sdDamage = case_when(
      (plotID == 35 & treeNum == 195) ~ "SD",    (plotID == 35 & treeNum == 198) ~ "SD",
      (plotID == 35 & treeNum == 204) ~ "SD",    (plotID == 35 & treeNum == 237) ~ "SD",
      (plotID == 35 & treeNum == 245) ~ "SD",    (plotID == 35 & treeNum == 248) ~ "SD",
      (plotID == 49 & treeNum == 65) ~ "SD",     (plotID == 49 & treeNum == 69) ~ "SD",
      (plotID == 60 & treeNum == 50) ~ "SD",
      TRUE ~ sdDamage)
    )

#paste values in from separate damage columns into damageCodes
#this currently creates a few duplicate codes in the damageCodes column. This could eventually be fixed with some conditionals.
tree_list <- mutate(tree_list,damageCodes = case_when(
    (birdDamage != "") ~ paste(damageCodes, birdDamage, sep = "|"),
    (brokDamage != "") ~ paste(damageCodes, brokDamage, sep = "|"),
    (bromDamage != "") ~ paste(damageCodes, bromDamage, sep = "|"),
    (crokDamage != "") ~ paste(damageCodes, crokDamage, sep = "|"),
    (fireDamage != "") ~ paste(damageCodes, fireDamage, sep = "|"),
    (forkDamage != "") ~ paste(damageCodes, forkDamage, sep = "|"),
    (mammDamage != "") ~ paste(damageCodes, mammDamage, sep = "|"),
    (mistDamage != "") ~ paste(damageCodes, mistDamage, sep = "|"),
    (sparDamage != "") ~ paste(damageCodes, sparDamage, sep = "|"),
    (twinDamage != "") ~ paste(damageCodes, twinDamage, sep = "|"),
    TRUE ~ damageCodes))

# Part 6: Write out Clean Tree List -------------------------

# write results to a csv in case GBIF format is not desirable
write.csv(tree_list, "dataSandbox/CleanData/YOSE_cleanTreeList.csv")

# Part7: Reorganization for GBIF -------------------------

# rename columns to match occurrence tab template
treeOccurrenceData <- tree_list %>% 
  rename(eventID = plotID,
         recordedBy = crew,
         taxonID = species,
         diameter = DBH_cm,
         height = height_m)

# add GBIF columns to match occurrence tab template
treeOccurrenceData <- treeOccurrenceData %>% 
  mutate(occurrenceID = paste("E", eventID, "-", "Tree", treeNum, sep = ""),
         basisOfRecord = "HumanObservation",
         scientificName = case_when(
           taxonID == "PIJE" ~ "Pinus jeffreyi (Balf., 1853)", 
           taxonID == "ABCO" ~ "Abies concolor ((Gordon & Glend.) Lindl. ex Hildebr., 1861)",
           taxonID == "ABMA" ~ "Abies magnifica (A.Murray bis, 1863)", 
           taxonID == "CADE" ~ "Calocedrus decurrens ((Torr.) Florin, 1956)", 
           taxonID == "PIPO" ~ "Pinus ponderosa (Douglas ex C.Lawson, 1836)",
           taxonID == "QUKE" ~ "Quercus kelloggii (Newb., 1858)",
           taxonID == "PSME" ~ "Pseudotsuga menziesii ((Mirb.) Franco, 1950)", 
           taxonID == "Pinales" ~ "Pinales (Gorozh., 1904)", 
           taxonID == "QUCH" ~ "Quercus chrysolepis (Leibm., 1854)",
           taxonID == "QUWI" ~ "Quercus wislizeni (A.DC., 1864)",
           taxonID == "ACMA" ~ "Acer macrophyllum (Pursh, 1813)",
           taxonID == "UMCA" ~ "Umbellularia californica ((Hook. & Arn.) Nutt., 1842)",
           taxonID == "PICO" ~ "Pinus contorta (Douglas ex Loudon, 1838)",
           taxonID == "JUOC" ~ "Juniperus occidentalis (Hook., 1838)",
           taxonID == "Abies_sp" ~ "Abies (Mill., 1754)"),
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
         activeBoleCanker = "",
         inactiveBoleCanker = "",
         deadTop = if_else((str_detect(damageCodes, "\\bDTOP\\b") | (str_detect(damageCodes, "\\bSD\\b"))), "Y", "N", missing = "N"),
         boleChar_text = if_else(str_detect(damageCodes, "\\bFIRE\\b"), "Y", "N", missing = "N"),
         boleChar_numeric = "", 
         diameter_flag = "measured",
         height_flag = "measured"
         )

# select columns for GBIF occurrence tab
gbifTreeOccurrence <- treeOccurrenceData %>% 
  select(occurrenceID, eventID, 
         basisOfRecord, taxonID, scientificName, recordedBy, 
         occurrenceStatus, individualCount, organismQuantity, 
         organismQuantityType, publicDisplay, dataAccess, 
         lifeStage, sex, reproductiveCondition, behavior, 
         covariateSample, preparations, identifiedBy, dateIdentified, 
         identificationReferences, identificationRemarks, 
         identificationQualifier, identificationVerificationStatus, 
         occurrenceRemarks, materialSampleID, 
         recordNumber, organismRemarks, identificationID, 
         diameter, diameter_flag, height, height_flag, 
         pitchTubes, exitHoles, 
         activeBranchCanker, inactiveBranchCanker, 
         activeBoleCanker, inactiveBoleCanker, 
         deadTop, percentLive, boleChar_text, boleChar_numeric, damageCodes) 

# Part8: Combine PILA and Tree Data -------------------------

gbifOccurrence <- rbind(cleanPILAdata, gbifTreeOccurrence)

#assign tree reproductive status 
gbifOccurrence <- gbifOccurrence %>%
  mutate(
    reproductiveCondition = case_when(
      str_detect(occurrenceRemarks, "(?i)no\\s+cone[s]?") ~ "no cones observed",
      str_detect(occurrenceRemarks, "(?i)cone[s]?") ~ "cone-bearing",
      TRUE ~ NA_character_
    )
  )

gbifOccurrence <- gbifOccurrence %>%
  mutate(
    damageCodes = sapply(damageCodes, function(x) {
      #split by "|"
      codes <- unique(unlist(strsplit(as.character(x), "\\|")))
      #remove NA
      codes <- codes[!is.na(codes) & codes != "NA"]
      #recombine into single string
      paste(codes, collapse = "|")
    })
  )
# bring in latitude and longitude from 1_SpatialDataWrangling.R
occurrence_positions <- read.csv("outputSandbox/occurrence_positions.csv")

# strip away any hidden characters
gbifOccurrence$occurrenceID <- trimws(gbifOccurrence$occurrenceID)
occurrence_positions$occurrenceID <- trimws(occurrence_positions$occurrenceID)

# join with occurrence data
gbifOccurrenceTest <- left_join(gbifOccurrence, occurrence_positions, by = "occurrenceID")

# write out results for occurrence tab
write.csv(gbifOccurrenceTest, "outputSandbox/YOSE_GBIFoccurrence.csv", row.names = FALSE) # don't save first column





