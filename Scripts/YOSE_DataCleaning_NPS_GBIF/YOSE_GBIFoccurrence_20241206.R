# GBIF: Occurence Tab for PILA and Associated Trees
# Authors: Jenny Cribbs, Tazlina Dentinger
# Date created: 06 December 2024
# Updated: 14 January 2025, TMD

# Overall Input: Read in PILA and Associated Tree data from Excel files downloaded from Google Sheets using for loop
# Code Description: clean data with a row for each tree with a unique occurrence ID and columns matching GBIF columns, so the output can be pasted into NPS template occurrence tab
# Overall Output:

# Part1: YOSE PILA Data Import -------------------------

# Load initial packages
if (!require("librarian")) install.packages("librarian")
librarian::shelf(tidyverse, readxl, viridis, patchwork)
library(VIM)

# Set the working directory
#setwd("/Users/jennifercribbs/Documents/YOSE/Analysis/MultipleDisturbances/")
setwd("/Users/tazli/Downloads/YOSE_SugarPine/MultipleDisturbances")

# Bring in the PILA data for each plot in the project folder from Google Sheets
# setting the directory for data extraction--change to your local data directory
#datadir <- "/Users/jennifercribbs/Documents/YOSE/Analysis/MultipleDisturbances/Data/RawData/YPE_Data"
datadir <- "/Users/tazli/Downloads/YOSE_SugarPine/MultipleDisturbances/Data/RawData/YPE_Data"

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

# Part2: PILA Data Wrangling -------------------------

# remove row with no data
pila_list <- pila_list %>% filter(!is.na(plotID))

# rename columns to match template style
pila_list <- pila_list %>% 
  rename(eventID = plotID,
         recordedBy = crew,
         diameter = DBH_cm,
         height = height_m,
         deadTop = DTOP, 
         boleChar = fire_scar)

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
         dateIdentified = date,
         identificationReferences = "",
         identificationRemarks = "",
         identificationQualifier = "",
         identificationVerificationStatus = "",
         occurrenceRemarks = notes,
         materialSampleID = "",
         recordNumber = "",
         organismRemarks = "",
         identificationID = "")



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
         diameter, height, pitchTubes, exitHoles, 
         activeBranchCanker, inactiveBranchCanker, 
         activeBoleCanker, inactiveBoleCanker, 
         deadTop, percentLive, boleChar, damageCodes)

# Part3: YOSE PILA Data Import -------------------------

# save as a csv file in the working directory
write.csv(cleanPILAdata, "YOSE_cleanPILAdata.csv", row.names = FALSE) # don't save first column

# Part4: Associated Tree Data Import -------------------------
# Bring in the tree data for each plot in the YPE_Data folder -----------

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

# Part4: Associated Tree Data Wrangling -------------------------

# fix plotID naming
tree_list <- tree_list %>% 
  rename(plotID = plot) # plotID entered as plot for Trees

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

# check species field
unique(tree_list$species)

# clean species list
tree_list <- tree_list %>%
  mutate(species = case_when(
    species == "PYGE" ~ "PIJE", # correct misspelling in data entry
    species %in% c("unknown", "UNK", "UNKNOWN", "Charcol", "Unknown") ~ "UNKNOWN",
    TRUE ~ species
  ))

# check species field again
unique(tree_list$species)

# remove SALIX and CONU because they should be shrubs 
# although some are tree-like, these species were likely not assessed as trees uniformly across the plots
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
                              gsub('BTOP', 'BROK', # change to BROK after verifying BTOPs
                                   gsub('_', '|', 
                                        gsub(', ', '|', 
                                             gsub('CROOK', 'CROK', tree_list$damageCodes)))))

#start with empty column and use separate if_else statements for each term 
tree_list <- tree_list %>% 
  mutate(abgrDamage = "",   birdDamage = "",
         brokDamage = "",   crokDamage = "",
         bromDamage = "",   dtopDamage = "",
         fireDamage = "",   forkDamage = "",
         leanDamage = "",   mammDamage = "",
         mechDamage = "",   mistDamage = "",
         sdDamage = "",     sparDamage = "",
         twinDamage = "",)

#add damage codes that are not accounted for in the column based on the notes
#commented lines were checked  - for these columns, all cases where the code found damage codes in the notes already had the code in the column
tree_list <- tree_list %>% 
  mutate(#abgrDamage = if_else(str_detect(notes, "(?i)abgr"), "ABGR", "", missing = ""),
         birdDamage = if_else(str_detect(notes, "(?i)sap\\s+sucker") | str_detect(notes, "(?i)sucker") | str_detect(notes, "(?i)wp") | str_detect(notes, "(?i)w\\s+p") | str_detect(notes, "(?i)bird") | str_detect(notes, "(?i)peck"), "BIRD", "", missing = ""),
         brokDamage = if_else(str_detect(notes, "(?i)brok") | str_detect(notes, "(?i)btop"), "BROK", "", missing = ""),
         bromDamage = if_else(str_detect(notes, "(?i)broom"), "BROM", "", missing = ""),
         crokDamage = if_else(str_detect(notes, "(?i)crok") | str_detect(notes, "(?i)crook"), "CROK", "", missing = ""),
        #dtop
         fireDamage = if_else(str_detect(notes, "(?i)char") | str_detect(notes, "(?i)torched") | str_detect(notes, "(?i)crisp") | str_detect(notes, "(?i)fire") | str_detect(notes, "(?i)burn") | str_detect(notes, "(?i)fs") | str_detect(notes, "(?i)cat\\s+face"), "FIRE", "", missing = ""),
         forkDamage = if_else(str_detect(notes, "(?i)fork"), "FORK", "", missing = ""), #why so few of these?
        #leanDamage = if_else(str_detect(notes, "(?i)lean") | str_detect(notes, "(?i)horiz"), "LEAN", "", missing = ""),
         mammDamage = if_else(str_detect(notes, "(?i)rodent") | str_detect(notes, "(?i)bear" ), "MAMM", "", missing = ""), #what to do with 1 instance of mech or rodent? probably just mech, since more general
        #mechDamage = if_else(str_detect(notes, "(?i)mech"), "MECH", "", missing = ""),
         mistDamage = if_else(str_detect(notes, "(?i)mist"), "MIST", "", missing = ""), #this may miss things that are only called as brooming, but I think that's best
             #sdDamage = if_else(str_detect(notes, "(?i)sd"), "SD", "", missing = ""),
         sparDamage = if_else(str_detect(notes, "(?i)spars") | str_detect(notes, "(?i)thin"), "SPAR", "", missing = ""),
         twinDamage = if_else(str_detect(notes, "(?i)twin"), "TWIN", "", missing = "")
#       , case_when(damageCodes == "NA" ~ "")   #would remove NAs in concat cells if it worked
        )

#check fire damage column - also need to check for false positive FIRE, then remove
unique(tree_list$fireDamage)

#temporary object to check the damage columns
treeDMG <- filter(tree_list, bromDamage == "BROM") %>% 
  select(plotID, treeNum, species, percentLive, damageCodes, notes, bromDamage)

#remove false positives in fireDamage col e.g. "offshoot" and "no fire scar"
tree_list <- tree_list %>%
  mutate(fireDamage = case_when(
    (plotID == 12 & treeNum == 6) ~ "",
    (plotID == 74 & treeNum == 7) ~ "",
    (plotID == 74 & treeNum == 9) ~ "",
    (plotID == 44 & treeNum == 3) ~ "",
    str_detect(notes, "(?i)no\\s+fire\\s+scar") ~ "",
    TRUE ~ fireDamage),
    mistDamage = case_when(
      str_detect(notes, "(?i)possible\\s+mist") ~ "",
      str_detect(notes, "(?i)propbably\\s+mist") ~ "",  #misspelling intentional; matches misspelling in notes
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
      (plotID == 18 & treeNum == 5) ~ "MECH",
      (plotID == 19 & treeNum == 91) ~ "MECH",
      (plotID == 35 & treeNum == 193) ~ "MECH",
      (plotID == 49 & treeNum == 52) ~ "MECH",
      TRUE ~ mechDamage),
    sdDamage = case_when(
      (plotID == 35 & treeNum == 195) ~ "SD",
      (plotID == 35 & treeNum == 198) ~ "SD",
      (plotID == 35 & treeNum == 204) ~ "SD",
      (plotID == 35 & treeNum == 237) ~ "SD",
      (plotID == 35 & treeNum == 245) ~ "SD",
      (plotID == 35 & treeNum == 248) ~ "SD",
      (plotID == 49 & treeNum == 65) ~ "SD",
      (plotID == 49 & treeNum == 69) ~ "SD",
      (plotID == 60 & treeNum == 50) ~ "SD",
      TRUE ~ sdDamage)
    )

#paste Damage into damageCodes
#this currently creates a few duplicate codes in the damageCodes column. This could eventually be fixed with some conditionals.
tree_list <- tree_list %>% 
  mutate(damageCodes = case_when(
    (birdDamage != "") ~ paste(damageCodes, birdDamage, sep = "|"),
    (brokDamage != "") ~ paste(damageCodes, brokDamage, sep = "|"),
    (bromDamage != "") ~ paste(damageCodes, bromDamage, sep = "|"),
    (crokDamage != "") ~ paste(damageCodes, crokDamage, sep = "|"),
    (dtopDamage != "") ~ paste(damageCodes, dtopDamage, sep = "|"),
    (fireDamage != "") ~ paste(damageCodes, fireDamage, sep = "|"),
    (forkDamage != "") ~ paste(damageCodes, forkDamage, sep = "|"),
    (mammDamage != "") ~ paste(damageCodes, mammDamage, sep = "|"),
    (mistDamage != "") ~ paste(damageCodes, mistDamage, sep = "|"),
    (sparDamage != "") ~ paste(damageCodes, sparDamage, sep = "|"),
    (twinDamage != "") ~ paste(damageCodes, twinDamage, sep = "|"),
    TRUE ~ damageCodes))

# Part 6: Write out Clean Tree List -------------------------

#maybe select out 
# write results to a csv in case GBIF format is not desirable
write.csv(tree_list, "CleanTreeList.csv")

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
           taxonID == "UNKNOWN" ~ "Pinales (Gorozh., 1904)", # check that we're not mislabeling any oaks etc. 
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
         boleChar = if_else(str_detect(damageCodes, "\\bFIRE\\b"), "Y", "N", missing = "N"))


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
         diameter, height, pitchTubes, exitHoles, 
         activeBranchCanker, inactiveBranchCanker, 
         activeBoleCanker, inactiveBoleCanker, 
         deadTop, percentLive, boleChar, damageCodes) 

# Part8: Combine PILA and Tree Data -------------------------

gbifOccurrence <- rbind (cleanPILAdata, gbifTreeOccurrence)
# save as a csv file in the working directory

gbifOccurrence <- gbifOccurrence %>%
  mutate(
    reproductiveCondition = case_when(
      str_detect(occurrenceRemarks, "(?i)no\\s+cone[s]?") ~ "no cones observed",
      str_detect(occurrenceRemarks, "(?i)cone[s]?") ~ "cone-bearing",
      TRUE ~ NA_character_
    )
  )

write.csv(gbifOccurrence, "YOSE_GBIFoccurrence.csv", row.names = FALSE) # don't save first column





