
# (3) Recode And Clean Understory Data/Pre-processing for Richness----------------------------------------

# Authors: Jenny Cribbs & Tazlina Dentinger
# Date: 21 April 2024
# Updated more recently than that

# Input: all_plots_understory data frame with 4 columns from step 2

# Code description: This cleaning script recodes misspellings, redundancies/synonyms e.g. scientific code and common name or code and full name, and lumps unknowns and life stages. 

# Desired Output: (1) A clean dataframe with one consistent code for each species, substrate, or association. (2) For richness, we will then filter out the substrates, so that the richness calculation includes only unique understory plant species (hits + associations). Unknowns will have to be filtered out entirely or lumped...discuss! (3) For percent cover, filter out associations since we only want to know what was hit (species and substrate). Probably best to put 2 and 3 as part of their respective analysis scripts.

# -------------------------------------------------------------------------

library(tidyverse)

# Read in data (or run Understory scripts 1 and 2)
all_plots_understory <- read_csv("dataSandbox/RawData/UnderstoryDataLong.csv")

#create unique ID for each plot & hit and number the hits to track the order
all_plots_understory <- all_plots_understory %>% mutate(id = paste0("pl", plotID, "_", "pt", dOut_m))
all_plots_understory <- all_plots_understory %>% group_by(id) %>% mutate(hitNum = row_number())
all_plots_understory <- all_plots_understory %>% mutate(hitNum = case_when(
  pin_vs_assoc == "pin" ~ hitNum,
  pin_vs_assoc == "assoc" ~ NA,
  TRUE ~ NA
)) %>% ungroup () %>% select(!id)

##---------------------- Part 1: Add identifications made post-field season ----------------------------
#Based on review documented in Unknown Veg Notes -----------------------------------------------------

#Still needs full check with full Unknown Veg Notes

#read in Unknown Veg csv file
unkVeg <- read_csv("/Users/jennifercribbs/Documents/R-Projects/MultipleDisturbances/dataSandbox/CleanData/Unknown Veg Notes - UnknownPlants.csv")
names(unkVeg) <- gsub(" ", "_", names(unkVeg))
#keep only relevant columns and YOSE rows
unkVeg <- unkVeg %>% select(project, asEntered, code, bestGuess, confidentTo, nextSteps, plotID) %>% filter(project == "YOSE")

#Create unique columns by plotID and species in each df to merge
unkVeg <- unkVeg %>% mutate(
  uniqueID = paste0(asEntered, plotID))
all_plots_understory <- all_plots_understory %>% mutate(
  uniqueID = paste0(species, plotID))

#use merge to map codes in dictionary to codes in df
all_plots_understory <- merge(all_plots_understory, unkVeg, by.x = "uniqueID", by.y = "uniqueID", all.x = T) #CHANGE BACK TO TRUE 
all_plots_understory <- all_plots_understory %>% 
  mutate(species = case_when(
    nextSteps %in% c("ID'ed", "Not ID-able") ~ bestGuess, #need to clean up bestGuess column before using - or use Code?
    is.na(bestGuess) ~ species, #double check that this worked...
    TRUE ~ species
  )) %>% select(plotID.x, dOut_m, pin_vs_assoc, species, confidentTo, hitNum) %>% rename(plotID = plotID.x)

##-------------------------- Part 2: clean data "manually" in R ----------------------------
#Includes fixing misspellings and incorrect entries or common names-------------------------

# Look at all unique hits
unique(all_plots_understory$species) # 375 (reduced from 501 when there was incorrect incrementing)

# bar chart to look at frequency of unique hits
# all_plots_understory %>% 
#   group_by(species) %>%
#   summarize (n = n()) %>%
#   mutate(total = sum(n),
#          freq = n / total) %>%
#   ggplot() +
#   geom_bar(mapping=aes(x=species, y=freq),stat="identity") +
#   xlab("Hit") +
#   ylab("Proportion") +
#   coord_flip()

#Standardize species ending (currently no "sp", no _1 etc.)
all_plots_understory <- all_plots_understory %>% 
  mutate(species = case_when(
    str_detect(species, " sp.") ~ unlist(str_split(species, " "))[1],
    str_detect(species, "Taraxacum sp") ~ "Taraxacum", #only case that doesn't include a "."
    str_detect(species, "_sp") ~ unlist(str_split(species, "_"))[1],
    str_detect(species, "_\\d+") ~ unlist(str_split(species, "_"))[1],
    TRUE ~ species
  ))

# Correct misspellings 
all_plots_understory <- all_plots_understory %>% 
  mutate(species = case_when(
    species %in% c("littter", "ltter", "littler", "litterlitter", "lItter", "pinecone", "bark") ~ "litter", #removed "unknown_DD" from list - should be in wood
    species %in% c("bare", "dirt", "DG", "dg") ~ "bareground",
    species %in% c("rcok", "gravel") ~ "rock",
    str_detect(species, "_DD") ~ "wood",
    species == "Pranus" ~ "Prunus",
    species %in% c("PPREM", "PREM") ~ "Prunus emarginata",
    species == "cynocerous" ~ "Cynosurus echinatus",
    species %in% c("striptanthis", "streptanthus tortuosus") ~ "Streptanthus tortuosus",
    species %in% c("Saxifrogacea", "saxifrogacea", "Saxifrage", "unk_saxifrage") ~ "Saxifragaceae",
    species %in% c("castilleja", "castelleja") ~ "Castilleja",
    species == "enicamena" ~ "Ericameria",
    species == "Eriogunum sp." ~ "Eriogonum",
    species == "Senicio glomerata" ~ "Senecio glomeratus",
    species == "Achillea millefolia" ~ "Achillea millefolium",
    species %in% c("CAREXI", "CAREX", "carex") ~ "Carex",
    species == "Pseudoghaphalium californicum" ~ "Pseudognaphalium californicum",
    species %in% c("unk_apeaceae", "unk_apiaceae") ~ "Apiaceae",
    species == "HIAE" ~ "HIAL",
    species == "CIEN" ~ "CEIN",
    species == "GRER" ~ "GAER",
    species == "COMO" ~ "CAMO",
    species == "STFO" ~ "STTO",
    species == "SYCO" ~ "SYMO",
    species == "PITO" ~ "Diplacus torreyi",
    species == "ERHU" ~ "ERNU",
    species == "DOOB" ~ "GOOB",
    species == "GADS" ~ "GADI",
    species == "RALE" ~ "RULE",
    species == "DEMI" ~ "DERI", #misspelling of species code
    species == "PUCA" ~ "ROCA",
    grepl("^QUWE_", species, ignore.case = TRUE) ~ "QUWI",
    species == "Erythranthre" ~ "Erythranthe",
    species == "RINI" ~ "RIVI", #incorrectly entered from plot 19
    species == "ARVE" ~ "ARVI", #over walkie-talkie...
    species == "epilobium" ~ "Epilobium",
    species %in% c("SALIX", "salix") ~ "Salix",
    species == "sedum" ~ "Sedum",
    species == "forbe" ~ "forb",
    species == "Gnaphalium californica" ~ "Gnaphalium californicum", 
    species == "Lilum parvum" ~ "Lilium parvum",
    TRUE ~ species
  ))

# Convert common names to scientific and fix scientific names
all_plots_understory <- all_plots_understory %>% 
  mutate(species = case_when(
    #species %in% c("moss1", "moss2") ~ "moss", # do we want to count brophytes, lichens, etc???
    species == "cryptantha/plagiobothrys" ~ "Boraginaceae",
    species == "brome" ~ "Bromus",
    species == "fireweed" ~ "CHAN",
    species == "horsetail" ~ "Equisetum",
    species == "rush" ~ "Juncus",
    species == "elgl" ~ "ELGL",
    species %in% c("whisker plant", "whisker brush", "whiskerbrush") ~ "LECI",
    species == "goldenrod" ~ "Solidago",
    species == "vetch" ~ "Vicia",
    species == "fern" ~ "Polypodiopsida",
    species == "Dodder" ~ "Cuscuta",
    species == "aster" ~ "Asteraceae",
    species == "grass" ~ "Poaceae",
    species == "saxifrage" ~ "Saxifragaceae",
    species == "moss" ~ "Bryophyta",
    species == "sedge" ~ "Carex",
    species == "forb" ~ paste0("forb_YPE", plotID),
    TRUE ~ species
  ))

# Correct misidentifications and incorrect entries
all_plots_understory <- all_plots_understory %>% 
  mutate(species = case_when(
    species == "AZOC" ~ "Rhododendron occidentale",
    species == "GAHI" ~ "SYMO", #paper data sheet said creeping snowberry; recoded to correct snowberry
    species == "MYOD" ~ "Osmorhiza", #paper data sheet said sweet cicely; recoded to correct sweet cicely
    species == "QUDU" ~ "Quercus berberidifolia", #incorrect scrub oak
    species == "SASC" ~ "Salix", #insufficient evidence of species-level ID
    species == "PEHE" & plotID %in% c("3", "4", "9") ~ "Penstemon", #paper data sheet said penstemon or blue penstemon
    species == "TOPU" ~ "TODI", #incorrect poison oak entered
    species == "CAUM" ~ "Calyptridium", #paper said pussypaws
    species == "PENE" ~ "Penstemon", #paper said maybe newberrii
    species == "COCO_seed" ~ "COCO", #not a tree => seeedlings not recorded
    species == "BRCA" & plotID == 8 ~ "Poaceae", #paper data sheet said grass 1
    species %in% c("Juncus", "juncus") & plotID %in% c(49,73) ~ "Poales", #on paper as juncus/round carex
    species == "GAPRO" ~ "Pyrola", #on paper as wintergreen
    species == "LOIN" ~ "Philadelphus lewisii", #based on note on datasheet 
    species == "LUMA" ~ "LULA", #L. macrophyllum doesn't exist
    species == "Juncus patens" ~ "Juncus", #insufficient evidence of species-level ID
    species == "epilobium" & plotID == 25 ~ "Epilobium brachycarpum",
    species == "PESP" & plotID == 2 ~ "Penstemon", 
    species == "Draperia drymariodes" ~ "Draperia systyla", #incorrect specific epithet written down
    species == "PSMI" ~ "Pseudognaphalium", #pseudognaphaliums too difficult to identify
    species == "lessingia" & (plotID == 29 | plotID == 34) ~ "Lessingia leptoclada",
    species == "LUAR" ~ "Lupinus", #unresolved duplicate code
    species == "GATR" & (plotID %in% c(35, 45, 54, 12, 32, 59, 49)) ~ "Galium triflorum",
    species == "FRCA" ~ "Frangula", 
    species %in% c("TAER", "TAOC") ~ "Taraxacum",
    species == "CYOF" & plotID == 21 ~ "Adelinia grandis",
    species == "Lathyrus nevadensis var. nevadensis" ~ "Lathyrus nevadensis", #unnecessary
    TRUE ~ species
  ))

##----------- Part 3: lump various Poales, forbs, life stages for visualization ---------------------------

# lump all carex for now
carexcheck <- all_plots_understory %>% 
  mutate(species = case_when(
    str_detect(species, "(?i)carex_") ~ "Carex",
    str_detect(species, "(?i)carex sp") ~ "Carex",
    TRUE ~ species
  ))

# lump all unknown grasses for now
grassgroup <- carexcheck %>% 
  mutate(species = case_when(
    grepl("^grass_", species, ignore.case = TRUE) ~ "Poaceae", 
    TRUE ~ species
  ))

# NEEDS TESTING WITH NEW UNKNOWN VEG NOTES
# Assign all unknown forbs a plot-based unique ID, and set confidentTo to Magnoliopsida, allowing for an under- and overestimate calculation of species richness
forbfest <- grassgroup %>% 
  mutate(species = case_when(
    grepl("^forb_", species, ignore.case = TRUE) ~ paste0("forb", plotID), 
    species %in% c("forb 1", "Forb", "forbe", "forbe_1", "forbe_2", "forb_1", "forbe_unknown", "forb_unknown") ~ paste0("forb_YPE", plotID),
    TRUE ~ species
  ))

all_plots_understory <- forbfest
# # Look at all unique hits
# unique(forbfest$species) # 315 -> 318
# 
# # bar chart to look at frequency of unique hits
# forbfest %>% 
#   group_by(species) %>%
#   summarize (n = n()) %>%
#   mutate(total = sum(n),
#          freq = n / total) %>%
#   ggplot() +
#   geom_bar(mapping=aes(x=species, y=freq),stat="identity") +
#   xlab("Hit") +
#   ylab("Proportion") +
#   theme(axis.text.x=element_text(angle=90,hjust=1))
# 
# # Consolidate life stages--actually maybe just for richness not for percent cover 
# allLifeStages <- forbfest %>% 
#   mutate(species = case_when(
#     grepl("^ABCO_", species, ignore.case = TRUE) ~ "ABCO",
#     grepl("^ABMA_", species, ignore.case = TRUE) ~ "ABMA",
#     grepl("^ACMA_", species, ignore.case = TRUE) ~ "ACMA",
#     grepl("^CADE_", species, ignore.case = TRUE) ~ "Calocedrus decurrens",
#     grepl("^JUOC_", species, ignore.case = TRUE) ~ "JUOC",
#     grepl("^PILA_", species, ignore.case = TRUE) ~ "PILA",
#     grepl("^PIJE_", species, ignore.case = TRUE) ~ "PIJE",
#     grepl("^PIPO_", species, ignore.case = TRUE) ~ "Pinus ponderosae",
#     grepl("^PSME_", species, ignore.case = TRUE) ~ "PSME",
#     grepl("^QUCH_", species, ignore.case = TRUE) ~ "QUCH",
#     grepl("^QUKE_", species, ignore.case = TRUE) ~ "QUKE",
#     grepl("^QUWI_", species, ignore.case = TRUE) ~ "QUWI",
#     TRUE ~ species
#   ))
# 
# unique(allLifeStages$species) # 283 ->292
# 
# # bar chart to look at frequency of unique hits
# allLifeStages %>% 
#   group_by(species) %>%
#   summarize (n = n()) %>%
#   mutate(total = sum(n),
#          freq = n / total) %>%
#   ggplot() +
#   geom_bar(mapping=aes(x=species, y=freq),stat="identity") +
#   xlab("Hit") +
#   ylab("Proportion") +
#   theme(axis.text.x=element_text(angle=90,hjust=1))
# 
# # group by plot 
# allLifeStages %>% group_by(plotID)  %>% 
# summarize (n = n()) %>%
#   mutate(total = sum(n),
#          freq = n / total) %>%
#   ggplot() +
#   geom_bar(mapping=aes(x=plotID, y=freq),stat="identity") +
#   xlab("Hit") +
#   ylab("Proportion") +
#   theme(axis.text.x=element_text(angle=90,hjust=1))

##-------- Part 4: Use Species Code Dictionary & Taxonstand to apply scientific names --------

#read in Species Code Dictionary csv file
spDict <- read_csv("/Users/jennifercribbs/Documents/R-Projects/MultipleDisturbances/Data/Dictionaries/Species Code Dictionary - Species Code Dictionary.csv")
#remove ` in column names
names(spDict) <- gsub("`", "", names(spDict))

#keep only relevant columns
#spDict <- spDict %>% mutate(inProject = case_when(
  #!is.na(...12) ~ paste(inProject, ...12, sep = ", "),
  #TRUE ~ inProject)) %>% 
  #select(code, scientificName, lifeForm, nativeVsIntroduced, annualVsPerennial, inProject)

#use merge to map codes in dictionary to codes in df
#all_plots_understory <- merge(all_plots_understory, spDict, by.x = "species", by.y = "code", all.x = T) #CHANGE BACK TO TRUE to keep all vals
#all_plots_understory <- all_plots_understory %>% 
  #mutate(species = case_when(
    #!is.na(all_plots_understory$scientificName) ~ all_plots_understory$scientificName, #is.na doesn't work still - converts a bunch of data to na
    #TRUE ~ species
 # )) %>% select(!scientificName)

#check species names
unique(all_plots_understory$species)

# speciesList <- all_plots_understory %>% select(species) %>% unique
# write_csv(speciesList, "dataSandbox/CleanData/speciesList.csv")

# MOVE TO SEPARATE SCRIPT 

#Taxonstand
library(U.Taxonstand)

# databases <- list("LCVP", "WFO", "WP", "WCVP")
# 
# for (db in databases)
  
#U.Taxonstand LCVP database
LCVP1 <- read_csv("C:/Users/tazli/Downloads/YOSE_SugarPine/MultipleDisturbances/dataSandbox/Dictionaries/Plants_LCVP_database_part1.csv")
LCVP2 <- read_csv("C:/Users/tazli/Downloads/YOSE_SugarPine/MultipleDisturbances/dataSandbox/Dictionaries/Plants_LCVP_database_part2.csv")
LCVP3 <- read_csv("C:/Users/tazli/Downloads/YOSE_SugarPine/MultipleDisturbances/dataSandbox/Dictionaries/Plants_LCVP_database_part3.csv")
LCVP <- rbind(LCVP1, LCVP2, LCVP3)
rm(LCVP1, LCVP2, LCVP3)

#use U.Taxonstand to spell check scientific names (takes a second)
nameMatchLCVP <- nameMatch(spList=all_plots_understory$species, spSource=LCVP, author = TRUE, max.distance= 4)
#keep only rows with fuzzy matching
nameMatchLCVPFuzzy <- nameMatchLCVP %>% select(Submitted_Name, Fuzzy, Name_in_database) %>% filter(Fuzzy == TRUE) %>% distinct()

#U.Taxonstand WP database
WP1 <- read_csv("C:/Users/tazli/Downloads/YOSE_SugarPine/MultipleDisturbances/dataSandbox/Dictionaries/Plants_WP_database_part1.csv", show_col_types = FALSE)
WP2 <- read_csv("C:/Users/tazli/Downloads/YOSE_SugarPine/MultipleDisturbances/dataSandbox/Dictionaries/Plants_WP_database_part2.csv", show_col_types = FALSE)
WP3 <- read_csv("C:/Users/tazli/Downloads/YOSE_SugarPine/MultipleDisturbances/dataSandbox/Dictionaries/Plants_WP_database_part3.csv", show_col_types = FALSE)
WP <- rbind(WP1, WP2, WP3)
rm(WP1, WP2, WP3)

#use U.Taxonstand to spell check scientific names (takes a second)
nameMatchWP <- nameMatch(spList=all_plots_understory$species, spSource=WP, author = TRUE, max.distance= 4)
#keep only rows with fuzzy matching
nameMatchWPFuzzy <- nameMatchWP %>% select(Submitted_Name, Fuzzy, Name_in_database) %>% filter(Fuzzy == TRUE) %>% distinct()

#U.Taxonstand WFO database
load("dataSandbox/Dictionaries/Plants_WFO.rdata") 
WFO <- database
rm(database)

nameMatchWFO <- nameMatch(spList=all_plots_understory$species, spSource=WFO, author = TRUE, max.distance= 4)
nameMatchWFOFuzzy <- nameMatchWFO %>% select(Submitted_Name, Fuzzy, Name_in_database) %>% filter(Fuzzy == TRUE) %>% distinct()

#match accepted names to the misspelled rows
all_plots_understory <- merge(all_plots_understory, nameMatchFuzzy, by.x = "species", by.y = "Submitted_Name", all.x = TRUE)
#change misspelled names to database names
all_plots_understory <- all_plots_understory %>% mutate(species = case_when(
  !is.na(Name_in_database) & Fuzzy == TRUE ~ Name_in_database, 
  TRUE ~ species
)) %>% select(species, plotID, dOut_m, pin_vs_assoc)

#Address name changes: What standard to use? 
# all_plots_understory <- all_plots_understory %>% 
#   mutate(species = case_when(
#     species == "piperia" ~ "Platanthera", #genus Pipera subsumed
#     species == "SACE" ~ "SAME", #Blue Elder moved to S. mexicana
#     TRUE ~ species
#   ))


##-------------------------------Part 5: Write dataframe to csv-----------------------------------
