# Beetle Data Clean Up

# read in gbif ready data--use actualy Excel sheet for County and UTM zone otherwise data is the same
treeCheck <- read.csv("gbifExcelOccurence.csv")

unique(treeCheck$exitHoles) # find photo timestamps for exit holes
unique(treeCheck$pitchTubes) # find photo timestamps for pitch tubes

treeCheck %>% select (eventID, dateIdentified, occurrenceID, exitHoles, pitchTubes) %>% filter(exitHoles == "JEC_1115AM" | exitHoles == "JEC_1144AM" | pitchTubes == "JEC_1115AM" | pitchTubes == "JEC_1203PM") # 18 photos to review
