coreData <- read.csv("/Users/jennifercribbs/Documents/Graduate School/Beetles Project/Data/ResinDuctRoughCount_SEKI2021.csv")
head(coreData)
unique(coreData$Core_ID) #82 cores in this prelim analysis
unique(coreData$Species) #5 with some blanks
unique(coreData$Calendar_Year) #1967-2021
unique(coreData$Resin_Duct_Count) # range is 0-15
boxplot(coreData$Ring_width) # no data yet
summary(coreData)

# boxplots by species
boxplot(as.factor(coreData$Species), coreData$Resin_Duct_Count)
