treeDataNov <- read_csv("/Users/jennifercribbs/Downloads/TreeData_13112024.csv")
unique(treeDataNov$deadTop)
unique(pila_list$DTOP)
summary(pila_list)
cankers <- pila_list %>% filter(is.na(inactiveBranchCanker | activeBranchCanker | inactiveBoleCanker | activeBoleCanker)) %>% select(plotID, treeNum, inactiveBranchCanker, activeBranchCanker, inactiveBoleCanker, activeBoleCanker)
cankers
# 23 NAs for at least one canker type--plots 5, 7, 8, and 29 don't have pila in the plot, so these represent true NAs. 
# Plot 60 trees 18-27 appear to have been assessed from a distance by the UCSB crew, so NAs are likely valid
# Plot 72 and 72 NAs were cleaned up by Taz and Jenny based on original notes, photos, and recollections 
unique(pila_list$exitHoles)
