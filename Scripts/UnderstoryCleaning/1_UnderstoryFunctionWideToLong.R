
# (1) YOSE Understory Data Carpentry Function -------------------------------------------------------------------------
# Authors: Andrew Latimer and Jenny Cribbs
# Date: 13 April 2024

# Function Input: List of tibbles containing all Understory data from each plot in the Transcribed Data folder. Field data was recorded with each point along the transect (1-50m) representing a row. Each plant or substrate that touched the pin flag at a given point was recorded as a 4 letter species code. Some points contain many hits. 

# Function Description: This function will take one of the plot-level data frames and turn it into a 4-column long-format data frame that contains the plot ID in column 1, the dOut_m pin location in column 2, whether the species was at the pin location (pin) or associated with it (assoc = not hit but within a 5m belt on either side of the transect line) and each species observation (code, full name, or unknown description) in column 4.
# To do this, we need 2 things: 1) count how many species are present at each location; 2) take all the species observations across all locations, get rid of the NAs and turn them into a long vector. 

# Function Output: The function converts the wide-format initial data with inconsistent numbers of columns to long-format data with 4 consistent columns. This can then be applied to the whole list and saved as a dataframe (see step 2)

# -------------------------------------------------------------------------

# set working directory to project dir 
#setwd("/Users/jennifercribbs/Documents/YOSE/Analysis/MultipleDisturbances")
setwd("/Users/jennifercribbs/Documents/R-Projects/MultipleDisturbances/Data/RawData/YPE_Data")

# Read function into the global environment 
convert_understory_data_to_long <- function(understory_data) {
  require(dplyr)
  # Get the relevant columns of the data frame, assuming the format is as is normal for the understory data frames (species1, species2, . . ., assoc1, assoc2, ...)
  understory_species <- dplyr::select(understory_data, matches("species"))
  understory_assoc <- dplyr::select(understory_data, matches("assoc"))
  
  # Count the species present at pin location and association in each row 
  n_species_by_row <- apply(understory_species, 1, f <- function(x) {return(length(na.omit(x)))}) 
  n_assoc_by_row <- apply(understory_assoc, 1, f <- function(x) {return(length(na.omit(x)))}) 
  
  # Turn the species and association info into long vectors with no NAs 
  species_vector <- unlist(apply(understory_species, 1, na.omit))
  assoc_vector <- unlist(apply(understory_assoc, 1, na.omit))
  n_species <- length(species_vector)
  n_assoc <- length(assoc_vector)
  total_number_of_obs <- n_species + n_assoc
  
  # Combine all the info into a long-form data frame with columns for plot id, location id, an indicator for "pin" vs "assoc", plus the long vectors of species and association ids in the last column. 
  species_understory_long <- data.frame(plotID = rep(understory_data$plotID[1], total_number_of_obs), dOut_m = c(rep(understory_data$dOut_m, n_species_by_row), rep(understory_data$dOut_m, n_assoc_by_row)), pin_vs_assoc = c(rep("pin", n_species), rep("assoc", n_assoc)), species = c(species_vector, assoc_vector))
  
  return(species_understory_long) 
} ## End function
