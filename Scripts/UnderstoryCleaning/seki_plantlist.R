# Author: Olivia Ross
# Date: 18 December 2024

# This script creates a list of unique plant names from vegetation collected for 
# Jenny Cribbs' Multiple Disturbances Project - SEKI 2024

# packages
librarian::shelf(here, dplyr, readxl, writexl, data.table)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Read in Understory List Data for each plot and combine files

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# one xlsx file per survey plot 

# list all xlsx files in the folder (should be 27)
understory_plot_files <- list.files(path = here("Data/RawData/UnderstoryListData"), 
                    pattern = "\\.xlsx$",
                    full.names = TRUE)
            
# create an empty list to store the understory files for each plot
understory_list <- list()

# loop through the "UnderstoryListData" folder and read in each xlsx file
for (file in understory_plot_files) {
  
  # read in each excel xlsx file
  understory_plot <- read_excel(file) 
  
  # store each understory plot file in it's own data frame
  understory_list[[file]] <- understory_plot 
}

# combine understory data frames for all plots
understory_plot_combined <- do.call(rbind, understory_list)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Create a list of unique plant names found in SEKI 2024 plots

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# select just species name for plant list
seki2024_species_list <- understory_plot_combined |>
  select(species)

# remove duplicates
seki2024_species_list <- seki2024_species_list[!duplicated(seki2024_species_list),] 
  
# sort alphabetically
seki2024_species_list <- seki2024_species_list[order(seki2024_species_list$species),]

# export as csv to data folder
write.csv(seki2024_species_list, here("Data/CleanData/seki2024_plantlist.csv"))

##Spellcheck and Cleaning on overall understory data
spellcheck <- understory_plot_combined %>% 
  mutate(species = case_when(
    species == "Alium" ~ "Allium",
    species == "Allium validium" ~ "Allium validum",
    species == "Apocynum androsaernifolium" ~ "Apocynum androsaemifolium",
    grepl("^Artemesia", species, ignore.case = TRUE) ~ "Artemisia",
    species == "Aster brewerii" ~ "Aster breweri",
    species %in% c('Castilleja applegati', "Castelleja applegatii") ~ "Castilleja applegatei",
    grepl("^Castelleja", species, ignore.case = TRUE) ~ "Castilleja",
    species == "Ceanothis integerrimus" ~ "Ceanothus integerrimus", 
    species == "Ceratodon purpureas" ~ "Ceratodon purpureus",
    species == "Chaenactis douglassii" ~ "Chaenactis douglasii",
    species == "Chamanerion angustifolium" ~ "Chamaenerion angustifolium",
    species == "Chrysolepsis sempervirens" ~ "Chrysolepis sempervirens",
    species == "Circium quercetorum" ~ "Cirsium quercetorum",
    species == "Conicum maculatum" ~ "Conium maculatum",
    species == "Cornus nuttalii" ~ "Cornus nuttallii",
    species == "Cryptogamma arcostichoides" ~ "Cryptogramma acrostichoides", 
    species == "Downed wood" ~ "wood",
    species %in% c("Hieracium hordium", "Hieracium horridium") ~ "Hieracium horridum",
    species == "Juncus paryii" ~ "Juncus parryi",
    species == "Juniper occidentalis" ~ "Juniperus occidentalis",
    species == "Kellogia galiodies" ~ "Kelloggia galioides",
    species == "Leptosiphon ciliatum" ~ "Leptosiphon ciliatus"
    species == "Monardella ordoratissima" ~ "Monardella odoratissima", 
    species == "Osmorhiza bertoli" ~ "Osmorhiza berteroi", 
    species == "Penstemon newberryii" ~ "Penstemon newberryi",
    species == "Phacelia mutilabis" ~ "Phacelia mutabilis",
    species == "Phyllodece breweri" ~ "Phyllodoce breweri",
    species == "Quercus chrysolepsis" ~ "Quercus chrysolepis"
    species == "Quercus kellogii" ~ "Quercus kelloggii",
    species == "Ribes viscosisimum" ~ "Ribes viscosissimum",
    species == "Symphocarpus roundifolius" ~ "Symphoricarpos rotundifolius",
    species == "Symphotrichum ascendens" ~ "Symphyotrichum ascendens",
    species == "Gallium sparsiflorum" ~ "Galium sparsiflorum"
    ))

#replace common names with scientific
scinames <- spellcheck %>% 
  mutate(species = case_when(
    species == "grass" ~ "Poaceae"
  ))