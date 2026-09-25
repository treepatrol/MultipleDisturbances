library(readxl)
library(tidyverse)

files <- list.files(
  path = "Vegetation",
  pattern = "\\.xlsx$",
  full.names = TRUE
)

veg <- map_df(files, function(f){
  
  dat <- read_excel(f)
  
  dat$plot_id <- tools::file_path_sans_ext(basename(f))
  
  dat
})