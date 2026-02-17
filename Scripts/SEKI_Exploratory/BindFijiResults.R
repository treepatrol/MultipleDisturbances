library(tidyverse)
# read in FIJI results table for each saved work session 
part1 <- read_csv("/Users/jennifercribbs/Downloads/PLN0201B_2400dpi_svb_20260203_label_end280.csv.csv")
part2 <- read_csv("/Users/jennifercribbs/Downloads/Results_PLN0201B_2400dpi_svb_20260203_label_part3.csv")
part3 <- read_csv("/Users/jennifercribbs/Downloads/Results_PLN0201B_2400dpi_svb_20260203_label_lastpart.csv")

# bind the parts together
full <- rbind(part1, part2, part3)

# write out complete csv (dumps into multiple disturbances)
write_csv(full, "Results_PLN0201B.csv")

# note rbind results retain the original numbering e.g. 1-280, 1-243, and 1-87
# convenient for comparing to image
# make sure to create a unique id for database

# PMN0914A JEC did part 1: 1-50; ASN did part 2: 1-390; JEC did part 4: 1-175
library(tidyverse)
# read in FIJI results table for each saved work session 
part1 <- read_csv("/Users/jennifercribbs/Downloads/Results_PMN0914A_part1-2.csv")
part2 <- read_csv("/Users/jennifercribbs/Downloads/Results_PMN0914A_part2-3.csv")
part3 <- read_csv("/Users/jennifercribbs/Downloads/Results_PMN0914A_part4_end.csv")

# bind the parts together
full <- rbind(part1, part2, part3)

# write out complete csv (dumps into multiple disturbances)
write_csv(full, "Results_PLN0914A.csv")

# note rbind results retain the original numbering e.g. 1-280, 1-243, and 1-87
# convenient for comparing to image
# make sure to create a unique id for database
