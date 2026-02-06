part1 <- read_csv("/Users/jennifercribbs/Documents/SEKI_beetles/TreeCorePhotos/Annotated/Results_PLS0109A_part1.csv")
part2 <- read_csv("/Users/jennifercribbs/Documents/SEKI_beetles/TreeCorePhotos/Annotated/Results_PLS0109A_part2.csv")

full <- rbind(part1, part2)

write_csv(full, "/Users/jennifercribbs/Documents/SEKI_beetles/TreeCorePhotos/Annotated/Results_PLS0109A_full.csv")
