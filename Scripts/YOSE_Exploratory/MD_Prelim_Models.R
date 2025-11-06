# read in understory richness and plot level data 

# read in associated tree data
treeList <- read.csv("/Users/jennifercribbs/Documents/YOSE/Analysis/MultipleDisturbances/Data/CleanData/CleanTreeList.csv") %>% select(plot, treeNum, species, dOut_m, dSide, DBH_cm, height_m, percentLive, damageCodes, notes) %>% rename(plotID = plot)

# Create a stem map for each plot
# list unique plot IDs
plots <- unique(treeList$plotID)

# Loop through each plot
for (plot in plots) {
  plot_data <- treeList %>% filter(plotID == plot)
  
  # Create the plot
  p <- ggplot(plot_data, aes(x = dSide, y = dOut_m, label = treeNum)) +
    geom_point(aes(color = species), size = 2) +
    geom_text(vjust = -0.5, hjust = 0.5) +
    labs(title = paste("Stem Map for Plot", plot),
         x = "Distance to Side (m)",
         y = "Distance Out (m)") +
    theme_minimal() +
    #scale_color_manual(values = c("PIJE" = "green", "UNKNOWN" = "red")) +
    coord_fixed()
  
  # Print the plot to the R console
  print(p)
  
  # Save the plot to a file
  ggsave(filename = paste0("Stem_Map_Plot_", plot, ".png"), plot = p)
}

treeList %>% group_by(plotID) %>% 
  ggplot(aes(x = dSide, y = dOut_m, label = treeNum)) +
  geom_point(aes(color = species), size = 3) +
  geom_text(vjust = -0.5) +
  labs(title = paste("Stem Map for Plot", plot),
       x = "Distance to Side (m)",
       y = "Distance Out (m)") +
  theme_minimal() +
  #scale_color_manual(values = c("PIJE" = "green", "UNKNOWN" = "red")) +
  coord_fixed()
  # Save the plot to a file
#ggsave(filename = paste("Stem_Map_Plot_", plot, ".png", sep = ""), plot = p)

# Create a stem map using facet_wrap
p <- ggplot(treeList, aes(x = dSide, y = dOut_m, label = treeNum)) +
  geom_point(aes(color = species), size = 1) +
  geom_text(vjust = -0.1, hjust = 0.1) +
  labs(title = "Stem Maps by Plot",
       x = "Distance to Side (m)",
       y = "Distance Out (m)") +
  theme_minimal() +
  #scale_color_manual(values = c("PIJE" = "green", "UNKNOWN" = "red")) +
  coord_fixed() +
  facet_wrap(~ plotID)

# Print the plot
print(p)

# Save the plot to a file
#ggsave(filename = "Stem_Maps_All_Plots.png", plot = p, width = 12, height = 8)

# will need to add columns to match pila data--> rbind actually did this automatically, but might be better to create each column to determine the type of NA, but working for now

# read combined data from combined_data.R--need to finalize a clean version
# associated trees is clean, but PILA still needs work

#combined_data <- read.csv("combined_data")

# calculate density of all trees 
# density = total number of overstory trees + total overstory PILA / length * width

library(dplyr)

# Define the plot area--this will vary, but assuming 2,000m per plot to get started
plot_area <- 100 * 20

# Calculate the density
density <- combined_data %>%
  group_by(plotID) %>%
  summarise(total_trees = n()) %>%
  mutate(density = total_trees / plot_area)

# View the results
print(density)

# calculate density of pila
# pila density = count of PILA / plot area
# Calculate overall tree density
overall_density <- combined_data %>%
  group_by(plotID) %>%
  summarise(total_trees = n()) %>%
  mutate(overall_density = total_trees / plot_area)

# Calculate PILA density
pila_density <- combined_data %>%
  filter(species == "PILA") %>%
  group_by(plotID) %>%
  summarise(total_pila_trees = n()) %>%
  mutate(pila_density = total_pila_trees / plot_area)

# Combine the results into one data frame
combined_density <- overall_density %>%
  left_join(pila_density, by = "plotID") %>%
  mutate(pila_density = replace_na(pila_density, 0))  # Replace NA with 0 if no PILA trees in some plots

# View the plot-level density results
print(combined_density)


# relative density = count PILA / count total trees 

# calculate overstory tree richness (unique tree species in plot)




# figure out a way to consider correlation given the two variables are not continuous

# display percent infected for high severity and random plots
# subset data into high severity and random plots


# logistic glmm models should work for wpbr and beetles (infected or not)

library(MASS)
library(nlme)
model1 <- glm(infected ~ Fire + DBH_cm + 1|plotID, family = binomial, data = piladat)
summary(lme(infected ~ Fire + DBH_cm + 1|plotID, family = binomial, data = piladat))

# assume 100 by 20 plot for now
plot_area <- 100 * 20
# Calculate overall tree density
overall_density <- combined_data %>%
  group_by(plotID) %>%
  summarise(total_trees = n()) %>%
  mutate(overall_density = total_trees / plot_area)

# Calculate PILA density
pila_density <- combined_data %>%
  filter(species == "PILA") %>%
  group_by(plotID) %>%
  summarise(total_pila_trees = n()) %>%
  mutate(pila_density = total_pila_trees / plot_area)

# Combine the results into one data frame
combined_density <- overall_density %>%
  left_join(pila_density, by = "plotID") %>%
  mutate(pila_density = replace_na(pila_density, 0))  # Replace NA with 0 if no PILA trees in some plots

# View the results
print(combined_density)

combined_density <- left_join(combined_density, prism)
combined_density <- left_join(combined_density, fire)

model1 <- lm(richness ~ vpdmax * Fire_Severity_Beg * + overall_density, data = combined_density)
model2 <- lm(richness ~ vpdmax * predicted_CBI + overall_density, data = combined_density)

AIC(model1, model2)
