
# packages
#install.packages(c("ggplot2", "ggpubr", "tidyverse", "broom", "AICcmodavg"))
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)
# Bring in Data ----------------------------------------------------------
# read in data from script 4_UnderstorySpeciesRichness.R
understoryRich <- read.csv("prismFireRichness.csv")

# read in manually reviewed fire severity (MTBS, plot assessment, plot photos)
fireSeverityChecked <- read.csv("/Users/jennifercribbs/Documents/R-Projects/MultipleDisturbances/dataSandbox/fireCompare_22052024.csv")
# read in data from PlotLevelData.R

understoryFire <- fireSeverityChecked %>% select(plotID, mtbs_severity, Fire_Severity_Beg, Fire_Severity_plotAverge) %>% left_join(understoryRich) %>% select(-severity)

#treelist <- read.csv("dataSandbox/treelist.csv")

# join on the ground fire data with richness and other plot data
#understoryFire <- left_join(understoryFire, treelist)

# Boxplots for MTBS Fire Severity ------------------------------------------------
# take a look at relationship btw richness and MTBS fire severity
understoryFire %>%
  ggplot() + geom_boxplot(mapping = aes(mtbs_severity, richness))

# identify outliers plots 9 (El Cap had a creek) and 54 
outliers <- understoryRich %>% filter(richness > 35)

# Test for Statistical Significance ---------------------------------------

aov(richness ~ mtbs_severity, data = understoryFire)

# Subset the data into two groups: burned and unburned
burned_group <- subset(understoryFire, Fire_Severity_Beg == 1|2|3)$richness
unburned_group <- subset(understoryFire, Fire_Severity_Beg == 0)$richness
# Perform independent samples t-test
t_test_result <- t.test(burned_group, unburned_group)
# Print the result--no statistically significance in burned and unburn richness means
print(t_test_result)

# There is no statistically significant difference between the means of richness when fire is lumped into one category

# Boxplots for Fire Severity ----------------------------------------------

# Create a ggplot with rounded fire severity
plotsev <- ggplot(understoryFire, aes(x = as.factor(Fire_Severity_Beg), y = richness, fill = as.factor(Fire_Severity_Beg))) +
  geom_boxplot(notch = FALSE) +
  xlab("Plot Fire Severity") +
  ylab("Species Richness") +
  scale_fill_manual(values = c("0" = "blue", "1" = "yellow", "2" = "orange", "3" = "red")) + 
  guides(fill = guide_legend(title = "Fire Severity"))

plotsev

ggsave("RichFire_plotSeverity.png", plot = plotsev)

plotviolin <- ggplot(understoryFire, aes(x = as.factor(Fire_Severity_Beg), y = richness, fill = as.factor(Fire_Severity_Beg))) +
  geom_violin() +
  xlab("Plot Fire Severity") +
  ylab("Species Richness") +
  scale_fill_manual(values = c("0" = "blue", "1" = "yellow", "2" = "orange", "3" = "red")) + 
  guides(fill = guide_legend(title = "Fire Severity"))

plotviolin
ggsave("RichFire_plotSeverityViolin.png", plot = plotviolin)

# test for significance between one or more categories 
aov(richness ~ Fire_Severity_Beg, data = understoryFire)
# look at trends in fire and climate variables
plot(understoryFire$Fire_Severity_Beg, understoryFire$richness)
plot(understoryFire$vpdmax, understoryFire$richness)
plot(understoryFire$tmean, understoryFire$richness)
plot(understoryFire$ppt, understoryFire$richness)

# should add plot as a random effect, but need to convert it back to numeric--maybe add year as a random effect?
# figure margins too large--may need to scale first before plotting and modeling

plot(understoryFire$richness ~ understoryFire$Fire_Severity_Beg)
abline(lm(richness ~ Fire_Severity_Beg + time_since + vpdmax + tmean + ppt, data = understoryFire))

# read in manually reviewed data
fireCompare <- read.csv("/Users/jennifercribbs/Documents/R-Projects/MultipleDisturbances/dataSandbox/fireCompare_11052024JEC.csv") 

ggplot(understoryFire, aes(x = as.factor(Fire_Severity_Beg), y = richness, fill = as.factor(Fire_Severity_Beg))) +
  geom_boxplot(notch = FALSE) +
  geom_jitter(width = 0.0, alpha = 0.5) +
  xlab("Plot Fire Severity") +
  ylab("Species Richness") +
  scale_fill_manual(values = c("0" = "blue", "1" = "yellow", "2" = "orange", "3" = "red")) + 
  guides(fill = guide_legend(title = "Fire Severity"))

aov(richness ~ Fire_Severity_Beg, data = understoryFire)
summary(aov(richness ~ Fire_Severity_Beg, data = understoryFire))

#lm(understoryFire$richness ~ understoryFire$Plot_Severity_Beg)

