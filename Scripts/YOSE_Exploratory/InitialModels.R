
library(tidyverse)
# bring in clean YOSE Data
# PILA data
pilaData <- read_csv("/Users/jennifercribbs/Documents/YOSE/Analysis/MultipleDisturbances/gbifExcelOccurence.csv")
# plot data from the field
plotData <- read_csv("/Users/jennifercribbs/Documents/YOSE/Analysis/MultipleDisturbances/Data/CleanData/PlotLevelData.csv")
# bring in fire data
fire <- read_csv("/Users/jennifercribbs/Documents/YOSE/Analysis/MultipleDisturbances/predictedCBI_ground_check.csv")
# bring in richness and climate data
prismRich <- read_csv("/Users/jennifercribbs/Documents/YOSE/Analysis/MultipleDisturbances/prismFireRichness.csv")
# make plotID numeric
prismRich$plotID <- as.numeric(prismRich$plotID)
# join fire and richness data
firePrismRich <- left_join(fire, prismRich)
# join with plotData
fullplotData <- left_join(plotData, firePrismRich)
# calculate plot area for mature PILA
fullPlotData <- fullplotData %>% mutate(area = width_pila * trans_length)
# calculate PILA density
pilaCount <- pilaData %>% group_by(eventID) %>% summarize(pila_count = n()) 
fullPlotData <- left_join(fullPlotData, pilaCount, by = c("plotID" = "eventID"))
fullPlotData <- fullPlotData %>% mutate(pilaDensity = (pila_count/area))
# calculate wpbr incidence
pilaData <- pilaData %>% 
  mutate(bole_canker = activeBoleCanker + inactiveBoleCanker,
         branch_canker = activeBranchCanker + inactiveBranchCanker) %>% 
  mutate(canker_count = branch_canker + bole_canker) %>% 
  mutate(infected = ifelse(bole_canker|branch_canker>0,1,0))
ic <- pilaData %>%
  group_by(eventID) %>%
  summarize(infected_count = sum(infected))
fullPlotData <- left_join(fullPlotData, ic, by = c("plotID" = "eventID"))
fullPlotData <- fullPlotData %>% mutate(wpbrIncidence = infected_count/pila_count)
# Prediction 1: Fire and drought may act synergistically to reduce WPBR 

# WPBR ~ fire * VPD + live PILA density (≥ 1m) + Alt host density + (1|eventID) (using all plots)
m1 <- glm(wpbrIncidence ~ Fire_Severity_plotAverage * vpdmax + pilaDensity, data = fullPlotData, family = gaussian)
m2 <- glm(wpbrIncidence ~ Fire_Severity_plotAverage, data = fullPlotData, family = gaussian)
m3 <- glm(wpbrIncidence ~ vpdmax, data = fullPlotData, family = gaussian)
m4 <- glm(wpbrIncidence ~ vpdmax + pilaDensity, data = fullPlotData, family = gaussian)
m5 <- glm(wpbrIncidence ~ Fire_Severity_plotAverage + pilaDensity, data = fullPlotData, family = gaussian)
m6 <- glm(wpbrIncidence ~ pilaDensity, data = fullPlotData, family = gaussian)
AIC(m1, m2, m3, m4, m5, m6)

# is gaussian the best choice?
# Check the distribution of wpbrIncidence
hist(fullPlotData$wpbrIncidence, main = "Distribution of wpbrIncidence", xlab = "wpbrIncidence") # skewed towards zero, not normal 

# Binomial regression (logit link) for plot-level data
m_binomial <- glm(cbind(infected_count, pila_count - infected_count) ~ Fire_Severity_plotAverage * vpdmax + pilaDensity, 
                  data = fullPlotData, family = binomial(link = "logit"))

# use lme4 with plot as a random effect
# group level effects in multilevel models 
# alternate host density--could keep this plot level (stuck with this for line intercept) or use closest prescence abscence 50m chunk
library(lme4)

m_binomial_lme4 <- glmer(
  cbind(infected_count, pila_count - infected_count) ~ 
    Fire_Severity_plotAverage * vpdmax + pilaDensity + 
    (1 | plotID),
  data = fullPlotData, 
  family = binomial(link = "logit")
)

summary(m_binomial_lme4)
ranef(m_binomial_lme4)
anova(m_binomial, m_binomial_lme4, test = "Chisq")



# Check AplotID# Check AIC to assess the fit
summary(m_binomial)

# Check diagnostics (residuals, etc.)
plot(resid(m_binomial))

# Extract residuals
deviance_residuals <- residuals(m_binomial, type = "deviance")
pearson_residuals <- residuals(m_binomial, type = "pearson")
working_residuals <- residuals(m_binomial, type = "working")

# Check the first few residuals
head(deviance_residuals)
head(pearson_residuals)

# Plot residuals vs. fitted values (predicted probabilities)
fitted_values <- predict(m_binomial, type = "response")

# Plot deviance residuals vs fitted values
plot(fitted_values, deviance_residuals, 
     xlab = "Fitted Values", ylab = "Deviance Residuals", 
     main = "Residuals vs. Fitted Values")
abline(h = 0, col = "red", lty = 2)  # Add horizontal line at 0

# Histogram of deviance residuals
hist(deviance_residuals, breaks = 20, main = "Histogram of Deviance Residuals", 
     xlab = "Deviance Residuals", col = "lightblue")

# Q-Q plot of deviance residuals
qqnorm(deviance_residuals, main = "Q-Q Plot of Deviance Residuals")
qqline(deviance_residuals, col = "red")

# Plot Pearson residuals vs fitted values
plot(fitted_values, pearson_residuals, 
     xlab = "Fitted Values", ylab = "Pearson Residuals", 
     main = "Pearson Residuals vs. Fitted Values")
abline(h = 0, col = "red", lty = 2)  # Horizontal line at 0

# Calculate leverage and Cook's distance
leverage <- hatvalues(m_binomial)
cooks_distance <- cooks.distance(m_binomial)

# Identify points with high leverage or influence
plot(leverage, cooks_distance, 
     xlab = "Leverage", ylab = "Cook's Distance", 
     main = "Leverage vs. Cook's Distance")
abline(h = 4 / length(fitted_values), col = "red", lty = 2)  # Threshold for high influence

# check for overdispersion
residual_deviance <- m_binomial$deviance
df_residual <- m_binomial$df.residual
ratio <- residual_deviance / df_residual
print(ratio)

# Plot deviance residuals to identify outliers
plot(deviance_residuals, main = "Deviance Residuals")
abline(h = 0, col = "red", lty = 2)  # Horizontal line at 0

# Trying a zero-inflated negative binomial model
# Install and load the pscl package if you haven't already
install.packages("pscl")
library(pscl)

# Fit the zero-inflated binomial model
zip_model <- zeroinfl(cbind(infected_count, pila_count - infected_count) ~ 
                        Fire_Severity_plotAverage * vpdmax + pilaDensity | 
                        Fire_Severity_plotAverage + vpdmax,
                      data = fullPlotData, 
                      dist = "negbin")

# View the model summary
summary(zip_model)


# Install the lme4 package if necessary
# install.packages("lme4")

library(lme4)

# Model 1: Full interaction model
m1 <- lmer(wpbrIncidence ~ Fire_Severity_plotAverage * vpdmax + pilaDensity + (1 | plotID), data = fullPlotData, family = gaussian)

# Model 2: Main effect of Fire_Severity_plotAverage
m2 <- lmer(wpbrIncidence ~ Fire_Severity_plotAverage + (1 | plotID), 
           data = fullPlotData, family = gaussian)

# Model 3: Main effect of vpdmax
m3 <- lmer(wpbrIncidence ~ vpdmax + (1 | plotID), 
           data = fullPlotData, family = gaussian)

# Compare AIC
AIC(m1, m2, m3)

# Prediction 2: Fire * drought * WPBR are associated with the greatest negative effects on PILA recruitment (using plots with ≥ 1 live PILA) 

# PILA recruitment ~ fire * VPD * (distance to nearest WPBR infection) + live PILA density + deadPILA density + Alt host density (using all plots)

# Question: As more drivers of tree mortality co-occur (and/or interact) in communities, do they become more diverse or does it really depend on the disturbance type? 
  
# Richness understory ~ fire * VPD * WPBR (using all plots)
# Richness overerstory ~ fire * VPD * WPBR (using all plots)