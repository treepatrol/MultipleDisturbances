# adapted from https://ourcodingclub.github.io/tutorials/brms/

# Set the working directory
setwd("/Users/jennifercribbs/Documents/YOSE/Analysis/")
# Load initial packages
library(tidyverse)
# Load the data
if (!require("librarian")) install.packages("librarian")
librarian::shelf(tidyverse, readxl, viridis, patchwork)

theme_set(
  theme_bw(base_size = 17)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
)

files <- list.files()
outdir <- "/Users/jennifercribbs/Documents/YOSE/Analysis/YPE_Data"

## unzipping files on your computer
setwd("/Users/jennifercribbs/Documents/YOSE/Analysis/YPE_Data")
files <- list.files()
outdir <- "/Users/jennifercribbs/Documents/YOSE/Analysis/YPE_Data"

## for loop to unzip files on your computer
# for (file in files) {
#   unzip(paste0(outdir,"/", file))
# }


## reading in all data
folders <- list.dirs(outdir)[-c(1,4)]

tree_list <- data.frame()

for (folder in folders) {
  newfiles = list.files(folder, pattern = "PILAdata")
  for(file in newfiles) {
    xlsfile <- read_excel(paste0(folder,"/",file)) %>% 
      dplyr::select(plotID, plot_type, DBH_cm, height_m, pitchTubes, exitHoles, activeBranchCanker, inactiveBranchCanker, 
                    activeBoleCanker, inactiveBoleCanker, DTOP, flags, percentLive)
    tree_list <- rbind(tree_list, xlsfile)
  }
}


## summarizing data
summary(tree_list)

#summary(aggr(tree_list))

# change data type to numeric 
tree_list$activeBoleCanker <- as.numeric(tree_list$activeBoleCanker) 
tree_list$inactiveBoleCanker <- as.numeric(tree_list$inactiveBoleCanker)
tree_list$activeBranchCanker <- as.numeric(tree_list$activeBranchCanker)
tree_list$inactiveBranchCanker <- as.numeric(tree_list$inactiveBranchCanker)

# create new columns combining active and inactive bole and branch cankers 
piladat <- tree_list %>% 
  mutate(bole_canker = activeBoleCanker + inactiveBoleCanker,
         branch_canker = activeBranchCanker + inactiveBranchCanker) %>% 
  mutate(canker_count = branch_canker + bole_canker) %>% 
  mutate(infected = ifelse(bole_canker|branch_canker>0,1,0))

head(piladat)  # to get the first observations in each column
str(piladat)  # what type of variables do we have

# Question: Has the sugar pine population in SEKI decreased over time? (pre-invasion, 1990s, 2010s)
# Question: Has the sugar pine population in YOSE decreased over time? (pre-invasion, 2023)

# create a histogram to determine the distribution
(hist_pila <- ggplot(piladat, aes(x = canker_count)) +
    geom_histogram(colour = "#8B5A00", fill = "#CD8500") +
    theme_bw() +
    ylab("Count\n") +
    xlab("\nPinus lambertiana canker count") + 
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain")))              
# display histogram 
hist_pila
# look at the number of years
unique(piladat$canker_count)
# install brms pacakge 
#install.packages("brms")  
library(brms)
# model 1--does not converge rhat > 1.05
# Do not try again--seems to create a model for each tree?

piladat$DBH_cm <- as.numeric(piladat$DBH_cm)

summary(piladat)
class(piladat$infected)

# data cleaning side note
unique(piladat$plot_type) # random (1313), highseverity (384), high_severity (2), NA


piladat %>% filter(plot_type == "highseverity") # converted plot 5 and 8 to highseverity

piladat %>% group_by(plot_type) %>% 
  summarize(infected = sum(infected, na.rm = TRUE))

pila1_mbrms <- brms::brm(canker_count ~ (DBH_cm),
                           data = piladat, family = poisson(), chains = 3,
                           iter = 3000, warmup = 1000)
summary(pila1_mbrms)
plot(pila1_mbrms)
pp_check(pila1_mbrms)  # posterior predictive checks

pila2_mbrms <- brms::brm(canker_count ~ (plot_type),
                         data = piladat, family = poisson(), chains = 3,
                         iter = 3000, warmup = 1000)
summary(pila2_mbrms)
plot(pila2_mbrms)

pp_check(pila2_mbrms)  # posterior predictive checks

pila3_mbrms <- brms::brm(infected ~ (plot_type),
                         data = piladat, family = logistic_normal(link = "identity", link_sigma = "log", refcat = NULL)
, chains = 3, iter = 3000, warmup = 1000)
tree_list %>% unique(plotID)

#logistic_normal(link = "identity", link_sigma = "log", refcat = NULL)
binary1 <- brms::brm(formula = infected ~ plot_type,
                     data = piladat,
                     family = bernoulli(link = "logit"),
                     warmup = 500,
                     iter = 2000,
                     chains = 2,
                     init = "0",
                     cores = 2)
# check for convergence
stanplot(binary1, type = "trace") # caterpillars look good for one run, not another--> more iterations?
# look at model summary
summary(binary1)

# compare to glm version 
binary1gml <- glm(formula = infected ~ plot_type,
                  family = binomial(link="logit"),
                  data = piladat)
summary(binary1gml)

mcmc_plot(binary1, type = "areas", prob = 0.95)

# interpret parameters
exp(fixef(binary1)[,-2])

# plot parameter estimates--looks ugly
mcmc_plot(binary1, 
         type = "areas",
         prob = 0.95,
         transformations = "exp") +
  geom_vline(xintercept = 1, color = "grey")


