# Plot Level PCA Analysis
# 28 February 2024
# Jenny Cribbs
# For Sierra Nevada Science Symposium Addendum

# create waypoints for export for prism data extraction 
#waypoints <- read_csv("/Users/jennifercribbs/Documents/YOSE/Plot_Waypoints_UTMzone.csv")
#plotlist <- unique(piladat$plotID)
#plotlist %>% arrange(plotID)
#waypoints$plotID

# bring in PRISM data
#prism <- read_csv("/Users/jennifercribbs/Downloads/updatedPRISMdata.csv")

# join tree data and prism data on plotID 
#plot_tree <- piladat %>% left_join(prism, by = c("plotID" = "PlotID"))

# bring in climate_fire data saved from PlotTreeLevel script
# climate_fire <- read_csv("Climate.Fire.csv")

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


