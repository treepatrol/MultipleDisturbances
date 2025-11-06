# adapted from https://ourcodingclub.github.io/tutorials/brms/
# for Sierra Nevada Science Symposium

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
# display histogram--so many zeros!
hist_pila
# look at the number of years
unique(piladat$canker_count)
# install brms pacakge 
#install.packages("brms")  
library(brms)
# model 1--does not converge rhat > 1.05
# Do not try again--seems to create a model for each tree?

summary(piladat)
class(piladat$infected)

# data cleaning side note
unique(piladat$plot_type) # random (1313), highseverity (384), high_severity (2), NA

# add plot-level variables to tree data set
piladat_plot <- piladat %>% left_join(climate_fire, by = c("plotID" = "PlotID"))


#logistic_normal(link = "identity", link_sigma = "log", refcat = NULL)
binary1 <- brms::brm(formula = infected ~ avg_fire_severity,
                     data = piladat_plot,
                     family = bernoulli(link = "logit"),
                     warmup = 500,
                     iter = 2000,
                     chains = 2,
                     init = "0",
                     cores = 2)
# check for convergence
stanplot(binary1, type = "trace") # caterpillars look good for one run, not another--> more iterations?
# look at model summary
summary(binary1) # doesn't include zero now

# compare to glm version 
binary1gml <- glm(formula = infected ~ avg_fire_severity,
                  family = binomial(link="logit"),
                  data = piladat_plot)
summary(binary1gml)

mcmc_plot(binary1, type = "areas", prob = 0.95)

# interpret parameters
exp(fixef(binary1)[,-2])

# plot parameter estimates
mcmc_plot(binary1, 
         type = "areas",
         prob = 0.95,
         transformations = "exp") +
  geom_vline(xintercept = 1, color = "grey")


