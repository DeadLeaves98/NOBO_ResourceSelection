# Title: Resource Selection Model at All Scales (3) ----
# Author: Autumn Randall 
# PI: Dr. DJ. McNeil 
# Date created: 1/17/2024
# Date Last Editted: 1/17/2024
###################################

#### MCP: Resource Selection Model  ----
# Read in csv from github 
nobo_MCP = read.csv("./ResSelData_MCP.csv")

MCP_mod = glmer(response ~ scale(DTN_road) + scale(perc_grassy) + scale(perc_bf) + scale(ndvi) +(1|Bird.ID), family = binomial, data = nobo_MCP)
summary(MCP_mod)
# Scale makes covariates relative to one another 
hist(nobo1$DTN_road)
hist(scale(nobo1$DTN_road))

#### Course: Resource Selection Model ---- 
nobo_Course = read.csv("./ResSelData_Course.csv")

Course_mod = glmer(response ~ scale(DTN_road) + scale(perc_grassy) + scale(perc_bf) + scale(ndvi) +(1|Bird.ID), family = binomial, data = nobo_Course)
summary(Course_mod)

#### Property AKA orton: Resource Selection Model ---- 
nobo_Property = read.csv("./ResSelData_Property.csv")

Property_mod = glmer(response ~ scale(DTN_road) + scale(perc_grassy) + scale(perc_bf) + scale(ndvi) +(1|Bird.ID), family = binomial, data = nobo_Property)
summary(Property_mod)

############################################################################

## ggplot figure 
## Dot Whisker plot attempt ---- 

library(dotwhisker)
library(dplyr)

dwplot(Course_mod) # By default, the whiskers span the 95% confidence interval

dwplot(list(MCP_mod, Course_mod, Property_mod),
       dodge_size = .4,
       vline = geom_vline(xintercept = 0,colour = "grey8", linetype = 2), 
       vars_order = c("DTN_road", "perc_grassy", "perc_bf", "ndvi"),) %>%
       relabel_predictors(
         c(DTN_road = "Distance to Nearest Road (m)",
           perc_grassy = "Percent Grassy Cover",
           perc_bf = "Percent Broodfield",
           ndvi = "NDVI"))
       
help(dwplot)
