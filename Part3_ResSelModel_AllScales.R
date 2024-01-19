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

##

dwplot(Course_mod)
dwplot(MCP_mod)
dwplot(Property_mod)# By default, the whiskers span the 95% confidence interval
help(dwplot) # to see the syntax 

FullYear_mods = list(MCP_mod, Course_mod, Property_mod)

plot1 = dwplot(FullYear_mods,
               ci = 0.95, 
               dodge_size = 0.4, # how far apart pts are frome eachother (0.4 = default) 
               show_intercept = FALSE, 
               model_order = NULL, 
               dot_args = list(size = 1.2), 
               vline = geom_vline(xintercept = 0, linetype = 2, colour ="grey8")) 
# It works until this point 
# continue fixing plots----
+
    relabel_predictors(
      c(
        scale(DTN_road) = "Distance to Nearest Road (m)",
        scale(perc_grassy) = "Percent Grassy Cover",
        scale(perc_bf) = "Percent Broodfield",
        scale(ndvi) = "NDVI"
      )
    )
######################################################
