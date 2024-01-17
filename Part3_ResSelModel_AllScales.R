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
