# Title: Resource Selection Temporal Scale  ----
# Author: Autumn Randall 
# PI: Dr. DJ. McNeil 
# Date created: 1/17/2024
# Date Last Editted: 1/17/2024
###################################

# Goal:
# We have already run the full year with all the data. 
# I want to split the data up based on 2 other temporal scales: 
# 1. Annual (done) 
# 2. Bi-Annual: Breeding vs Nonbreeding 
# 3. Seasonal: Spring, Summer, Fall, Winter 

# This will be using the Course Scaled data 
# This can easily be changed, but it seemed that the home range scale (MCP) had the 
# least magnitude of affect on NOBO resource selection. On a management scale that 
# has different courses/sites that they manage for and each may need their own management regime 
# to achieve the desired results due to heterogeneity of the landscape (e.g. soil quality, vegetation 
# cover, proximity to urban neighborhoods). This is mainly referencing below the creek and above the creek 
# Maybe this is not a sound thought process -- we should discuss 

##############################################################################

nobo_c = read.csv("./ResSelData_Course.csv") # read in the course level data 

##### Breeding vs nonbreeding -- COURSE ####

#     breeding data subset
breeding22 <- nobo_c[nobo_c$Date >= "2022-04-01" & nobo_c$Date <= "2022-09-30", ]
breeding23 <- nobo_c[nobo_c$Date >= "2023-04-01" & nobo_c$Date <= "2023-09-30", ]
breeding = rbind(breeding22, breeding23)
nrow(nobo_breeding) # 54735

# nonbreeding data subset
non_breeding1 = nobo_c[nobo_c$Date >= "2022-01-01" & nobo_c$Date <= "2022-03-31", ]
non_breeding2 = nobo_c[nobo_c$Date >= "2022-10-01" & nobo_c$Date <= "2023-03-31", ]
non_breeding3 = nobo_c[nobo_c$Date >= "2023-10-01" & nobo_c$Date <= "2023-12-31", ]
nonbreeding = rbind(non_breeding1, non_breeding2, non_breeding3)
nrow(nonbreeding) # 17826

##### Models: breeding vs nonbreeding  #### 
library(dotwhisker)
library(dplyr)

nonbreeding_mod = glmer(response ~ scale(DTN_road) + scale(perc_grassy) + scale(perc_bf) + scale(ndvi) +(1|Bird.ID), family = binomial, data = nonbreeding)

breeding_mod = glmer(response ~ scale(DTN_road) + scale(perc_grassy) + scale(perc_bf) + scale(ndvi) +(1|Bird.ID), family = binomial, data = breeding)

Fig1 = dwplot(list(breeding_mod, nonbreeding_mod))


################################################################################

# out of curiousity 
nobo_p = read.csv("./ResSelData_Property.csv") # read in the course level data 

##### Breeding vs nonbreeding -- PROPERTY  ####

#     breeding data subset
breeding22_p <- nobo_p[nobo_p$Date >= "2022-04-01" & nobo_p$Date <= "2022-09-30", ]
breeding23_p <- nobo_p[nobo_p$Date >= "2023-04-01" & nobo_p$Date <= "2023-09-30", ]
breeding_p = rbind(breeding22_p, breeding23_p)
nrow(nobo_breeding) # 54735

# nonbreeding data subset
non_breeding1_p = nobo_p[nobo_p$Date >= "2022-01-01" & nobo_p$Date <= "2022-03-31", ]
non_breeding2_p = nobo_p[nobo_p$Date >= "2022-10-01" & nobo_p$Date <= "2023-03-31", ]
non_breeding3_p = nobo_p[nobo_p$Date >= "2023-10-01" & nobo_p$Date <= "2023-12-31", ]
nonbreeding_p = rbind(non_breeding1_p, non_breeding2_p, non_breeding3_p)
nrow(nonbreeding_p) # 17826

##### Models: breeding vs nonbreeding  #### 
library(dotwhisker)
library(dplyr)

nonbreeding_mod_p = glmer(response ~ scale(DTN_road) + scale(perc_grassy) + scale(perc_bf) + scale(ndvi) +(1|Bird.ID), family = binomial, data = nonbreeding_p)

breeding_mod_p = glmer(response ~ scale(DTN_road) + scale(perc_grassy) + scale(perc_bf) + scale(ndvi) +(1|Bird.ID), family = binomial, data = breeding_p)

# COMPARES AT A PROPERTY SCALE 
Fig2 = dwplot(list(breeding_mod_p, nonbreeding_mod_p))

 # does not seem to be too much of a difference when it comes to the outcome of the data 
 # It seems maybe the confidence intervals get a little larger at a home range scale 
 # but they do not have an influence on use of covertypes/landscape features 

###################################################################################
###################################################################################
###################################################################################

#### Seasons: Spring, Summer, Fall, Winter 

# Already ran this in but just in case 
nobo_c = read.csv("./ResSelData_Course.csv") # read in the course level data 

##### Breeding vs nonbreeding ####

# Meteorological Seasons
# spring runs from March 1 to May 31; summer runs from June 1 to August 31; fall
# (autumn) runs from September 1 to November 30; and. winter runs from December 1 to February 28 (February 29 in a leap year).

# spring 
spring22 = nobo_c[nobo_c$Date >= "2022-03-01" & nobo_c$Date <= "2022-05-31", ]
spring23 = nobo_c[nobo_c$Date >= "2023-03-01" & nobo_c$Date <= "2023-05-31", ]
spring = rbind(spring22, spring23)


# summer 
summer22 = nobo_c[nobo_c$Date >= "2022-06-01" & nobo_c$Date <= "2022-08-31", ]
summer23 = nobo_c[nobo_c$Date >= "2023-06-01" & nobo_c$Date <= "2023-08-31", ]
summer = rbind(summer22, summer23)

# fall 
fall22 = nobo_c[nobo_c$Date >= "2022-09-01" & nobo_c$Date <= "2022-11-30", ]
fall23 = nobo_c[nobo_c$Date >= "2023-09-01" & nobo_c$Date <= "2023-11-30", ]
fall = rbind(fall22, fall23)

# winter 
winter22 = nobo_c[nobo_c$Date >= "2022-12-01" & nobo_c$Date <= "2023-02-28", ]
winter23 = nobo_c[nobo_c$Date >= "2023-12-01" & nobo_c$Date <= "2024-02-28", ]
winter = rbind(winter22, winter23)


##### Models: SEASON  #### 

spring_mod = glmer(response ~ scale(DTN_road) + scale(perc_grassy) + scale(perc_bf) + scale(ndvi) +(1|Bird.ID), family = binomial, data = spring)
summer_mod = glmer(response ~ scale(DTN_road) + scale(perc_grassy) + scale(perc_bf) + scale(ndvi) +(1|Bird.ID), family = binomial, data = summer)
fall_mod = glmer(response ~ scale(DTN_road) + scale(perc_grassy) + scale(perc_bf) + scale(ndvi) +(1|Bird.ID), family = binomial, data = fall)
winter_mod = glmer(response ~ scale(DTN_road) + scale(perc_grassy) + scale(perc_bf) + scale(ndvi) +(1|Bird.ID), family = binomial, data = winter)

Fig3 = dwplot(list(spring_mod, summer_mod, fall_mod, winter_mod))


###################################################################################
###################################################################################
###################################################################################


##### 2 month intervals ####

# January - February 
jf_22 = nobo_c[nobo_c$Date >= "2022-01-01" & nobo_c$Date <= "2022-02-28", ]
jf_23 = nobo_c[nobo_c$Date >= "2023-01-01" & nobo_c$Date <= "2023-02-31", ]
jf = rbind(jf_22, jf_23)
nrow(jf) # 11445

# March - April 
ma_22 = nobo_c[nobo_c$Date >= "2022-03-01" & nobo_c$Date <= "2022-04-31", ]
ma_23 = nobo_c[nobo_c$Date >= "2023-03-01" & nobo_c$Date <= "2023-04-31", ]
ma = rbind(ma_22, ma_23)
nrow(ma) # 7776

# May - June 
mj_22 = nobo_c[nobo_c$Date >= "2022-05-01" & nobo_c$Date <= "2022-06-31", ]
mj_23 = nobo_c[nobo_c$Date >= "2023-05-01" & nobo_c$Date <= "2023-06-31", ]
mj = rbind(mj_22, mj_23)
nrow(mj) # 7776

# July - August 
ja_22 = nobo_c[nobo_c$Date >= "2022-07-01" & nobo_c$Date <= "2022-08-31", ]
ja_23 = nobo_c[nobo_c$Date >= "2023-07-01" & nobo_c$Date <= "2023-08-31", ]
ja = rbind(ja_22, ja_23)
nrow(ja) # 21612

# September - October 
so_22 = nobo_c[nobo_c$Date >= "2022-09-01" & nobo_c$Date <= "2022-10-31", ]
so_23 = nobo_c[nobo_c$Date >= "2023-09-01" & nobo_c$Date <= "2023-10-31", ]
so = rbind(so_22, so_23)
nrow(so) # 8670

# November - December 
nd_22 = nobo_c[nobo_c$Date >= "2022-11-01" & nobo_c$Date <= "2022-12-31", ]
nd_23 = nobo_c[nobo_c$Date >= "2023-11-01" & nobo_c$Date <= "2023-12-31", ]
nd = rbind(nd_22, nd_23)
nrow(nd) # 21612

##### Models: 2 month intervals  #### 

jf_mod = glmer(response ~ scale(DTN_road) + scale(perc_grassy) + scale(perc_bf) + scale(ndvi) +(1|Bird.ID), family = binomial, data = jf)
ma_mod = glmer(response ~ scale(DTN_road) + scale(perc_grassy) + scale(perc_bf) + scale(ndvi) +(1|Bird.ID), family = binomial, data = ma)
mj_mod = glmer(response ~ scale(DTN_road) + scale(perc_grassy) + scale(perc_bf) + scale(ndvi) +(1|Bird.ID), family = binomial, data = mj)
ja_mod = glmer(response ~ scale(DTN_road) + scale(perc_grassy) + scale(perc_bf) + scale(ndvi) +(1|Bird.ID), family = binomial, data = ja)
so_mod = glmer(response ~ scale(DTN_road) + scale(perc_grassy) + scale(perc_bf) + scale(ndvi) +(1|Bird.ID), family = binomial, data = so)
nd_mod = glmer(response ~ scale(DTN_road) + scale(perc_grassy) + scale(perc_bf) + scale(ndvi) +(1|Bird.ID), family = binomial, data = nd)

Fig4 = dwplot(list(jf_mod, ma_mod, mj_mod, ja_mod, so_mod, nd_mod))


