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
# 4. 2 month intervals: jan-feb, mar-apr, etc... 

# This will be using the Course Scaled data 
# This can easily be changed, but it seemed that the home range scale (MCP) had the 
# least magnitude of affect on NOBO resource selection. On a management scale that 
# has different courses/sites that they manage for and each may need their own management regime 
# to achieve the desired results due to heterogeneity of the landscape (e.g. soil quality, vegetation 
# cover, proximity to urban neighborhoods). This is mainly referencing below the creek and above the creek 
# Maybe this is not a sound thought process -- we should discuss 

##############################################################################

nobo_c = read.csv("./ResSelData_Course.csv") # read in the course level data 
nobo_c[,11:22] <- scale(nobo_c[,11:22]) # scale all the variables

##### Breeding vs nonbreeding -- COURSE ####

#     breeding data subset
breeding22 <- nobo_c[nobo_c$Date >= "2022-04-01" & nobo_c$Date <= "2022-09-30", ]
breeding23 <- nobo_c[nobo_c$Date >= "2023-04-01" & nobo_c$Date <= "2023-09-30", ]
breeding = rbind(breeding22, breeding23)
nrow(breeding) # 54735

# nonbreeding data subset
non_breeding1 = nobo_c[nobo_c$Date >= "2022-01-01" & nobo_c$Date <= "2022-03-31", ]
non_breeding2 = nobo_c[nobo_c$Date >= "2022-10-01" & nobo_c$Date <= "2023-03-31", ]
non_breeding3 = nobo_c[nobo_c$Date >= "2023-10-01" & nobo_c$Date <= "2023-12-31", ]
nonbreeding = rbind(non_breeding1, non_breeding2, non_breeding3)
nrow(nonbreeding) # 17826

##### Models: breeding vs nonbreeding  #### 
library(dotwhisker); library(dplyr); library(lme4)

nonbreeding_mod = glmer(response ~ DTN_road + perc_grassy + perc_bf + ndvi +(1|Bird.ID), family = binomial, data = nonbreeding)
breeding_mod = glmer(response ~ DTN_road + perc_grassy + perc_bf + ndvi +(1|Bird.ID), family = binomial, data = breeding)
summary(nonbreeding_mod)

# A basic dot whisker plot without editing anything 
Fig1 = dwplot(list(breeding_mod, nonbreeding_mod))

# lets make it pretty for the "elderly eyes" (circa DJ 01/18/2024)
bi_annual_mods = list(breeding_mod, nonbreeding_mod)

# dot whisker plot with editing 
Fig1_biannual = dwplot(bi_annual_mods,
                      ci = 0.95, 
                      dodge_size = 0.4, # how far apart pts are frome eachother (0.4 = default) 
                      show_intercept = FALSE, 
                      model_order = NULL, 
                      dot_args = list(size = 3), 
                      vline = geom_vline(xintercept = 0, linetype = 2, colour ="grey8"), 
                      vars_order = c("DTN_road", "perc_grassy", "perc_bf", "ndvi")) +
  theme_bw() + xlab("Coefficient Estimate") + ylab("") + 
  scale_color_discrete(name = "Model", labels = c("NB", "Br"))

Fig1_biannual

# Facet dot whisker plot 
Fig1_biannual_facet = dwplot(bi_annual_mods, 
               ci = 0.95, 
               dodge_size = 0.4, # how far apart pts are frome eachother (0.4 = default) 
               show_intercept = FALSE, 
               model_order = NULL, 
               dot_args = list(size = 3), 
               vline = geom_vline(xintercept = 0, linetype = 2, colour ="grey8"), 
               vars_order = c("DTN_road", "perc_grassy", "perc_bf", "ndvi"),
) +
  facet_grid(~model, scales="free_y") +
  theme_bw() + xlab("Coefficient Estimate") + ylab("") 
Fig1_biannual_facet

###################################################################################
###################################################################################
###################################################################################

#### Seasons: Spring, Summer, Fall, Winter 

# Already ran this in but just in case 
nobo_c = read.csv("./ResSelData_Course.csv") # read in the course level data 
nobo_c[,11:22] <- scale(nobo_c[,11:22]) # scale all the variables

##### course ->Breeding vs nonbreeding ####

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

Fig_season = dwplot(list(spring_mod, summer_mod, fall_mod, winter_mod),
ci = 0.95, 
dodge_size = 0.4, # how far apart pts are frome eachother (0.4 = default) 
show_intercept = FALSE, 
model_order = NULL, 
dot_args = list(size = 3), 
vline = geom_vline(xintercept = 0, linetype = 2, colour ="grey8"), 
vars_order = c("scale(DTN_road)", "scale(perc_grassy)", "scale(perc_bf)", "scale(ndvi)")) +
  theme_bw() + xlab("Coefficient Estimate") + ylab("")
Fig_season # pulls up the fig

Fig_season_facet = dwplot(list(spring_mod, summer_mod, fall_mod, winter_mod), 
              ci = 0.95, 
              dodge_size = 0.4, # how far apart pts are frome eachother (0.4 = default) 
              show_intercept = FALSE, 
              model_order = NULL, 
              dot_args = list(size = 1.2), 
              vline = geom_vline(xintercept = 0, linetype = 2, colour ="grey8"), 
              vars_order = c("scale(DTN_road)", "scale(perc_grassy)", "scale(perc_bf)", "scale(ndvi)"),
) +
  facet_grid(~model, scales="free_y") +
  theme_bw() + xlab("Coefficient Estimate") + ylab("") 
Fig_season_facet # pulls up the fig

###################################################################################
###################################################################################
###################################################################################


##### Course -> 2 month intervals ####

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

month2_interval = dwplot(list(jf_mod, ma_mod, mj_mod, ja_mod, so_mod, nd_mod),
              ci = 0.95, 
              dodge_size = 0.4, # how far apart pts are frome eachother (0.4 = default) 
              show_intercept = FALSE, 
              model_order = NULL, 
              dot_args = list(size = 3), 
              vline = geom_vline(xintercept = 0, linetype = 2, colour ="grey8"), 
              vars_order = c("scale(DTN_road)", "scale(perc_grassy)", "scale(perc_bf)", "scale(ndvi)")) +
  theme_bw() + xlab("Coefficient Estimate") + ylab("")

month2_interval

month2_interval_facet = dwplot(list(jf_mod, ma_mod, mj_mod, ja_mod, so_mod, nd_mod), 
  ci = 0.95, 
  dodge_size = 0.4, # how far apart pts are frome eachother (0.4 = default) 
  show_intercept = FALSE, 
  model_order = NULL, 
  dot_args = list(size = 3), 
  vline = geom_vline(xintercept = 0, linetype = 2, colour ="grey8"), 
  vars_order = c("scale(DTN_road)", "scale(perc_grassy)", "scale(perc_bf)", "scale(ndvi)"),
  ) +
  facet_grid(~model, scales="free_y") +
  theme_bw() + xlab("Coefficient Estimate") + ylab("")





###############################################################################
################################################################################
# SCALED TO PROPERTY ----
# exploring fig when the scale is expanding to property 
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

##### MODELS breeding vs nonbreeding -PROPERTY  #### 
library(dotwhisker)
library(dplyr)

nonbreeding_mod_p = glmer(response ~ scale(DTN_road) + scale(perc_grassy) + scale(perc_bf) + scale(ndvi) +(1|Bird.ID), family = binomial, data = nonbreeding_p)

breeding_mod_p = glmer(response ~ scale(DTN_road) + scale(perc_grassy) + scale(perc_bf) + scale(ndvi) +(1|Bird.ID), family = binomial, data = breeding_p)

# COMPARES AT A PROPERTY SCALE 
Fig2 = dwplot(list(breeding_mod_p, nonbreeding_mod_p))

# does not seem to be too much of a difference when it comes to the outcome of the data 
# It seems maybe the confidence intervals get a little larger at a home range scale 
# but they do not have an influence on use of covertypes/landscape features 







##### HERE IS WHAT I WANT TO SHOW YOU 


# an attempt -- scaling predictors before putting them into a model 
# nonbreeding_mod = glmer(response ~ scale(DTN_road) + scale(perc_grassy) + scale(perc_bf) + scale(ndvi) +(1|Bird.ID), family = binomial, data = nonbreeding)

nobo_c = read.csv("./ResSelData_Course.csv") # read in the course level data 

##### Breeding vs nonbreeding -- COURSE ####

#     breeding data subset
breeding22 <- nobo_c[nobo_c$Date >= "2022-04-01" & nobo_c$Date <= "2022-09-30", ]
breeding23 <- nobo_c[nobo_c$Date >= "2023-04-01" & nobo_c$Date <= "2023-09-30", ]
breeding = rbind(breeding22, breeding23)
nrow(breeding) # 54735

# nonbreeding data subset
non_breeding1 = nobo_c[nobo_c$Date >= "2022-01-01" & nobo_c$Date <= "2022-03-31", ]
non_breeding2 = nobo_c[nobo_c$Date >= "2022-10-01" & nobo_c$Date <= "2023-03-31", ]
non_breeding3 = nobo_c[nobo_c$Date >= "2023-10-01" & nobo_c$Date <= "2023-12-31", ]
nonbreeding = rbind(non_breeding1, non_breeding2, non_breeding3)
nrow(nonbreeding) # 17826

##### Models: breeding vs nonbreeding  #### 
library(dotwhisker)
library(dplyr)
# attempting to scale first then put in the model to be able to adjust var names using dwplot() 
road_sc_nb = scale(nonbreeding$DTN_road)
grassy_sc_nb = scale(nonbreeding$perc_grassy)
bf_sc_nb = scale(nonbreeding$perc_bf)
ndvi_sc_nb = scale(nonbreeding$ndvi)

nonbreeding_mod_sc = glmer(response ~ road_sc + grassy_sc + bf_sc + ndvi_sc +(1|Bird.ID), family = binomial, data = nonbreeding)
sc_test = dwplot(nonbreeding_mod_sc)
summary(nonbreeding_mod_sc)

sc_test_fig= dwplot(nonbreeding_mod_sc,
                    ci = 0.95, 
                    dodge_size = 0.4, # how far apart pts are frome eachother (0.4 = default) 
                    show_intercept = FALSE, 
                    model_order = NULL, 
                    dot_args = list(size = 1.2), 
                    vline = geom_vline(xintercept = 0, linetype = 2, colour ="grey8"), 
                    vars_order = c("road_sc", "grassy_sc", "bf_sc", "ndvi_sc"),
) %>%
  relabel_predictors(c(road_sc = "Distance to Nearest Road", grassy_sc = "Percent Grassy Cover", 
                       bf_sc = "Percent Broodfield", ndvi_sc = "NDVI")) +
  theme_bw() + xlab("Coefficient Estimate") + ylab("")



#### 
# the real one 
nonbreeding_mod = glmer(response ~ scale(DTN_road) + scale(perc_grassy) + scale(perc_bf) + scale(ndvi) +(1|Bird.ID), family = binomial, data = nonbreeding)

# this is an attempt to show that 'scale(x)' doesnt seem to work with the code for a dwplot 
nonbreeding_mod_test = dwplot(nonbreeding_mod,
                              ci = 0.95, 
                              dodge_size = 0.4, # how far apart pts are frome eachother (0.4 = default) 
                              show_intercept = FALSE, 
                              model_order = NULL, 
                              dot_args = list(size = 1.2), 
                              vline = geom_vline(xintercept = 0, linetype = 2, colour ="grey8"), 
                              vars_order = c("scale(DTN_road)", "scale(perc_grassy)", "scale(perc_bf)", "scale(ndvi)"), 
) %>%
  relabel_predictors(c(scale(DTN_road) = "Distance to Nearest Road", scale(perc_grassy) = "Percent Grassy Cover", 
                       scale(perc_bf) = "Percent Broodfield", scale(ndvi) = "NDVI")) +
  theme_bw() + xlab("Coefficient Estimate") + ylab("")


# this seemed to work for one variable... guess I will have to do it for both?



