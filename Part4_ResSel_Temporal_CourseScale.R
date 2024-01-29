# Title: Resource Selection Temporal Scale  ----
# Author: Autumn Randall 
# PI: Dr. DJ. McNeil 
# Date created: 1/17/2024
# Date Last Editted: 1/17/2024
###################################

# Goal:
# We have already run the full year with all the data. 
# I want to split the data up based on 5 temporal scales: 
# 1. Annual
# 2. Bi-Annual: Breeding vs Nonbreeding 
# 3. Seasonal: Spring, Summer, Fall, Winter
# 4. 2 month intervals: jan-feb, mar-apr,
# 5. 1 month interval  

# This will be using the Course Scaled data 
# This can easily be changed, but it seemed that the home range scale (MCP) had the 
# least magnitude of affect on NOBO resource selection. On a management scale that 
# has different courses/sites that they manage for and each may need their own management regime 
# to achieve the desired results due to heterogeneity of the landscape (e.g. soil quality, vegetation 
# cover, proximity to urban neighborhoods). This is mainly referencing below the creek and above the creek 
# Maybe this is not a sound thought process -- we should discuss 

##############################################################################
# COURSE ----
nobo_c = read.csv("./ResSelData_Course.csv") # read in the course level data 
nobo_c[,11:22] <- scale(nobo_c[,11:22]) # scale all the variables
length(unique(nobo_c$Bird.ID)) #588


## models ----
Course_mod = glmer(response ~ DTN_road + perc_grassy + perc_bf + ndvi +(1|Bird.ID), family = binomial, data = nobo_c)
summary(Course_mod)

# ANNUAL ----
nobo_c = read.csv("./ResSelData_Course.csv") # read in the course level data 
nobo_c[,11:22] <- scale(nobo_c[,11:22]) # scale all the variables


#by year data subset
annual22 <- nobo_c[nobo_c$Date >= "2022-01-01" & nobo_c$Date <= "2022-12-31", ]
annual23 <- nobo_c[nobo_c$Date >= "2023-01-01" & nobo_c$Date <= "2023-12-31", ]
nrow(annual22) # 35664
nrow(annual23) # 36897

## Models ----
library(dotwhisker); library(dplyr); library(lme4)

annual22_mod = glmer(response ~ DTN_road + perc_grassy + perc_bf + ndvi +(1|Bird.ID), family = binomial, data = annual22)
annual23_mod = glmer(response ~ DTN_road + perc_grassy + perc_bf + ndvi +(1|Bird.ID), family = binomial, data = annual23)
summary(annual22_mod)
summary(annual23_mod)
# (st error * 1.96) + or - to estimate 


## Figure ----
year_mods = list(annual22_mod, annual23_mod)
# Annual using the "course" scale 
Fig6_ResSel_Annual = dwplot(year_mods,
                                ci = 0.95, 
                                dodge_size = 0.4, # how far apart pts are frome eachother (0.4 = default) 
                                show_intercept = FALSE, 
                                model_order = NULL, 
                                dot_args = list(size = 3), 
                                vline = geom_vline(xintercept = 0, linetype = 2, colour ="grey8"), 
                                vars_order = c("DTN_road", "perc_grassy", "perc_bf", "ndvi")) %>% # plot line at zero _behind_coefs
  relabel_predictors(
    c(
      DTN_road = "Distance to Nearest Road",
      perc_grassy = "Percent Grassy",
      perc_bf = "Percent Broodfield",
      ndvi = "NDVI")
  ) +
  
  theme_bw() + xlab("Coefficient Estimate") + ylab("") + # Assigns a name to thex and y axis
  scale_color_discrete(name = "Annual Course Model", labels = c("Year 2022", "Year 2023"), type = c('slateblue2', 'slateblue4'))# labels the legend then the models, then assigns colors 


 Fig6_ResSel_Annual + xlim(c(-1,1)) + coord_flip()
 #adjusts the x axis 

# B VS NB ####

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

## Models ---- 
library(dotwhisker); library(dplyr); library(lme4)

nonbreeding_mod = glmer(response ~ DTN_road + perc_grassy + perc_bf + ndvi +(1|Bird.ID), family = binomial, data = nonbreeding)
breeding_mod = glmer(response ~ DTN_road + perc_grassy + perc_bf + ndvi +(1|Bird.ID), family = binomial, data = breeding)
summary(breeding_mod)
summary(nonbreeding_mod)


## Figure ----

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
                      vars_order = c("DTN_road", "perc_grassy", "perc_bf", "ndvi")) %>% # plot line at zero _behind_coefs
  relabel_predictors(
    c(
      DTN_road = "Distance to Nearest Road",
      perc_grassy = "Percent Grassy",
      perc_bf = "Percent Broodfield",
      ndvi = "NDVI")
  ) +
  
  theme_bw() + xlab("Coefficient Estimate") + ylab("") + 
  scale_color_discrete(name = "Bi-Annual Course Model", labels = c("Breeding", "Non-breeding"), type = c('slateblue2', 'slateblue4'))# labels the legend then the models, then assigns colors 
Fig1_biannual + xlim(c(-1,1)) + coord_flip()



###################################################################################
###################################################################################
###################################################################################

#SEASON ---- 

# Already ran this in but just in case 
nobo_c = read.csv("./ResSelData_Course.csv") # read in the course level data 
nobo_c[,11:22] <- scale(nobo_c[,11:22]) # scale all the variables

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


## Models  #### 

spring_mod = glmer(response ~ DTN_road + perc_grassy + perc_bf + ndvi +(1|Bird.ID), family = binomial, data = spring)
summer_mod = glmer(response ~ DTN_road + perc_grassy + perc_bf + ndvi +(1|Bird.ID), family = binomial, data = summer)
fall_mod = glmer(response ~ DTN_road + perc_grassy + perc_bf + ndvi +(1|Bird.ID), family = binomial, data = fall)
winter_mod = glmer(response ~ DTN_road + perc_grassy + perc_bf + ndvi +(1|Bird.ID), family = binomial, data = winter)

summary(spring_mod)
summary(summer_mod)
summary(fall_mod)
summary(winter_mod)

## Figure ----
season_mod = list(spring_mod, summer_mod, fall_mod, winter_mod)

# dot whisker plot with editing 

dwplot(list(spring_mod, summer_mod, fall_mod, winter_mod))


Fig2_seasonal = dwplot(list(spring_mod, summer_mod, fall_mod, winter_mod),
                       ci = 0.95, 
                       dodge_size = 0.4, # how far apart pts are frome eachother (0.4 = default) 
                       show_intercept = FALSE, 
                       model_order = NULL, 
                       dot_args = list(size = 3), 
                       vline = geom_vline(xintercept = 0, linetype = 2, colour ="grey8"), 
                       vars_order = c("DTN_road", "perc_grassy", "perc_bf", "ndvi")) %>% # plot line at zero _behind_coefs
  relabel_predictors(
    c(
      DTN_road = "Distance to Nearest Road",
      perc_grassy = "Percent Grassy",
      perc_bf = "Percent Broodfield",
      ndvi = "NDVI")
  ) +
  
  theme_bw() + xlab("Coefficient Estimate") + ylab("") + 
  scale_color_discrete(name = "Seasonal Course Model", labels = c("Spring", "Summer", "Fall", "Winter"), type = c('thistle2', 'plum3', 'slateblue2', 'slateblue4'))# labels the legend then the models, then assigns colors 


Fig2_seasonal + xlim(c(-1,1)) + coord_flip()


###################################################################################
###################################################################################
###################################################################################
# 2 MONTH  ----

# Already ran this in but just in case 
nobo_c = read.csv("./ResSelData_Course.csv") # read in the course level data 
nobo_c[,11:22] <- scale(nobo_c[,11:22]) # scale all the variables

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

## Models  #### 

jf_mod = glmer(response ~ DTN_road + perc_grassy + perc_bf + ndvi +(1|Bird.ID), family = binomial, data = jf)
ma_mod = glmer(response ~ DTN_road + perc_grassy + perc_bf + ndvi +(1|Bird.ID), family = binomial, data = ma)
mj_mod = glmer(response ~ DTN_road + perc_grassy + perc_bf + ndvi +(1|Bird.ID), family = binomial, data = mj)
ja_mod = glmer(response ~ DTN_road + perc_grassy + perc_bf + ndvi +(1|Bird.ID), family = binomial, data = ja)
so_mod = glmer(response ~ DTN_road + perc_grassy + perc_bf + ndvi +(1|Bird.ID), family = binomial, data = so)
nd_mod = glmer(response ~ DTN_road + perc_grassy + perc_bf + ndvi +(1|Bird.ID), family = binomial, data = nd)

summary(jf_mod)
summary(ma_mod)
summary(mj_mod)
summary(ja_mod)
summary(so_mod)
summary(nd_mod)

## Figure ----
month_2interv_mods = list(jf_mod, ma_mod, mj_mod, ja_mod, so_mod, nd_mod)

Fig3_month_2interv = dwplot(month_2interv_mods,
                       ci = 0.95, 
                       dodge_size = 0.4, # how far apart pts are frome eachother (0.4 = default) 
                       show_intercept = FALSE, 
                       model_order = NULL, 
                       dot_args = list(size = 3), 
                       vline = geom_vline(xintercept = 0, linetype = 2, colour ="grey8"), 
                       vars_order = c("DTN_road", "perc_grassy", "perc_bf", "ndvi")) %>% # plot line at zero _behind_coefs
  relabel_predictors(
    c(
      DTN_road = "Distance to Nearest Road",
      perc_grassy = "Percent Grassy",
      perc_bf = "Percent Broodfield",
      ndvi = "NDVI")
  ) +
  
  theme_bw() + xlab("Coefficient Estimate") + ylab("") + 
  scale_color_discrete(name = "2-Month Course Model", labels = c("Jan-Feb", "Mar-Apr", "May-Jun", "Jul-Aug", 
                                                  "Sep-Oct", "Nov-Dec"), type = c('thistle2', 'plum3', 'plum4', 'slateblue', 'slateblue2', 'slateblue4'))# labels the legend then the models, then assigns colors 


Fig3_month_2interv + xlim(c(-1,1)) + coord_flip()
 


# MONTH  ####
nobo_c = read.csv("./ResSelData_Course.csv") # read in the course level data 
nobo_c[,11:22] <- scale(nobo_c[,11:22]) # scale all the variables


# January  
jan_22 = nobo_c[nobo_c$Date >= "2022-01-01" & nobo_c$Date <= "2022-01-31", ]
jan_23 = nobo_c[nobo_c$Date >= "2023-01-01" & nobo_c$Date <= "2023-01-31", ]
jan = rbind(jan_22, jan_23)
nrow(jan) # 6177


# February   
feb_22 = nobo_c[nobo_c$Date >= "2022-02-01" & nobo_c$Date <= "2022-02-31", ]
feb_23 = nobo_c[nobo_c$Date >= "2023-02-01" & nobo_c$Date <= "2023-02-31", ]
feb = rbind(feb_22, feb_23)
nrow(feb) # 5268

# March   
mar_22 = nobo_c[nobo_c$Date >= "2022-03-01" & nobo_c$Date <= "2022-03-31", ]
mar_23 = nobo_c[nobo_c$Date >= "2023-03-01" & nobo_c$Date <= "2023-03-31", ]
mar = rbind(mar_22, mar_23)
nrow(mar) # 2283

# April  
apr_22 = nobo_c[nobo_c$Date >= "2022-04-01" & nobo_c$Date <= "2022-04-31", ]
apr_23 = nobo_c[nobo_c$Date >= "2023-04-01" & nobo_c$Date <= "2023-04-31", ]
apr = rbind(apr_22, apr_23)
nrow(apr) # 5493

# May  
may_22 = nobo_c[nobo_c$Date >= "2022-05-01" & nobo_c$Date <= "2022-05-31", ]
may_23 = nobo_c[nobo_c$Date >= "2023-05-01" & nobo_c$Date <= "2023-05-31", ]
may = rbind(may_22, may_23)
nrow(may) # 7230

# June  
jun_22 = nobo_c[nobo_c$Date >= "2022-06-01" & nobo_c$Date <= "2022-06-31", ]
jun_23 = nobo_c[nobo_c$Date >= "2023-06-01" & nobo_c$Date <= "2023-06-31", ]
jun = rbind(jun_22, jun_23)
nrow(jun) # 14634

# July  
jul_22 = nobo_c[nobo_c$Date >= "2022-07-01" & nobo_c$Date <= "2022-07-31", ]
jul_23 = nobo_c[nobo_c$Date >= "2023-07-01" & nobo_c$Date <= "2023-07-31", ]
jul = rbind(jul_22, jul_23)
nrow(jul) # 12666

# August  
aug_22 = nobo_c[nobo_c$Date >= "2022-08-01" & nobo_c$Date <= "2022-08-31", ]
aug_23 = nobo_c[nobo_c$Date >= "2023-08-01" & nobo_c$Date <= "2023-08-31", ]
aug = rbind(aug_22, aug_23)
nrow(aug) # 8946

# September   
sept_22 = nobo_c[nobo_c$Date >= "2022-09-01" & nobo_c$Date <= "2022-09-31", ]
sept_23 = nobo_c[nobo_c$Date >= "2023-09-01" & nobo_c$Date <= "2023-09-31", ]
sept = rbind(sept_22, sept_23)
nrow(sept) # 5766

# October  
oct_22 = nobo_c[nobo_c$Date >= "2022-10-01" & nobo_c$Date <= "2022-10-31", ]
oct_23 = nobo_c[nobo_c$Date >= "2023-10-01" & nobo_c$Date <= "2023-10-31", ]
oct = rbind(oct_22, oct_23)
nrow(oct) # 2904

# November   
nov_22 = nobo_c[nobo_c$Date >= "2022-11-01" & nobo_c$Date <= "2022-11-31", ]
nov_23 = nobo_c[nobo_c$Date >= "2023-11-01" & nobo_c$Date <= "2023-11-31", ]
nov = rbind(nov_22, nov_23)
nrow(nov) # 516

# December   
dec_22 = nobo_c[nobo_c$Date >= "2022-12-01" & nobo_c$Date <= "2022-12-31", ]
dec_23 = nobo_c[nobo_c$Date >= "2023-12-01" & nobo_c$Date <= "2023-12-31", ]
dec = rbind(dec_22, dec_23)
nrow(dec) # 678

#### Models  ----
jan_mod = glmer(response ~ DTN_road + perc_grassy + perc_bf + ndvi +(1|Bird.ID), family = binomial, data = jan)
feb_mod = glmer(response ~ DTN_road + perc_grassy + perc_bf + ndvi +(1|Bird.ID), family = binomial, data = feb)
mar_mod = glmer(response ~ DTN_road + perc_grassy + perc_bf + ndvi +(1|Bird.ID), family = binomial, data = mar)
apr_mod = glmer(response ~ DTN_road + perc_grassy + perc_bf + ndvi +(1|Bird.ID), family = binomial, data = apr)
may_mod = glmer(response ~ DTN_road + perc_grassy + perc_bf + ndvi +(1|Bird.ID), family = binomial, data = may)
jun_mod = glmer(response ~ DTN_road + perc_grassy + perc_bf + ndvi +(1|Bird.ID), family = binomial, data = jun)
jul_mod = glmer(response ~ DTN_road + perc_grassy + perc_bf + ndvi +(1|Bird.ID), family = binomial, data = jul)
aug_mod = glmer(response ~ DTN_road + perc_grassy + perc_bf + ndvi +(1|Bird.ID), family = binomial, data = aug)
sept_mod = glmer(response ~ DTN_road + perc_grassy + perc_bf + ndvi +(1|Bird.ID), family = binomial, data = sept)
oct_mod = glmer(response ~ DTN_road + perc_grassy + perc_bf + ndvi +(1|Bird.ID), family = binomial, data = oct)
nov_mod = glmer(response ~ DTN_road + perc_grassy + perc_bf + ndvi +(1|Bird.ID), family = binomial, data = nov)
dec_mod = glmer(response ~ DTN_road + perc_grassy + perc_bf + ndvi +(1|Bird.ID), family = binomial, data = dec)

summary(jan_mod)
summary(feb_mod)
summary(mar_mod)
summary(apr_mod)
summary(may_mod)
summary(jun_mod)
summary(jul_mod)
summary(aug_mod)
summary(sept_mod)
summary(oct_mod)
summary(nov_mod)
summary(dec_mod)

#### Figure ----
# make a list to hold the mods 
month_mod = list(jan_mod, feb_mod, mar_mod, apr_mod, may_mod, jun_mod, jul_mod, aug_mod, sept_mod, oct_mod, nov_mod, dec_mod)

Fig4_month = dwplot(month_mod,
                            ci = 0.95, 
                            dodge_size = 0.4, # how far apart pts are frome eachother (0.4 = default) 
                            show_intercept = FALSE, 
                            model_order = NULL, 
                            dot_args = list(size = 3), 
                            vline = geom_vline(xintercept = 0, linetype = 2, colour ="grey8"), 
                            vars_order = c("DTN_road", "perc_grassy", "perc_bf", "ndvi")) %>% # plot line at zero _behind_coefs
  relabel_predictors(
    c(
      DTN_road = "Distance to Nearest Road",
      perc_grassy = "Percent Grassy",
      perc_bf = "Percent Broodfield",
      ndvi = "NDVI")
  ) +
  
  theme_bw() + xlab("Coefficient Estimate") + ylab("") + 
  scale_color_discrete(name = "Monthly Course Model", labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", 
                                                  "Sep", "Oct", "Nov", "Dec"), 
                       type = c('thistle','thistle2', 'pink', 'pink3', 'plum','plum3',
                                         'mediumorchid', 'mediumorchid4',
                                         'slateblue', 'slateblue2', 'slateblue4',
                                         'pink4'))# labels the legend then the models, then assigns colors 


Fig4_month + xlim(c(-1,1)) + coord_flip()






























