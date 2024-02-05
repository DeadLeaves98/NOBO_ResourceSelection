


##############################################################################
#                           spring ----
##############################################################################
# BrierScore for the yearly temporal models 
# 2022 and 2023 
##########################################

library(dismo); library(lme4)

# Brier Scores, "all" combined dataset
###############

spring_mod = readRDS("./for_brier/spring_Course_mod.rds") # model for nonbreeding
spring = read.csv("./for_brier/spring_ResSelData_Course.csv") # Data for nonbreeding

#################################################################### Iterative AUC and Briers Score!

spring # spring dataset for processing 
spring = spring[-c(1)]
# spring 
spring$sub1 = rbinom(n = nrow(spring), size = 1, prob = 0.75) # create a "subset 1" column; 75% prob
nrow(subset(spring, sub1 == 1))/nrow(spring) # did it work? Yes

# spring: now let's make em for the rest of the 10 subsets
spring$sub2 = rbinom(n = nrow(spring), size = 1, prob = 0.75)
spring$sub3 = rbinom(n = nrow(spring), size = 1, prob = 0.75)
spring$sub4 = rbinom(n = nrow(spring), size = 1, prob = 0.75)
spring$sub5 = rbinom(n = nrow(spring), size = 1, prob = 0.75)
spring$sub6 = rbinom(n = nrow(spring), size = 1, prob = 0.75)
spring$sub7 = rbinom(n = nrow(spring), size = 1, prob = 0.75)
spring$sub8 = rbinom(n = nrow(spring), size = 1, prob = 0.75)
spring$sub9 = rbinom(n = nrow(spring), size = 1, prob = 0.75)
spring$sub10 = rbinom(n = nrow(spring), size = 1, prob = 0.75)

# make blank dataset to hold results
AUCTable_spring <- data.frame("DataSubset" = 0, "AUC" = 0, "BrierScore" = 0)

# in the for() loop 
# keep getting error that says "Error in levelfun(r, n, allow.new.levels = allow.new.levels) 


for(i in 1:10){
  #i = 1
  datasubset <- subset(spring, spring[,22+i] == 1) # first subset is col 24; get 1's
  
  # fit model with subset i
  
  topmod <- glmer(response ~ DTN_road + perc_grassy + perc_bf + ndvi + (1 | Bird.ID),
                  family = binomial, data = datasubset)
  
  # predict and arrange data for AUC
  testsubset <- subset(spring, spring[,22+i] == 0)# first subset is col 24; get 0's
  pred_1 <- predict(topmod, type = "response", newdata = testsubset, allow.new.levels = TRUE)
  testsubset$predicted <- pred_1
  DataforAUC <- data.frame("naive" = testsubset$response, "predicted" = testsubset$predicted)
  head(DataforAUC)
  
  # calculate AUC
  p <- subset(DataforAUC, naive == 1)
  p <- p$predicted
  a <- subset(DataforAUC, naive == 0)
  a <- a$predicted
  e <- dismo::evaluate(p, a)
  aucvalue = e@auc
  
  # brier score
  brierval <- sum((DataforAUC$predicted - DataforAUC$naive)^2) / nrow(DataforAUC)
  
  # add this data to the table
  newrow <- c(i, round(aucvalue,2), round(brierval,2))
  AUCTable_spring <- rbind(AUCTable_spring, newrow)
}
AUCTable_spring <- AUCTable_spring[2:11,]
AUCTable_spring
Score_spring = mean(AUCTable_spring$BrierScore)
Score_spring



##############################################################################
#                           summer  ----
##############################################################################


library(dismo); library(lme4)

# Brier Scores, "all" combined dataset
###############

summer_mod = readRDS("./for_brier/summer_Course_mod.rds") # model for nonbreeding
summer = read.csv("./for_brier/summer_ResSelData_Course.csv") # Data for nonbreeding

#################################################################### Iterative AUC and Briers Score!
summer = summer[-c(1)]
# summer 
summer$sub1 = rbinom(n = nrow(summer), size = 1, prob = 0.75) # create a "subset 1" column; 75% prob
nrow(subset(summer, sub1 == 1))/nrow(summer) # did it work? Yes

# summer: now let's make em for the rest of the 10 subsets
summer$sub2 = rbinom(n = nrow(summer), size = 1, prob = 0.75)
summer$sub3 = rbinom(n = nrow(summer), size = 1, prob = 0.75)
summer$sub4 = rbinom(n = nrow(summer), size = 1, prob = 0.75)
summer$sub5 = rbinom(n = nrow(summer), size = 1, prob = 0.75)
summer$sub6 = rbinom(n = nrow(summer), size = 1, prob = 0.75)
summer$sub7 = rbinom(n = nrow(summer), size = 1, prob = 0.75)
summer$sub8 = rbinom(n = nrow(summer), size = 1, prob = 0.75)
summer$sub9 = rbinom(n = nrow(summer), size = 1, prob = 0.75)
summer$sub10 = rbinom(n = nrow(summer), size = 1, prob = 0.75)

# make blank dataset to hold results
AUCTable_summer <- data.frame("DataSubset" = 0, "AUC" = 0, "BrierScore" = 0)

# in the for() loop 
# keep getting error that says "Error in levelfun(r, n, allow.new.levels = allow.new.levels) 


for(i in 1:10){
  #i = 1
  datasubset <- subset(summer, summer[,22+i] == 1) # first subset is col 24; get 1's
  
  # fit model with subset i
  
  topmod <- glmer(response ~ DTN_road + perc_grassy + perc_bf + ndvi + (1 | Bird.ID),
                  family = binomial, data = datasubset)
  
  # predict and arrange data for AUC
  testsubset <- subset(summer, summer[,22+i] == 0)# first subset is col 24; get 0's
  pred_1 <- predict(topmod, type = "response", newdata = testsubset, allow.new.levels = TRUE)
  testsubset$predicted <- pred_1
  DataforAUC <- data.frame("naive" = testsubset$response, "predicted" = testsubset$predicted)
  head(DataforAUC)
  
  # calculate AUC
  p <- subset(DataforAUC, naive == 1)
  p <- p$predicted
  a <- subset(DataforAUC, naive == 0)
  a <- a$predicted
  e <- dismo::evaluate(p, a)
  aucvalue = e@auc
  
  # brier score
  brierval <- sum((DataforAUC$predicted - DataforAUC$naive)^2) / nrow(DataforAUC)
  
  # add this data to the table
  newrow <- c(i, round(aucvalue,2), round(brierval,2))
  AUCTable_summer <- rbind(AUCTable_summer, newrow)
}
AUCTable_summer <- AUCTable_summer[2:11,]
AUCTable_summer
Score_summer = mean(AUCTable_summer$BrierScore)
Score_summer


##############################################################################
#                           fall  ----
##############################################################################


library(dismo); library(lme4)

# Brier Scores, "all" combined dataset
###############

fall_mod = readRDS("./for_brier/fall_Course_mod.rds") # model for nonbreeding
fall = read.csv("./for_brier/fall_ResSelData_Course.csv") # Data for nonbreeding

#################################################################### Iterative AUC and Briers Score!
fall = fall[-c(1)]
# fall 
fall$sub1 = rbinom(n = nrow(fall), size = 1, prob = 0.75) # create a "subset 1" column; 75% prob
nrow(subset(fall, sub1 == 1))/nrow(fall) # did it work? Yes

# fall: now let's make em for the rest of the 10 subsets
fall$sub2 = rbinom(n = nrow(fall), size = 1, prob = 0.75)
fall$sub3 = rbinom(n = nrow(fall), size = 1, prob = 0.75)
fall$sub4 = rbinom(n = nrow(fall), size = 1, prob = 0.75)
fall$sub5 = rbinom(n = nrow(fall), size = 1, prob = 0.75)
fall$sub6 = rbinom(n = nrow(fall), size = 1, prob = 0.75)
fall$sub7 = rbinom(n = nrow(fall), size = 1, prob = 0.75)
fall$sub8 = rbinom(n = nrow(fall), size = 1, prob = 0.75)
fall$sub9 = rbinom(n = nrow(fall), size = 1, prob = 0.75)
fall$sub10 = rbinom(n = nrow(fall), size = 1, prob = 0.75)

# make blank dataset to hold results
AUCTable_fall <- data.frame("DataSubset" = 0, "AUC" = 0, "BrierScore" = 0)

# in the for() loop 
# keep getting error that says "Error in levelfun(r, n, allow.new.levels = allow.new.levels) 


for(i in 1:10){
  #i = 1
  datasubset <- subset(fall, fall[,22+i] == 1) # first subset is col 24; get 1's
  
  # fit model with subset i
  
  topmod <- glmer(response ~ DTN_road + perc_grassy + perc_bf + ndvi + (1 | Bird.ID),
                  family = binomial, data = datasubset)
  
  # predict and arrange data for AUC
  testsubset <- subset(fall, fall[,22+i] == 0)# first subset is col 24; get 0's
  pred_1 <- predict(topmod, type = "response", newdata = testsubset, allow.new.levels = TRUE)
  testsubset$predicted <- pred_1
  DataforAUC <- data.frame("naive" = testsubset$response, "predicted" = testsubset$predicted)
  head(DataforAUC)
  
  # calculate AUC
  p <- subset(DataforAUC, naive == 1)
  p <- p$predicted
  a <- subset(DataforAUC, naive == 0)
  a <- a$predicted
  e <- dismo::evaluate(p, a)
  aucvalue = e@auc
  
  # brier score
  brierval <- sum((DataforAUC$predicted - DataforAUC$naive)^2) / nrow(DataforAUC)
  
  # add this data to the table
  newrow <- c(i, round(aucvalue,2), round(brierval,2))
  AUCTable_fall <- rbind(AUCTable_fall, newrow)
}
AUCTable_fall <- AUCTable_fall[2:11,]
AUCTable_fall
Score_fall = mean(AUCTable_fall$BrierScore)
Score_fall




##############################################################################
#                           winter ----   
##############################################################################


library(dismo); library(lme4)

# Brier Scores, "all" combined dataset
###############

winter_mod = readRDS("./for_brier/winter_Course_mod.rds") # model for nonbreeding
winter = read.csv("./for_brier/winter_ResSelData_Course.csv") # Data for nonbreeding

#################################################################### Iterative AUC and Briers Score!
winter = winter[-c(1)]
# winter 
winter$sub1 = rbinom(n = nrow(winter), size = 1, prob = 0.75) # create a "subset 1" column; 75% prob
nrow(subset(winter, sub1 == 1))/nrow(winter) # did it work? Yes

# winter: now let's make em for the rest of the 10 subsets
winter$sub2 = rbinom(n = nrow(winter), size = 1, prob = 0.75)
winter$sub3 = rbinom(n = nrow(winter), size = 1, prob = 0.75)
winter$sub4 = rbinom(n = nrow(winter), size = 1, prob = 0.75)
winter$sub5 = rbinom(n = nrow(winter), size = 1, prob = 0.75)
winter$sub6 = rbinom(n = nrow(winter), size = 1, prob = 0.75)
winter$sub7 = rbinom(n = nrow(winter), size = 1, prob = 0.75)
winter$sub8 = rbinom(n = nrow(winter), size = 1, prob = 0.75)
winter$sub9 = rbinom(n = nrow(winter), size = 1, prob = 0.75)
winter$sub10 = rbinom(n = nrow(winter), size = 1, prob = 0.75)

# make blank dataset to hold results
AUCTable_winter <- data.frame("DataSubset" = 0, "AUC" = 0, "BrierScore" = 0)

# in the for() loop 
# keep getting error that says "Error in levelfun(r, n, allow.new.levels = allow.new.levels) 


for(i in 1:10){
  #i = 1
  datasubset <- subset(winter, winter[,22+i] == 1) # first subset is col 24; get 1's
  
  # fit model with subset i
  
  topmod <- glmer(response ~ DTN_road + perc_grassy + perc_bf + ndvi + (1 | Bird.ID),
                  family = binomial, data = datasubset)
  
  # predict and arrange data for AUC
  testsubset <- subset(winter, winter[,22+i] == 0)# first subset is col 24; get 0's
  pred_1 <- predict(topmod, type = "response", newdata = testsubset, allow.new.levels = TRUE)
  testsubset$predicted <- pred_1
  DataforAUC <- data.frame("naive" = testsubset$response, "predicted" = testsubset$predicted)
  head(DataforAUC)
  
  # calculate AUC
  p <- subset(DataforAUC, naive == 1)
  p <- p$predicted
  a <- subset(DataforAUC, naive == 0)
  a <- a$predicted
  e <- dismo::evaluate(p, a)
  aucvalue = e@auc
  
  # brier score
  brierval <- sum((DataforAUC$predicted - DataforAUC$naive)^2) / nrow(DataforAUC)
  
  # add this data to the table
  newrow <- c(i, round(aucvalue,2), round(brierval,2))
  AUCTable_winter <- rbind(AUCTable_winter, newrow)
}
AUCTable_winter <- AUCTable_winter[2:11,]
AUCTable_winter
Score_winter = mean(AUCTable_winter$BrierScore)
Score_winter



Score_seasonal = mean(Score_winter, Score_spring, Score_summer, Score_fall)
