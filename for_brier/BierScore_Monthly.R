
##############################################################################
#                           jan ----
##############################################################################
# BrierScore for the yearly temporal models 
# 2022 and 2023 
##########################################

library(dismo); library(lme4)

# Brier Scores, "all" combined dataset
###############

jan_mod = readRDS("./for_brier/jan_Course_mod.rds") # model for nonbreeding
jan = read.csv("./for_brier/jan_ResSelData_Course.csv") # Data for nonbreeding

#################################################################### Iterative AUC and Briers Score!

jan # jan dataset for processing 
jan = jan[-c(1)]
# jan 
jan$sub1 = rbinom(n = nrow(jan), size = 1, prob = 0.75) # create a "subset 1" column; 75% prob
nrow(subset(jan, sub1 == 1))/nrow(jan) # did it work? Yes

# jan: now let's make em for the rest of the 10 subsets
jan$sub2 = rbinom(n = nrow(jan), size = 1, prob = 0.75)
jan$sub3 = rbinom(n = nrow(jan), size = 1, prob = 0.75)
jan$sub4 = rbinom(n = nrow(jan), size = 1, prob = 0.75)
jan$sub5 = rbinom(n = nrow(jan), size = 1, prob = 0.75)
jan$sub6 = rbinom(n = nrow(jan), size = 1, prob = 0.75)
jan$sub7 = rbinom(n = nrow(jan), size = 1, prob = 0.75)
jan$sub8 = rbinom(n = nrow(jan), size = 1, prob = 0.75)
jan$sub9 = rbinom(n = nrow(jan), size = 1, prob = 0.75)
jan$sub10 = rbinom(n = nrow(jan), size = 1, prob = 0.75)

# make blank dataset to hold results
AUCTable_jan <- data.frame("DataSubset" = 0, "AUC" = 0, "BrierScore" = 0)

# in the for() loop 
# keep getting error that says "Error in levelfun(r, n, allow.new.levels = allow.new.levels) 


for(i in 1:10){
  #i = 1
  datasubset <- subset(jan, jan[,22+i] == 1) # first subset is col 24; get 1's
  
  # fit model with subset i
  
  topmod <- glmer(response ~ DTN_road + perc_grassy + perc_bf + ndvi + (1 | Bird.ID),
                  family = binomial, data = datasubset)
  
  # predict and arrange data for AUC
  testsubset <- subset(jan, jan[,22+i] == 0)# first subset is col 24; get 0's
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
  AUCTable_jan <- rbind(AUCTable_jan, newrow)
}
AUCTable_jan <- AUCTable_jan[2:11,]
AUCTable_jan
Score_jan = mean(AUCTable_jan$BrierScore)
Score_jan


##############################################################################
#                           feb ----
##############################################################################
# BrierScore for the yearly temporal models 
# 2022 and 2023 
##########################################

library(dismo); library(lme4)

# Brier Scores, "all" combined dataset
###############

feb_mod = readRDS("./for_brier/feb_Course_mod.rds") # model for nonbreeding
feb = read.csv("./for_brier/feb_ResSelData_Course.csv") # Data for nonbreeding

#################################################################### Iterative AUC and Briers Score!

feb # feb dataset for processing 
feb = feb[-c(1)]
# feb 
feb$sub1 = rbinom(n = nrow(feb), size = 1, prob = 0.75) # create a "subset 1" column; 75% prob
nrow(subset(feb, sub1 == 1))/nrow(feb) # did it work? Yes

# feb: now let's make em for the rest of the 10 subsets
feb$sub2 = rbinom(n = nrow(feb), size = 1, prob = 0.75)
feb$sub3 = rbinom(n = nrow(feb), size = 1, prob = 0.75)
feb$sub4 = rbinom(n = nrow(feb), size = 1, prob = 0.75)
feb$sub5 = rbinom(n = nrow(feb), size = 1, prob = 0.75)
feb$sub6 = rbinom(n = nrow(feb), size = 1, prob = 0.75)
feb$sub7 = rbinom(n = nrow(feb), size = 1, prob = 0.75)
feb$sub8 = rbinom(n = nrow(feb), size = 1, prob = 0.75)
feb$sub9 = rbinom(n = nrow(feb), size = 1, prob = 0.75)
feb$sub10 = rbinom(n = nrow(feb), size = 1, prob = 0.75)

# make blank dataset to hold results
AUCTable_feb <- data.frame("DataSubset" = 0, "AUC" = 0, "BrierScore" = 0)

# in the for() loop 
# keep getting error that says "Error in levelfun(r, n, allow.new.levels = allow.new.levels) 


for(i in 1:10){
  #i = 1
  datasubset <- subset(feb, feb[,22+i] == 1) # first subset is col 24; get 1's
  
  # fit model with subset i
  
  topmod <- glmer(response ~ DTN_road + perc_grassy + perc_bf + ndvi + (1 | Bird.ID),
                  family = binomial, data = datasubset)
  
  # predict and arrange data for AUC
  testsubset <- subset(feb, feb[,22+i] == 0)# first subset is col 24; get 0's
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
  AUCTable_feb <- rbind(AUCTable_feb, newrow)
}
AUCTable_feb <- AUCTable_feb[2:11,]
AUCTable_feb
Score_feb = mean(AUCTable_feb$BrierScore)
Score_feb


##############################################################################
#                           mar ----
##############################################################################
# BrierScore for the yearly temporal models 
# 2022 and 2023 
##########################################

library(dismo); library(lme4)

# Brier Scores, "all" combined dataset
###############

mar_mod = readRDS("./for_brier/mar_Course_mod.rds") # model for nonbreeding
mar = read.csv("./for_brier/mar_ResSelData_Course.csv") # Data for nonbreeding

#################################################################### Iterative AUC and Briers Score!

mar # mar dataset for processing 
mar = mar[-c(1)]
# mar 
mar$sub1 = rbinom(n = nrow(mar), size = 1, prob = 0.75) # create a "subset 1" column; 75% prob
nrow(subset(mar, sub1 == 1))/nrow(mar) # did it work? Yes

# mar: now let's make em for the rest of the 10 subsets
mar$sub2 = rbinom(n = nrow(mar), size = 1, prob = 0.75)
mar$sub3 = rbinom(n = nrow(mar), size = 1, prob = 0.75)
mar$sub4 = rbinom(n = nrow(mar), size = 1, prob = 0.75)
mar$sub5 = rbinom(n = nrow(mar), size = 1, prob = 0.75)
mar$sub6 = rbinom(n = nrow(mar), size = 1, prob = 0.75)
mar$sub7 = rbinom(n = nrow(mar), size = 1, prob = 0.75)
mar$sub8 = rbinom(n = nrow(mar), size = 1, prob = 0.75)
mar$sub9 = rbinom(n = nrow(mar), size = 1, prob = 0.75)
mar$sub10 = rbinom(n = nrow(mar), size = 1, prob = 0.75)

# make blank dataset to hold results
AUCTable_mar <- data.frame("DataSubset" = 0, "AUC" = 0, "BrierScore" = 0)

# in the for() loop 
# keep getting error that says "Error in levelfun(r, n, allow.new.levels = allow.new.levels) 


for(i in 1:10){
  #i = 1
  datasubset <- subset(mar, mar[,22+i] == 1) # first subset is col 24; get 1's
  
  # fit model with subset i
  
  topmod <- glmer(response ~ DTN_road + perc_grassy + perc_bf + ndvi + (1 | Bird.ID),
                  family = binomial, data = datasubset)
  
  # predict and arrange data for AUC
  testsubset <- subset(mar, mar[,22+i] == 0)# first subset is col 24; get 0's
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
  AUCTable_mar <- rbind(AUCTable_mar, newrow)
}
AUCTable_mar <- AUCTable_mar[2:11,]
AUCTable_mar
Score_mar = mean(AUCTable_mar$BrierScore)
Score_mar


##############################################################################
#                           apr ----
##############################################################################
# BrierScore for the yearly temporal models 
# 2022 and 2023 
##########################################

library(dismo); library(lme4)

# Brier Scores, "all" combined dataset
###############

apr_mod = readRDS("./for_brier/apr_Course_mod.rds") # model for nonbreeding
apr = read.csv("./for_brier/apr_ResSelData_Course.csv") # Data for nonbreeding

#################################################################### Iterative AUC and Briers Score!

apr # apr dataset for processing 
apr = apr[-c(1)]
# apr 
apr$sub1 = rbinom(n = nrow(apr), size = 1, prob = 0.75) # create a "subset 1" column; 75% prob
nrow(subset(apr, sub1 == 1))/nrow(apr) # did it work? Yes

# apr: now let's make em for the rest of the 10 subsets
apr$sub2 = rbinom(n = nrow(apr), size = 1, prob = 0.75)
apr$sub3 = rbinom(n = nrow(apr), size = 1, prob = 0.75)
apr$sub4 = rbinom(n = nrow(apr), size = 1, prob = 0.75)
apr$sub5 = rbinom(n = nrow(apr), size = 1, prob = 0.75)
apr$sub6 = rbinom(n = nrow(apr), size = 1, prob = 0.75)
apr$sub7 = rbinom(n = nrow(apr), size = 1, prob = 0.75)
apr$sub8 = rbinom(n = nrow(apr), size = 1, prob = 0.75)
apr$sub9 = rbinom(n = nrow(apr), size = 1, prob = 0.75)
apr$sub10 = rbinom(n = nrow(apr), size = 1, prob = 0.75)

# make blank dataset to hold results
AUCTable_apr <- data.frame("DataSubset" = 0, "AUC" = 0, "BrierScore" = 0)

# in the for() loop 
# keep getting error that says "Error in levelfun(r, n, allow.new.levels = allow.new.levels) 


for(i in 1:10){
  #i = 1
  datasubset <- subset(apr, apr[,22+i] == 1) # first subset is col 24; get 1's
  
  # fit model with subset i
  
  topmod <- glmer(response ~ DTN_road + perc_grassy + perc_bf + ndvi + (1 | Bird.ID),
                  family = binomial, data = datasubset)
  
  # predict and arrange data for AUC
  testsubset <- subset(apr, apr[,22+i] == 0)# first subset is col 24; get 0's
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
  AUCTable_apr <- rbind(AUCTable_apr, newrow)
}
AUCTable_apr <- AUCTable_apr[2:11,]
AUCTable_apr
Score_apr = mean(AUCTable_apr$BrierScore)
Score_apr


##############################################################################
#                           may ----
##############################################################################
# BrierScore for the yearly temporal models 
# 2022 and 2023 
##########################################

library(dismo); library(lme4)

# Brier Scores, "all" combined dataset
###############

may_mod = readRDS("./for_brier/may_Course_mod.rds") # model for nonbreeding
may = read.csv("./for_brier/may_ResSelData_Course.csv") # Data for nonbreeding

#################################################################### Iterative AUC and Briers Score!

may # may dataset for processing 
may = may[-c(1)]
# may 
may$sub1 = rbinom(n = nrow(may), size = 1, prob = 0.75) # create a "subset 1" column; 75% prob
nrow(subset(may, sub1 == 1))/nrow(may) # did it work? Yes

# may: now let's make em for the rest of the 10 subsets
may$sub2 = rbinom(n = nrow(may), size = 1, prob = 0.75)
may$sub3 = rbinom(n = nrow(may), size = 1, prob = 0.75)
may$sub4 = rbinom(n = nrow(may), size = 1, prob = 0.75)
may$sub5 = rbinom(n = nrow(may), size = 1, prob = 0.75)
may$sub6 = rbinom(n = nrow(may), size = 1, prob = 0.75)
may$sub7 = rbinom(n = nrow(may), size = 1, prob = 0.75)
may$sub8 = rbinom(n = nrow(may), size = 1, prob = 0.75)
may$sub9 = rbinom(n = nrow(may), size = 1, prob = 0.75)
may$sub10 = rbinom(n = nrow(may), size = 1, prob = 0.75)

# make blank dataset to hold results
AUCTable_may <- data.frame("DataSubset" = 0, "AUC" = 0, "BrierScore" = 0)

# in the for() loop 
# keep getting error that says "Error in levelfun(r, n, allow.new.levels = allow.new.levels) 


for(i in 1:10){
  #i = 1
  datasubset <- subset(may, may[,22+i] == 1) # first subset is col 24; get 1's
  
  # fit model with subset i
  
  topmod <- glmer(response ~ DTN_road + perc_grassy + perc_bf + ndvi + (1 | Bird.ID),
                  family = binomial, data = datasubset)
  
  # predict and arrange data for AUC
  testsubset <- subset(may, may[,22+i] == 0)# first subset is col 24; get 0's
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
  AUCTable_may <- rbind(AUCTable_may, newrow)
}
AUCTable_may <- AUCTable_may[2:11,]
AUCTable_may
Score_may = mean(AUCTable_may$BrierScore)
Score_may



##############################################################################
#                           jun ----
##############################################################################
# BrierScore for the yearly temporal models 
# 2022 and 2023 
##########################################

library(dismo); library(lme4)

# Brier Scores, "all" combined dataset
###############

jun_mod = readRDS("./for_brier/jun_Course_mod.rds") # model for nonbreeding
jun = read.csv("./for_brier/jun_ResSelData_Course.csv") # Data for nonbreeding

#################################################################### Iterative AUC and Briers Score!

jun # jun dataset for processing 
jun = jun[-c(1)]
# jun 
jun$sub1 = rbinom(n = nrow(jun), size = 1, prob = 0.75) # create a "subset 1" column; 75% prob
nrow(subset(jun, sub1 == 1))/nrow(jun) # did it work? Yes

# jun: now let's make em for the rest of the 10 subsets
jun$sub2 = rbinom(n = nrow(jun), size = 1, prob = 0.75)
jun$sub3 = rbinom(n = nrow(jun), size = 1, prob = 0.75)
jun$sub4 = rbinom(n = nrow(jun), size = 1, prob = 0.75)
jun$sub5 = rbinom(n = nrow(jun), size = 1, prob = 0.75)
jun$sub6 = rbinom(n = nrow(jun), size = 1, prob = 0.75)
jun$sub7 = rbinom(n = nrow(jun), size = 1, prob = 0.75)
jun$sub8 = rbinom(n = nrow(jun), size = 1, prob = 0.75)
jun$sub9 = rbinom(n = nrow(jun), size = 1, prob = 0.75)
jun$sub10 = rbinom(n = nrow(jun), size = 1, prob = 0.75)

# make blank dataset to hold results
AUCTable_jun <- data.frame("DataSubset" = 0, "AUC" = 0, "BrierScore" = 0)

# in the for() loop 
# keep getting error that says "Error in levelfun(r, n, allow.new.levels = allow.new.levels) 


for(i in 1:10){
  #i = 1
  datasubset <- subset(jun, jun[,22+i] == 1) # first subset is col 24; get 1's
  
  # fit model with subset i
  
  topmod <- glmer(response ~ DTN_road + perc_grassy + perc_bf + ndvi + (1 | Bird.ID),
                  family = binomial, data = datasubset)
  
  # predict and arrange data for AUC
  testsubset <- subset(jun, jun[,22+i] == 0)# first subset is col 24; get 0's
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
  AUCTable_jun <- rbind(AUCTable_jun, newrow)
}
AUCTable_jun <- AUCTable_jun[2:11,]
AUCTable_jun
Score_jun = mean(AUCTable_jun$BrierScore)
Score_jun



##############################################################################
#                           jul ----
##############################################################################
# BrierScore for the yearly temporal models 
# 2022 and 2023 
##########################################

library(dismo); library(lme4)

# Brier Scores, "all" combined dataset
###############

jul_mod = readRDS("./for_brier/jul_Course_mod.rds") # model for nonbreeding
jul = read.csv("./for_brier/jul_ResSelData_Course.csv") # Data for nonbreeding

#################################################################### Iterative AUC and Briers Score!

jul # jul dataset for processing 
jul = jul[-c(1)]
# jul 
jul$sub1 = rbinom(n = nrow(jul), size = 1, prob = 0.75) # create a "subset 1" column; 75% prob
nrow(subset(jul, sub1 == 1))/nrow(jul) # did it work? Yes

# jul: now let's make em for the rest of the 10 subsets
jul$sub2 = rbinom(n = nrow(jul), size = 1, prob = 0.75)
jul$sub3 = rbinom(n = nrow(jul), size = 1, prob = 0.75)
jul$sub4 = rbinom(n = nrow(jul), size = 1, prob = 0.75)
jul$sub5 = rbinom(n = nrow(jul), size = 1, prob = 0.75)
jul$sub6 = rbinom(n = nrow(jul), size = 1, prob = 0.75)
jul$sub7 = rbinom(n = nrow(jul), size = 1, prob = 0.75)
jul$sub8 = rbinom(n = nrow(jul), size = 1, prob = 0.75)
jul$sub9 = rbinom(n = nrow(jul), size = 1, prob = 0.75)
jul$sub10 = rbinom(n = nrow(jul), size = 1, prob = 0.75)

# make blank dataset to hold results
AUCTable_jul <- data.frame("DataSubset" = 0, "AUC" = 0, "BrierScore" = 0)

# in the for() loop 
# keep getting error that says "Error in levelfun(r, n, allow.new.levels = allow.new.levels) 


for(i in 1:10){
  #i = 1
  datasubset <- subset(jul, jul[,22+i] == 1) # first subset is col 24; get 1's
  
  # fit model with subset i
  
  topmod <- glmer(response ~ DTN_road + perc_grassy + perc_bf + ndvi + (1 | Bird.ID),
                  family = binomial, data = datasubset)
  
  # predict and arrange data for AUC
  testsubset <- subset(jul, jul[,22+i] == 0)# first subset is col 24; get 0's
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
  AUCTable_jul <- rbind(AUCTable_jul, newrow)
}
AUCTable_jul <- AUCTable_jul[2:11,]
AUCTable_jul
Score_jul = mean(AUCTable_jul$BrierScore)
Score_jul



##############################################################################
#                           aug ----
##############################################################################
# BrierScore for the yearly temporal models 
# 2022 and 2023 
##########################################

library(dismo); library(lme4)

# Brier Scores, "all" combined dataset
###############

aug_mod = readRDS("./for_brier/aug_Course_mod.rds") # model for nonbreeding
aug = read.csv("./for_brier/aug_ResSelData_Course.csv") # Data for nonbreeding

#################################################################### Iterative AUC and Briers Score!

aug # aug dataset for processing 
aug = aug[-c(1)]
# aug 
aug$sub1 = rbinom(n = nrow(aug), size = 1, prob = 0.75) # create a "subset 1" column; 75% prob
nrow(subset(aug, sub1 == 1))/nrow(aug) # did it work? Yes

# aug: now let's make em for the rest of the 10 subsets
aug$sub2 = rbinom(n = nrow(aug), size = 1, prob = 0.75)
aug$sub3 = rbinom(n = nrow(aug), size = 1, prob = 0.75)
aug$sub4 = rbinom(n = nrow(aug), size = 1, prob = 0.75)
aug$sub5 = rbinom(n = nrow(aug), size = 1, prob = 0.75)
aug$sub6 = rbinom(n = nrow(aug), size = 1, prob = 0.75)
aug$sub7 = rbinom(n = nrow(aug), size = 1, prob = 0.75)
aug$sub8 = rbinom(n = nrow(aug), size = 1, prob = 0.75)
aug$sub9 = rbinom(n = nrow(aug), size = 1, prob = 0.75)
aug$sub10 = rbinom(n = nrow(aug), size = 1, prob = 0.75)

# make blank dataset to hold results
AUCTable_aug <- data.frame("DataSubset" = 0, "AUC" = 0, "BrierScore" = 0)

# in the for() loop 
# keep getting error that says "Error in levelfun(r, n, allow.new.levels = allow.new.levels) 


for(i in 1:10){
  #i = 1
  datasubset <- subset(aug, aug[,22+i] == 1) # first subset is col 24; get 1's
  
  # fit model with subset i
  
  topmod <- glmer(response ~ DTN_road + perc_grassy + perc_bf + ndvi + (1 | Bird.ID),
                  family = binomial, data = datasubset)
  
  # predict and arrange data for AUC
  testsubset <- subset(aug, aug[,22+i] == 0)# first subset is col 24; get 0's
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
  AUCTable_aug <- rbind(AUCTable_aug, newrow)
}
AUCTable_aug <- AUCTable_aug[2:11,]
AUCTable_aug
Score_aug = mean(AUCTable_aug$BrierScore)
Score_aug



##############################################################################
#                           sept ----
##############################################################################
# BrierScore for the yearly temporal models 
# 2022 and 2023 
##########################################

library(dismo); library(lme4)

# Brier Scores, "all" combined dataset
###############

sept_mod = readRDS("./for_brier/sept_Course_mod.rds") # model for nonbreeding
sept = read.csv("./for_brier/sept_ResSelData_Course.csv") # Data for nonbreeding

#################################################################### Iterative AUC and Briers Score!

sept # sept dataset for processing 
sept = sept[-c(1)]
# sept 
sept$sub1 = rbinom(n = nrow(sept), size = 1, prob = 0.75) # create a "subset 1" column; 75% prob
nrow(subset(sept, sub1 == 1))/nrow(sept) # did it work? Yes

# sept: now let's make em for the rest of the 10 subsets
sept$sub2 = rbinom(n = nrow(sept), size = 1, prob = 0.75)
sept$sub3 = rbinom(n = nrow(sept), size = 1, prob = 0.75)
sept$sub4 = rbinom(n = nrow(sept), size = 1, prob = 0.75)
sept$sub5 = rbinom(n = nrow(sept), size = 1, prob = 0.75)
sept$sub6 = rbinom(n = nrow(sept), size = 1, prob = 0.75)
sept$sub7 = rbinom(n = nrow(sept), size = 1, prob = 0.75)
sept$sub8 = rbinom(n = nrow(sept), size = 1, prob = 0.75)
sept$sub9 = rbinom(n = nrow(sept), size = 1, prob = 0.75)
sept$sub10 = rbinom(n = nrow(sept), size = 1, prob = 0.75)

# make blank dataset to hold results
AUCTable_sept <- data.frame("DataSubset" = 0, "AUC" = 0, "BrierScore" = 0)

# in the for() loop 
# keep getting error that says "Error in levelfun(r, n, allow.new.levels = allow.new.levels) 


for(i in 1:10){
  #i = 1
  datasubset <- subset(sept, sept[,22+i] == 1) # first subset is col 24; get 1's
  
  # fit model with subset i
  
  topmod <- glmer(response ~ DTN_road + perc_grassy + perc_bf + ndvi + (1 | Bird.ID),
                  family = binomial, data = datasubset)
  
  # predict and arrange data for AUC
  testsubset <- subset(sept, sept[,22+i] == 0)# first subset is col 24; get 0's
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
  AUCTable_sept <- rbind(AUCTable_sept, newrow)
}
AUCTable_sept <- AUCTable_sept[2:11,]
AUCTable_sept
Score_sept = mean(AUCTable_sept$BrierScore)
Score_sept



##############################################################################
#                           oct ----
##############################################################################
# BrierScore for the yearly temporal models 
# 2022 and 2023 
##########################################

library(dismo); library(lme4)

# Brier Scores, "all" combined dataset
###############

oct_mod = readRDS("./for_brier/oct_Course_mod.rds") # model for nonbreeding
oct = read.csv("./for_brier/oct_ResSelData_Course.csv") # Data for nonbreeding

#################################################################### Iterative AUC and Briers Score!

oct # oct dataset for processing 
oct = oct[-c(1)]
# oct 
oct$sub1 = rbinom(n = nrow(oct), size = 1, prob = 0.75) # create a "subset 1" column; 75% prob
nrow(subset(oct, sub1 == 1))/nrow(oct) # did it work? Yes

# oct: now let's make em for the rest of the 10 subsets
oct$sub2 = rbinom(n = nrow(oct), size = 1, prob = 0.75)
oct$sub3 = rbinom(n = nrow(oct), size = 1, prob = 0.75)
oct$sub4 = rbinom(n = nrow(oct), size = 1, prob = 0.75)
oct$sub5 = rbinom(n = nrow(oct), size = 1, prob = 0.75)
oct$sub6 = rbinom(n = nrow(oct), size = 1, prob = 0.75)
oct$sub7 = rbinom(n = nrow(oct), size = 1, prob = 0.75)
oct$sub8 = rbinom(n = nrow(oct), size = 1, prob = 0.75)
oct$sub9 = rbinom(n = nrow(oct), size = 1, prob = 0.75)
oct$sub10 = rbinom(n = nrow(oct), size = 1, prob = 0.75)

# make blank dataset to hold results
AUCTable_oct <- data.frame("DataSubset" = 0, "AUC" = 0, "BrierScore" = 0)

# in the for() loop 
# keep getting error that says "Error in levelfun(r, n, allow.new.levels = allow.new.levels) 


for(i in 1:10){
  #i = 1
  datasubset <- subset(oct, oct[,22+i] == 1) # first subset is col 24; get 1's
  
  # fit model with subset i
  
  topmod <- glmer(response ~ DTN_road + perc_grassy + perc_bf + ndvi + (1 | Bird.ID),
                  family = binomial, data = datasubset)
  
  # predict and arrange data for AUC
  testsubset <- subset(oct, oct[,22+i] == 0)# first subset is col 24; get 0's
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
  AUCTable_oct <- rbind(AUCTable_oct, newrow)
}
AUCTable_oct <- AUCTable_oct[2:11,]
AUCTable_oct
Score_oct = mean(AUCTable_oct$BrierScore)
Score_oct



##############################################################################
#                           nov ----
##############################################################################
# BrierScore for the yearly temporal models 
# 2022 and 2023 
##########################################

library(dismo); library(lme4)

# Brier Scores, "all" combined dataset
###############

nov_mod = readRDS("./for_brier/nov_Course_mod.rds") # model for nonbreeding
nov = read.csv("./for_brier/nov_ResSelData_Course.csv") # Data for nonbreeding

#################################################################### Iterative AUC and Briers Score!

nov # nov dataset for processing 
nov = nov[-c(1)]
# nov 
nov$sub1 = rbinom(n = nrow(nov), size = 1, prob = 0.75) # create a "subset 1" column; 75% prob
nrow(subset(nov, sub1 == 1))/nrow(nov) # did it work? Yes

# nov: now let's make em for the rest of the 10 subsets
nov$sub2 = rbinom(n = nrow(nov), size = 1, prob = 0.75)
nov$sub3 = rbinom(n = nrow(nov), size = 1, prob = 0.75)
nov$sub4 = rbinom(n = nrow(nov), size = 1, prob = 0.75)
nov$sub5 = rbinom(n = nrow(nov), size = 1, prob = 0.75)
nov$sub6 = rbinom(n = nrow(nov), size = 1, prob = 0.75)
nov$sub7 = rbinom(n = nrow(nov), size = 1, prob = 0.75)
nov$sub8 = rbinom(n = nrow(nov), size = 1, prob = 0.75)
nov$sub9 = rbinom(n = nrow(nov), size = 1, prob = 0.75)
nov$sub10 = rbinom(n = nrow(nov), size = 1, prob = 0.75)

# make blank dataset to hold results
AUCTable_nov <- data.frame("DataSubset" = 0, "AUC" = 0, "BrierScore" = 0)

# in the for() loop 
# keep getting error that says "Error in levelfun(r, n, allow.new.levels = allow.new.levels) 


for(i in 1:10){
  #i = 1
  datasubset <- subset(nov, nov[,22+i] == 1) # first subset is col 24; get 1's
  
  # fit model with subset i
  
  topmod <- glmer(response ~ DTN_road + perc_grassy + perc_bf + ndvi + (1 | Bird.ID),
                  family = binomial, data = datasubset)
  
  # predict and arrange data for AUC
  testsubset <- subset(nov, nov[,22+i] == 0)# first subset is col 24; get 0's
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
  AUCTable_nov <- rbind(AUCTable_nov, newrow)
}
AUCTable_nov <- AUCTable_nov[2:11,]
AUCTable_nov
Score_nov = mean(AUCTable_nov$BrierScore)
Score_nov




##############################################################################
#                           dec ----
##############################################################################
# BrierScore for the yearly temporal models 
# 2022 and 2023 
##########################################

library(dismo); library(lme4)

# Brier Scores, "all" combined dataset
###############

dec_mod = readRDS("./for_brier/dec_Course_mod.rds") # model for nonbreeding
dec = read.csv("./for_brier/dec_ResSelData_Course.csv") # Data for nonbreeding

#################################################################### Iterative AUC and Briers Score!

dec # dec dataset for processing 
dec = dec[-c(1)]
# dec 
dec$sub1 = rbinom(n = nrow(dec), size = 1, prob = 0.75) # create a "subset 1" column; 75% prob
nrow(subset(dec, sub1 == 1))/nrow(dec) # did it work? Yes

# dec: now let's make em for the rest of the 10 subsets
dec$sub2 = rbinom(n = nrow(dec), size = 1, prob = 0.75)
dec$sub3 = rbinom(n = nrow(dec), size = 1, prob = 0.75)
dec$sub4 = rbinom(n = nrow(dec), size = 1, prob = 0.75)
dec$sub5 = rbinom(n = nrow(dec), size = 1, prob = 0.75)
dec$sub6 = rbinom(n = nrow(dec), size = 1, prob = 0.75)
dec$sub7 = rbinom(n = nrow(dec), size = 1, prob = 0.75)
dec$sub8 = rbinom(n = nrow(dec), size = 1, prob = 0.75)
dec$sub9 = rbinom(n = nrow(dec), size = 1, prob = 0.75)
dec$sub10 = rbinom(n = nrow(dec), size = 1, prob = 0.75)

# make blank dataset to hold results
AUCTable_dec <- data.frame("DataSubset" = 0, "AUC" = 0, "BrierScore" = 0)

# in the for() loop 
# keep getting error that says "Error in levelfun(r, n, allow.new.levels = allow.new.levels) 


for(i in 1:10){
  #i = 1
  datasubset <- subset(dec, dec[,22+i] == 1) # first subset is col 24; get 1's
  
  # fit model with subset i
  
  topmod <- glmer(response ~ DTN_road + perc_grassy + perc_bf + ndvi + (1 | Bird.ID),
                  family = binomial, data = datasubset)
  
  # predict and arrange data for AUC
  testsubset <- subset(dec, dec[,22+i] == 0)# first subset is col 24; get 0's
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
  AUCTable_dec <- rbind(AUCTable_dec, newrow)
}
AUCTable_dec <- AUCTable_dec[2:11,]
AUCTable_dec
Score_dec = mean(AUCTable_dec$BrierScore)
Score_dec



Score_Monthly = mean(Score_jan, Score_feb, Score_mar, Score_apr, Score_may, Score_jun, 
                     Score_jul, Score_aug, Score_sept, Score_oct, Score_nov, Score_dec)
