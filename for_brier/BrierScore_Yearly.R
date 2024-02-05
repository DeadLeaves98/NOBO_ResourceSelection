# BrierScore for the yearly temporal models 
# 2022 and 2023 
##########################################

library(dismo); library(lme4)

# Brier Scores, "all" combined dataset
###############

year22_mod = readRDS("./for_brier/year22_Course_mod.rds") # model for 2022
year23_mod = readRDS("./for_brier/year23_Course_mod.rds") # model for 2023 
dat_22 = read.csv("./for_brier/year2022_ResSelData_Course.csv") # Data for 2022 
dat_23 = read.csv("./for_brier/year2023_ResSelData_Course.csv") # Data for 2023 


# mod_all <- readRDS("./for_brier/All_Course_mod.rds") # first, read in the model
# dat_all <- read.csv("./ResSelData_Course.csv") # second, read in the dataset that goes w/ the model

#################################################################### Iterative AUC and Briers Score!

dat_22 # 2022 dataset for processing 
dat_22 = dat_22[-c(1)]
# 2022 
dat_22$sub1 = rbinom(n = nrow(dat_22), size = 1, prob = 0.75) # create a "subset 1" column; 75% prob
nrow(subset(dat_22, sub1 == 1))/nrow(dat_22) # did it work? Yes

# 2022: now let's make em for the rest of the 10 subsets
dat_22$sub2 = rbinom(n = nrow(dat_22), size = 1, prob = 0.75)
dat_22$sub3 = rbinom(n = nrow(dat_22), size = 1, prob = 0.75)
dat_22$sub4 = rbinom(n = nrow(dat_22), size = 1, prob = 0.75)
dat_22$sub5 = rbinom(n = nrow(dat_22), size = 1, prob = 0.75)
dat_22$sub6 = rbinom(n = nrow(dat_22), size = 1, prob = 0.75)
dat_22$sub7 = rbinom(n = nrow(dat_22), size = 1, prob = 0.75)
dat_22$sub8 = rbinom(n = nrow(dat_22), size = 1, prob = 0.75)
dat_22$sub9 = rbinom(n = nrow(dat_22), size = 1, prob = 0.75)
dat_22$sub10 = rbinom(n = nrow(dat_22), size = 1, prob = 0.75)

# make blank dataset to hold results
AUCTable22 <- data.frame("DataSubset" = 0, "AUC" = 0, "BrierScore" = 0)

# in the for() loop 
# keep getting error that says "Error in levelfun(r, n, allow.new.levels = allow.new.levels) 


for(i in 1:10){
  #i = 1
  datasubset <- subset(dat_22, dat_22[,22+i] == 1) # first subset is col 24; get 1's
  
  # fit model with subset i
  
  topmod <- glmer(response ~ DTN_road + perc_grassy + perc_bf + ndvi + (1 | Bird.ID),
                  family = binomial, data = datasubset)
  
  # predict and arrange data for AUC
  testsubset <- subset(dat_22, dat_22[,22+i] == 0)# first subset is col 24; get 0's
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
  AUCTable22 <- rbind(AUCTable22, newrow)
}
AUCTable22 <- AUCTable22[2:11,]
AUCTable22
Score22 = mean(AUCTable22$BrierScore)

################################################################################
#                                 2023
################################################################################


# Brier Scores, "all" combined dataset
###############

year23_mod = readRDS("./for_brier/year23_Course_mod.rds") # model for 2023 
dat_23 = read.csv("./for_brier/year2023_ResSelData_Course.csv") # Data for 2023 
dat_23 = dat_23[-c(1)]

dat_23 # 2023 dataset for processing 

# 2023
dat_23$sub1 = rbinom(n = nrow(dat_23), size = 1, prob = 0.75) # create a "subset 1" column; 75% prob
nrow(subset(dat_23, sub1 == 1))/nrow(dat_23) # did it work? Yes

# 2023: now let's make em for the rest of the 10 subsets
dat_23$sub2 = rbinom(n = nrow(dat_23), size = 1, prob = 0.75)
dat_23$sub3 = rbinom(n = nrow(dat_23), size = 1, prob = 0.75)
dat_23$sub4 = rbinom(n = nrow(dat_23), size = 1, prob = 0.75)
dat_23$sub5 = rbinom(n = nrow(dat_23), size = 1, prob = 0.75)
dat_23$sub6 = rbinom(n = nrow(dat_23), size = 1, prob = 0.75)
dat_23$sub7 = rbinom(n = nrow(dat_23), size = 1, prob = 0.75)
dat_23$sub8 = rbinom(n = nrow(dat_23), size = 1, prob = 0.75)
dat_23$sub9 = rbinom(n = nrow(dat_23), size = 1, prob = 0.75)
dat_23$sub10 = rbinom(n = nrow(dat_23), size = 1, prob = 0.75)

# make blank dataset to hold results
AUCTable23 <- data.frame("DataSubset" = 0, "AUC" = 0, "BrierScore" = 0)

# in the for() loop 
# keep getting error that says "Error in levelfun(r, n, allow.new.levels = allow.new.levels) 


for(i in 1:10){
  #i = 1
  datasubset <- subset(dat_23, dat_23[,22+i] == 1) # first subset is col 24; get 1's
  
  # fit model with subset i
  
  topmod <- glmer(response ~ DTN_road + perc_grassy + perc_bf + ndvi + (1 | Bird.ID),
                  family = binomial, data = datasubset)
  
  # predict and arrange data for AUC
  testsubset <- subset(dat_23, dat_23[,22+i] == 0)# first subset is col 24; get 0's
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
  AUCTable23 <- rbind(AUCTable23, newrow)
}
AUCTable23 <- AUCTable23[2:11,]
AUCTable23
Score2023 = mean(AUCTable23$BrierScore)
Score2023


yearlyscore = mean(Score2023, Score22)
