# BrierScore for the yearly temporal models 
# 2022 and 2023 
##########################################

library(dismo); library(lme4)

# Brier Scores, "all" combined dataset
###############

breeding_mod = readRDS("./for_brier/Br_Course_mod.rds") # model for breeding
nonbreeding_mod = readRDS("./for_brier/NONbr_Course_mod.rds") # model for nonbreeding
breeding = read.csv("./for_brier/breeding_ResSelData_Course.csv") # Data for breeding  
nonbreeding = read.csv("./for_brier/nonbreeding_ResSelData_Course.csv") # Data for nonbreeding


# mod_all <- readRDS("./for_brier/All_Course_mod.rds") # first, read in the model
# dat_all <- read.csv("./ResSelData_Course.csv") # second, read in the dataset that goes w/ the model

#################################################################### Iterative AUC and Briers Score!

breeding # breeding dataset for processing 
breeding = breeding[-c(1)]
# breeding 
breeding$sub1 = rbinom(n = nrow(breeding), size = 1, prob = 0.75) # create a "subset 1" column; 75% prob
nrow(subset(breeding, sub1 == 1))/nrow(breeding) # did it work? Yes

# breeding: now let's make em for the rest of the 10 subsets
breeding$sub2 = rbinom(n = nrow(breeding), size = 1, prob = 0.75)
breeding$sub3 = rbinom(n = nrow(breeding), size = 1, prob = 0.75)
breeding$sub4 = rbinom(n = nrow(breeding), size = 1, prob = 0.75)
breeding$sub5 = rbinom(n = nrow(breeding), size = 1, prob = 0.75)
breeding$sub6 = rbinom(n = nrow(breeding), size = 1, prob = 0.75)
breeding$sub7 = rbinom(n = nrow(breeding), size = 1, prob = 0.75)
breeding$sub8 = rbinom(n = nrow(breeding), size = 1, prob = 0.75)
breeding$sub9 = rbinom(n = nrow(breeding), size = 1, prob = 0.75)
breeding$sub10 = rbinom(n = nrow(breeding), size = 1, prob = 0.75)

# make blank dataset to hold results
AUCTablebreeding <- data.frame("DataSubset" = 0, "AUC" = 0, "BrierScore" = 0)

# in the for() loop 
# keep getting error that says "Error in levelfun(r, n, allow.new.levels = allow.new.levels) 


for(i in 1:10){
  #i = 1
  datasubset <- subset(breeding, breeding[,22+i] == 1) # first subset is col 24; get 1's
  
  # fit model with subset i
  
  topmod <- glmer(response ~ DTN_road + perc_grassy + perc_bf + ndvi + (1 | Bird.ID),
                  family = binomial, data = datasubset)
  
  # predict and arrange data for AUC
  testsubset <- subset(breeding, breeding[,22+i] == 0)# first subset is col 24; get 0's
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
  AUCTablebreeding <- rbind(AUCTablebreeding, newrow)
}
AUCTablebreeding <- AUCTablebreeding[2:11,]
AUCTablebreeding
Scorebreeding = mean(AUCTablebreeding$BrierScore)
Scorebreeding


##############################################################################
#                           Nonbreeding 
##############################################################################
# BrierScore for the yearly temporal models 
# 2022 and 2023 
##########################################

library(dismo); library(lme4)

# Brier Scores, "all" combined dataset
###############

nonbreeding_mod = readRDS("./for_brier/NONbr_Course_mod.rds") # model for nonbreeding
nonbreeding = read.csv("./for_brier/nonbreeding_ResSelData_Course.csv") # Data for nonbreeding

#################################################################### Iterative AUC and Briers Score!

nonbreeding # nonbreeding dataset for processing 
nonbreeding = nonbreeding[-c(1)]
# nonbreeding 
nonbreeding$sub1 = rbinom(n = nrow(nonbreeding), size = 1, prob = 0.75) # create a "subset 1" column; 75% prob
nrow(subset(nonbreeding, sub1 == 1))/nrow(nonbreeding) # did it work? Yes

# nonbreeding: now let's make em for the rest of the 10 subsets
nonbreeding$sub2 = rbinom(n = nrow(nonbreeding), size = 1, prob = 0.75)
nonbreeding$sub3 = rbinom(n = nrow(nonbreeding), size = 1, prob = 0.75)
nonbreeding$sub4 = rbinom(n = nrow(nonbreeding), size = 1, prob = 0.75)
nonbreeding$sub5 = rbinom(n = nrow(nonbreeding), size = 1, prob = 0.75)
nonbreeding$sub6 = rbinom(n = nrow(nonbreeding), size = 1, prob = 0.75)
nonbreeding$sub7 = rbinom(n = nrow(nonbreeding), size = 1, prob = 0.75)
nonbreeding$sub8 = rbinom(n = nrow(nonbreeding), size = 1, prob = 0.75)
nonbreeding$sub9 = rbinom(n = nrow(nonbreeding), size = 1, prob = 0.75)
nonbreeding$sub10 = rbinom(n = nrow(nonbreeding), size = 1, prob = 0.75)

# make blank dataset to hold results
AUCTable_nb <- data.frame("DataSubset" = 0, "AUC" = 0, "BrierScore" = 0)

# in the for() loop 
# keep getting error that says "Error in levelfun(r, n, allow.new.levels = allow.new.levels) 


for(i in 1:10){
  #i = 1
  datasubset <- subset(nonbreeding, nonbreeding[,22+i] == 1) # first subset is col 24; get 1's
  
  # fit model with subset i
  
  topmod <- glmer(response ~ DTN_road + perc_grassy + perc_bf + ndvi + (1 | Bird.ID),
                  family = binomial, data = datasubset)
  
  # predict and arrange data for AUC
  testsubset <- subset(nonbreeding, nonbreeding[,22+i] == 0)# first subset is col 24; get 0's
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
  AUCTable_nb <- rbind(AUCTable_nb, newrow)
}
AUCTable_nb <- AUCTable_nb[2:11,]
AUCTable_nb
Score_nonbreeding = mean(AUCTable_nb$BrierScore)
Score_nonbreeding


BiAnnual_Score = mean(Score_nonbreeding, Scorebreeding)
