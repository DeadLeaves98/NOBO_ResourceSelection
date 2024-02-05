library(dismo); library(lme4)

# Brier Scores, "all" combined dataset
###############
mod_all <- readRDS("./for_brier/All_Course_mod.rds") # first, read in the model
dat_all <- read.csv("./ResSelData_Course.csv") # second, read in the dataset that goes w/ the model

#################################################################### Iterative AUC and Briers Score!

dataforROC <- dat_all # duplicate the dataset for ease of processing
dataforROC$sub1 = rbinom(n = nrow(dataforROC), size = 1, prob = 0.75) # create a "subset 1" column; 75% prob
nrow(subset(dataforROC, sub1 == 1))/nrow(dataforROC) # did it work? Yes

# now let's make em for the rest of the 10 subsets
dataforROC$sub2 = rbinom(n = nrow(dataforROC), size = 1, prob = 0.75)
dataforROC$sub3 = rbinom(n = nrow(dataforROC), size = 1, prob = 0.75)
dataforROC$sub4 = rbinom(n = nrow(dataforROC), size = 1, prob = 0.75)
dataforROC$sub5 = rbinom(n = nrow(dataforROC), size = 1, prob = 0.75)
dataforROC$sub6 = rbinom(n = nrow(dataforROC), size = 1, prob = 0.75)
dataforROC$sub7 = rbinom(n = nrow(dataforROC), size = 1, prob = 0.75)
dataforROC$sub8 = rbinom(n = nrow(dataforROC), size = 1, prob = 0.75)
dataforROC$sub9 = rbinom(n = nrow(dataforROC), size = 1, prob = 0.75)
dataforROC$sub10 = rbinom(n = nrow(dataforROC), size = 1, prob = 0.75)

# make blank dataset to hold results
AUCTable <- data.frame("DataSubset" = 0, "AUC" = 0, "BrierScore" = 0)

for(i in 1:10){
  #i = 1
  datasubset <- subset(dataforROC, dataforROC[,22+i] == 1) # first subset is col 23; get 1's
  
  # fit model with subset i
  
  topmod <- glmer(response ~ DTN_road + perc_grassy + perc_bf + ndvi + (1 | Bird.ID),
                  family = binomial, data = datasubset)
  
  # predict and arrange data for AUC
  testsubset <- subset(dataforROC, dataforROC[,22+i] == 0)# first subset is col 23; get 0's
  pred_1 <- predict(topmod, type = "response", newdata = testsubset)
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
  AUCTable <- rbind(AUCTable, newrow)
}

AUCTable <- AUCTable[2:11,]
AUCTable
mean(AUCTable$BrierScore)


