


##############################################################################
#                           JAn/Feb ----
##############################################################################
# BrierScore for the yearly temporal models 
# 2022 and 2023 
##########################################

library(dismo); library(lme4)

# Brier Scores, "all" combined dataset
###############

jf_mod = readRDS("./for_brier/jf_Course_mod.rds") # model for nonbreeding
jf = read.csv("./for_brier/jf_ResSelData_Course.csv") # Data for nonbreeding

#################################################################### Iterative AUC and Briers Score!

jf # jf dataset for processing 
jf = jf[-c(1)]
# jf 
jf$sub1 = rbinom(n = nrow(jf), size = 1, prob = 0.75) # create a "subset 1" column; 75% prob
nrow(subset(jf, sub1 == 1))/nrow(jf) # did it work? Yes

# jf: now let's make em for the rest of the 10 subsets
jf$sub2 = rbinom(n = nrow(jf), size = 1, prob = 0.75)
jf$sub3 = rbinom(n = nrow(jf), size = 1, prob = 0.75)
jf$sub4 = rbinom(n = nrow(jf), size = 1, prob = 0.75)
jf$sub5 = rbinom(n = nrow(jf), size = 1, prob = 0.75)
jf$sub6 = rbinom(n = nrow(jf), size = 1, prob = 0.75)
jf$sub7 = rbinom(n = nrow(jf), size = 1, prob = 0.75)
jf$sub8 = rbinom(n = nrow(jf), size = 1, prob = 0.75)
jf$sub9 = rbinom(n = nrow(jf), size = 1, prob = 0.75)
jf$sub10 = rbinom(n = nrow(jf), size = 1, prob = 0.75)

# make blank dataset to hold results
AUCTable_jf <- data.frame("DataSubset" = 0, "AUC" = 0, "BrierScore" = 0)

# in the for() loop 
# keep getting error that says "Error in levelfun(r, n, allow.new.levels = allow.new.levels) 


for(i in 1:10){
  #i = 1
  datasubset <- subset(jf, jf[,22+i] == 1) # first subset is col 24; get 1's
  
  # fit model with subset i
  
  topmod <- glmer(response ~ DTN_road + perc_grassy + perc_bf + ndvi + (1 | Bird.ID),
                  family = binomial, data = datasubset)
  
  # predict and arrange data for AUC
  testsubset <- subset(jf, jf[,22+i] == 0)# first subset is col 24; get 0's
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
  AUCTable_jf <- rbind(AUCTable_jf, newrow)
}
AUCTable_jf <- AUCTable_jf[2:11,]
AUCTable_jf
Score_jf = mean(AUCTable_jf$BrierScore)
Score_jf




##############################################################################
#                          MARCH/APRIL----
##############################################################################
# BrierScore for the yearly temporal models 
# 2022 and 2023 
##########################################

library(dismo); library(lme4)

# Brier Scores, "all" combined dataset
###############

ma_mod = readRDS("./for_brier/ma_Course_mod.rds") # model for nonbreeding
ma = read.csv("./for_brier/ma_ResSelData_Course.csv") # Data for nonbreeding

#################################################################### Iterative AUC and Briers Score!

ma # ma dataset for processing 
ma = ma[-c(1)]
# ma 
ma$sub1 = rbinom(n = nrow(ma), size = 1, prob = 0.75) # create a "subset 1" column; 75% prob
nrow(subset(ma, sub1 == 1))/nrow(ma) # did it work? Yes

# ma: now let's make em for the rest of the 10 subsets
ma$sub2 = rbinom(n = nrow(ma), size = 1, prob = 0.75)
ma$sub3 = rbinom(n = nrow(ma), size = 1, prob = 0.75)
ma$sub4 = rbinom(n = nrow(ma), size = 1, prob = 0.75)
ma$sub5 = rbinom(n = nrow(ma), size = 1, prob = 0.75)
ma$sub6 = rbinom(n = nrow(ma), size = 1, prob = 0.75)
ma$sub7 = rbinom(n = nrow(ma), size = 1, prob = 0.75)
ma$sub8 = rbinom(n = nrow(ma), size = 1, prob = 0.75)
ma$sub9 = rbinom(n = nrow(ma), size = 1, prob = 0.75)
ma$sub10 = rbinom(n = nrow(ma), size = 1, prob = 0.75)

# make blank dataset to hold results
AUCTable_ma <- data.frame("DataSubset" = 0, "AUC" = 0, "BrierScore" = 0)

# in the for() loop 
# keep getting error that says "Error in levelfun(r, n, allow.new.levels = allow.new.levels) 


for(i in 1:10){
  #i = 1
  datasubset <- subset(ma, ma[,22+i] == 1) # first subset is col 24; get 1's
  
  # fit model with subset i
  
  topmod <- glmer(response ~ DTN_road + perc_grassy + perc_bf + ndvi + (1 | Bird.ID),
                  family = binomial, data = datasubset)
  
  # predict and arrange data for AUC
  testsubset <- subset(ma, ma[,22+i] == 0)# first subset is col 24; get 0's
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
  AUCTable_ma <- rbind(AUCTable_ma, newrow)
}
AUCTable_ma <- AUCTable_ma[2:11,]
AUCTable_ma
Score_ma = mean(AUCTable_ma$BrierScore)
Score_ma




##############################################################################
#                           MAY/JUNE ----
##############################################################################
# BrierScore for the yearly temporal models 
# 2022 and 2023 
##########################################

library(dismo); library(lme4)

# Brier Scores, "all" combined dataset
###############

mj_mod = readRDS("./for_brier/mj_Course_mod.rds") # model for nonbreeding
mj = read.csv("./for_brier/mj_ResSelData_Course.csv") # Data for nonbreeding

#################################################################### Iterative AUC and Briers Score!

mj # mj dataset for processing 
mj = mj[-c(1)]
# mj 
mj$sub1 = rbinom(n = nrow(mj), size = 1, prob = 0.75) # create a "subset 1" column; 75% prob
nrow(subset(mj, sub1 == 1))/nrow(mj) # did it work? Yes

# mj: now let's make em for the rest of the 10 subsets
mj$sub2 = rbinom(n = nrow(mj), size = 1, prob = 0.75)
mj$sub3 = rbinom(n = nrow(mj), size = 1, prob = 0.75)
mj$sub4 = rbinom(n = nrow(mj), size = 1, prob = 0.75)
mj$sub5 = rbinom(n = nrow(mj), size = 1, prob = 0.75)
mj$sub6 = rbinom(n = nrow(mj), size = 1, prob = 0.75)
mj$sub7 = rbinom(n = nrow(mj), size = 1, prob = 0.75)
mj$sub8 = rbinom(n = nrow(mj), size = 1, prob = 0.75)
mj$sub9 = rbinom(n = nrow(mj), size = 1, prob = 0.75)
mj$sub10 = rbinom(n = nrow(mj), size = 1, prob = 0.75)

# make blank dataset to hold results
AUCTable_mj <- data.frame("DataSubset" = 0, "AUC" = 0, "BrierScore" = 0)

# in the for() loop 
# keep getting error that says "Error in levelfun(r, n, allow.new.levels = allow.new.levels) 


for(i in 1:10){
  #i = 1
  datasubset <- subset(mj, mj[,22+i] == 1) # first subset is col 24; get 1's
  
  # fit model with subset i
  
  topmod <- glmer(response ~ DTN_road + perc_grassy + perc_bf + ndvi + (1 | Bird.ID),
                  family = binomial, data = datasubset)
  
  # predict and arrange data for AUC
  testsubset <- subset(mj, mj[,22+i] == 0)# first subset is col 24; get 0's
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
  AUCTable_mj <- rbind(AUCTable_mj, newrow)
}
AUCTable_mj <- AUCTable_mj[2:11,]
AUCTable_mj
Score_mj = mean(AUCTable_mj$BrierScore)
Score_mj




##############################################################################
#                           JULY AUGUST ----
##############################################################################
# BrierScore for the yearly temporal models 
# 2022 and 2023 
##########################################

library(dismo); library(lme4)

# Brier Scores, "all" combined dataset
###############

ja_mod = readRDS("./for_brier/ja_Course_mod.rds") # model for nonbreeding
ja = read.csv("./for_brier/ja_ResSelData_Course.csv") # Data for nonbreeding

#################################################################### Iterative AUC and Briers Score!

ja # ja dataset for processing 
ja = ja[-c(1)]
# ja 
ja$sub1 = rbinom(n = nrow(ja), size = 1, prob = 0.75) # create a "subset 1" column; 75% prob
nrow(subset(ja, sub1 == 1))/nrow(ja) # did it work? Yes

# ja: now let's make em for the rest of the 10 subsets
ja$sub2 = rbinom(n = nrow(ja), size = 1, prob = 0.75)
ja$sub3 = rbinom(n = nrow(ja), size = 1, prob = 0.75)
ja$sub4 = rbinom(n = nrow(ja), size = 1, prob = 0.75)
ja$sub5 = rbinom(n = nrow(ja), size = 1, prob = 0.75)
ja$sub6 = rbinom(n = nrow(ja), size = 1, prob = 0.75)
ja$sub7 = rbinom(n = nrow(ja), size = 1, prob = 0.75)
ja$sub8 = rbinom(n = nrow(ja), size = 1, prob = 0.75)
ja$sub9 = rbinom(n = nrow(ja), size = 1, prob = 0.75)
ja$sub10 = rbinom(n = nrow(ja), size = 1, prob = 0.75)

# make blank dataset to hold results
AUCTable_ja <- data.frame("DataSubset" = 0, "AUC" = 0, "BrierScore" = 0)

# in the for() loop 
# keep getting error that says "Error in levelfun(r, n, allow.new.levels = allow.new.levels) 


for(i in 1:10){
  #i = 1
  datasubset <- subset(ja, ja[,22+i] == 1) # first subset is col 24; get 1's
  
  # fit model with subset i
  
  topmod <- glmer(response ~ DTN_road + perc_grassy + perc_bf + ndvi + (1 | Bird.ID),
                  family = binomial, data = datasubset)
  
  # predict and arrange data for AUC
  testsubset <- subset(ja, ja[,22+i] == 0)# first subset is col 24; get 0's
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
  AUCTable_ja <- rbind(AUCTable_ja, newrow)
}
AUCTable_ja <- AUCTable_ja[2:11,]
AUCTable_ja
Score_ja = mean(AUCTable_ja$BrierScore)
Score_ja




##############################################################################
#                           SEPTEMBER AND OCTOBER ----
##############################################################################
# BrierScore for the yearly temporal models 
# 2022 and 2023 
##########################################

library(dismo); library(lme4)

# Brier Scores, "all" combined dataset
###############

so_mod = readRDS("./for_brier/so_Course_mod.rds") # model for nonbreeding
so = read.csv("./for_brier/so_ResSelData_Course.csv") # Data for nonbreeding

#################################################################### Iterative AUC and Briers Score!

so # so dataset for processing 
so = so[-c(1)]
# so 
so$sub1 = rbinom(n = nrow(so), size = 1, prob = 0.75) # create a "subset 1" column; 75% prob
nrow(subset(so, sub1 == 1))/nrow(so) # did it work? Yes

# so: now let's make em for the rest of the 10 subsets
so$sub2 = rbinom(n = nrow(so), size = 1, prob = 0.75)
so$sub3 = rbinom(n = nrow(so), size = 1, prob = 0.75)
so$sub4 = rbinom(n = nrow(so), size = 1, prob = 0.75)
so$sub5 = rbinom(n = nrow(so), size = 1, prob = 0.75)
so$sub6 = rbinom(n = nrow(so), size = 1, prob = 0.75)
so$sub7 = rbinom(n = nrow(so), size = 1, prob = 0.75)
so$sub8 = rbinom(n = nrow(so), size = 1, prob = 0.75)
so$sub9 = rbinom(n = nrow(so), size = 1, prob = 0.75)
so$sub10 = rbinom(n = nrow(so), size = 1, prob = 0.75)

# make blank dataset to hold results
AUCTable_so <- data.frame("DataSubset" = 0, "AUC" = 0, "BrierScore" = 0)

# in the for() loop 
# keep getting error that says "Error in levelfun(r, n, allow.new.levels = allow.new.levels) 


for(i in 1:10){
  #i = 1
  datasubset <- subset(so, so[,22+i] == 1) # first subset is col 24; get 1's
  
  # fit model with subset i
  
  topmod <- glmer(response ~ DTN_road + perc_grassy + perc_bf + ndvi + (1 | Bird.ID),
                  family = binomial, data = datasubset)
  
  # predict and arrange data for AUC
  testsubset <- subset(so, so[,22+i] == 0)# first subset is col 24; get 0's
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
  AUCTable_so <- rbind(AUCTable_so, newrow)
}
AUCTable_so <- AUCTable_so[2:11,]
AUCTable_so
Score_so = mean(AUCTable_so$BrierScore)
Score_so




##############################################################################
#                           November DEC ----
##############################################################################
# BrierScore for the yearly temporal models 
# 2022 and 2023 
##########################################

library(dismo); library(lme4)

# Brier Scores, "all" combined dataset
###############

nd_mod = readRDS("./for_brier/nd_Course_mod.rds") # model for nonbreeding
nd = read.csv("./for_brier/nd_ResSelData_Course.csv") # Data for nonbreeding

#################################################################### Iterative AUC and Briers Score!

nd # nd dataset for processing 
nd = nd[-c(1)]
# nd 
nd$sub1 = rbinom(n = nrow(nd), size = 1, prob = 0.75) # create a "subset 1" column; 75% prob
nrow(subset(nd, sub1 == 1))/nrow(nd) # did it work? Yes

# nd: now let's make em for the rest of the 10 subsets
nd$sub2 = rbinom(n = nrow(nd), size = 1, prob = 0.75)
nd$sub3 = rbinom(n = nrow(nd), size = 1, prob = 0.75)
nd$sub4 = rbinom(n = nrow(nd), size = 1, prob = 0.75)
nd$sub5 = rbinom(n = nrow(nd), size = 1, prob = 0.75)
nd$sub6 = rbinom(n = nrow(nd), size = 1, prob = 0.75)
nd$sub7 = rbinom(n = nrow(nd), size = 1, prob = 0.75)
nd$sub8 = rbinom(n = nrow(nd), size = 1, prob = 0.75)
nd$sub9 = rbinom(n = nrow(nd), size = 1, prob = 0.75)
nd$sub10 = rbinom(n = nrow(nd), size = 1, prob = 0.75)

# make blank dataset to hold results
AUCTable_nd <- data.frame("DataSubset" = 0, "AUC" = 0, "BrierScore" = 0)

# in the for() loop 
# keep getting error that says "Error in levelfun(r, n, allow.new.levels = allow.new.levels) 


for(i in 1:10){
  #i = 1
  datasubset <- subset(nd, nd[,22+i] == 1) # first subset is col 24; get 1's
  
  # fit model with subset i
  
  topmod <- glmer(response ~ DTN_road + perc_grassy + perc_bf + ndvi + (1 | Bird.ID),
                  family = binomial, data = datasubset)
  
  # predict and arrange data for AUC
  testsubset <- subset(nd, nd[,22+i] == 0)# first subset is col 24; get 0's
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
  AUCTable_nd <- rbind(AUCTable_nd, newrow)
}
AUCTable_nd <- AUCTable_nd[2:11,]
AUCTable_nd
Score_nd = mean(AUCTable_nd$BrierScore)
Score_nd




Score_Seasonal = mean(Score_nd, Score_so, Score_ja, Score_mj, Score_ma, Score_jf)
