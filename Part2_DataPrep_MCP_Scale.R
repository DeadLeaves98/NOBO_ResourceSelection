# Title: Resource Selection _ Real vs Random _ Scale to MCP ----
# Author: Autumn Randall 
# PI: Dr. DJ McNeil
# Date finished: 
# Date last Edited: 1/16/2024 ----
##################################################

# Read in the "clean" csv for telemetry data from github repository
library(rgdal); library(raster); library(adehabitatHR); library(rgeos); library(sf); library(dplyr)
library(lubridate); library(stringr); library(hablar); library(AICcmodavg);  library(lme4)
nobo1 <- read.csv("./cleaned_Data.csv")

# make blank data.frame() to hold randoms
#nobo2 <- nobo1[0,] # blank data.frame

#############################
############################# Part 1 MCP FORLOOP()----

# save Alber's Equal Area Conic projection
albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
OP <- readOGR("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2022/Adult data/Resource Use/shapefiles/OrtonCourses_JustTreatmentSites.shp")

# Also fix the names in the shapefile
OP$course <- ifelse(OP$course == "campcrane2", "campcrane", OP$course) # change "campcrane2" to "campcrane"
OP$course <- ifelse(OP$course == "campcrane1", "campcrane", OP$course) # change "campcrane2" to "campcrane"

# blankDF
RandomsDF <- data.frame("X" = NA, "Bird.ID" = NA, "ObjectID" = NA, "Date" = NA, "Observer" = NA, 
                        "Bird.Status" = NA, "Fate" = NA, "Location.Type" = "Random", "x" = NA, "y" = NA, "chick" = NA, "encounter" = NA,
                        "year" = NA, "ordinal" = NA, "week" = NA, "breedingseason" = NA, "breedingseasonCov" = NA, "yearBinary" = NA, 
                        "n" = NA, "CentroidCourses" = NA)

####The for() loop:
for(i in 1:length(unique(nobo1$Bird.ID))){
  
  # i = 33
  # subset ith bird
  bob_i <- subset(nobo1, Bird.ID == unique(nobo1$Bird.ID)[i]) 
  
  # Make MCP 
  bob_i_sp <- SpatialPoints(coords = data.frame("x" = bob_i$x, "y" = bob_i$y)) # convert to spatial object
  crs(bob_i_sp) <- CRS("+init=epsg:4326") # define CRS 
  mcp_i <- mcp(bob_i_sp, percent=100) # create MCP for bob_sp
  plot(mcp_i) # plot to see if it worked 
  
  # Make 0m buffer around the MCP -- kept the code and made the buffer 0m
  mcp_i <- spTransform(mcp_i, CRS(albers)) # transform MCP to projected coordinate system
  bob_i_sp <- spTransform(bob_i_sp, CRS(albers)) # transform points to projected coordinate system
  buff_i <- gBuffer(mcp_i, width=0) #throws a warning but should be okay
  title_i = paste0("Bird ID = ", bob_i$Bird.ID[1], " (", i, " of ", length(unique(nobo1$Bird.ID)), ")")
  plot(buff_i, main = title_i); plot(mcp_i, add = TRUE); plot(bob_i_sp, add = TRUE)
  
  # generate random points (requires st_sample() which can only be done using sf)
  buff_i_2 <- st_as_sf(buff_i) # convert to sf
  rand_i = st_sample(buff_i_2, size=nrow(bob_i) * 2) # generate 2 random points for each "used" point using st_sample()
  rand_i <- as_Spatial(rand_i) # convert back to sp
  plot(rand_i, add = TRUE, col = "red") # plot to confirm it worked
  rand_i_2 <- spTransform(rand_i, CRS("+init=epsg:4326")) # convert to lat/long because this is what nobo1 uses
  randomcoords_i <- data.frame(rand_i_2@coords) # extract lat/long from the points and convert to data.frame
  
  # generate pseudo-dates for the randoms
  random_dates <- c(bob_i$Date, bob_i$Date)
  
  # new rows to be added to the "randoms" data.frame
  # next, make the new piece of data to be r-binded
  newrandoms <- data.frame("X" = NA, "Bird.ID" = bob_i$Bird.ID[1], "ObjectID" = NA, "Date" = random_dates, "Observer" = NA,
                           "Bird.Status" = NA, "Fate" = NA, "Location.Type" = "Random", "x" = randomcoords_i$coords.x1, "y" = randomcoords_i$coords.x2, "chick" = NA, "encounter" = NA,
                           "year" = NA, "ordinal" = NA, "week" = NA, "breedingseason" = NA, "breedingseasonCov" = NA, "yearBinary" = NA, "n" = bob_i$n, "CentroidCourses" = bob_i$CentroidCourses )
  RandomsDF <- rbind(RandomsDF, newrandoms) # combine the new "randoms" with the real data
}
#### END FORLOOP() ----

#View(RandomsDF)
RandomsDF <- RandomsDF[2:nrow(RandomsDF),] # get rid of blank row at the beginning
RandomsDF$n = NA # add blank column for "n"
RandomsDF$chick = NA # add a blank column for chick 

nobo2 <- rbind(nobo1, RandomsDF)


nobo2$response <- ifelse(nobo2$Location.Type == "Regular", 1, 0)
unique(nobo2$Location.Type) # 

#############################
############################# #SAVE MCP TELEMETRY ----

# write.csv(nobo2, "./MCP_CleanedTelem.csv")# this file has real vs random points generated for the MCP Scale 

## This has been checked and everything run properly as of 1/16/2023 - asr 

################################################################################
################################################################################
############################# Part 2: ADDING COVARIATES ----



##### Course  ---- 
# Turn nobo2 to nobo for the sake of ease because I definitely just copied this code 
nobo = nobo2

# Just as a reminder the course is in the object OP 
head(OP)

# bird locations as spatial
nobo_sp <- SpatialPoints(coords = data.frame("x" = nobo$x, "y" = nobo$y)) # convert DF to Spatial Points
crs(nobo_sp) <- CRS("+init=epsg:4326") # define CRS for the spatial points (EPSG 4326 == lat/lon WGS84 etc)
nobo_sp <- spTransform(nobo_sp, crs(OP)) # transform nobo_sp from WGS84 to match "roads"

# make sure they look OK
plot(OP); plot(nobo_sp, add = TRUE)

# extract course ID for points
extraction <- over(nobo_sp, OP)
extraction$course <- ifelse(is.na(extraction$course), "other", extraction$course) # change "NA" to "other"
extraction$course <- ifelse(extraction$course == "campcrane2", "campcrane", extraction$course) # change "campcrane2" to "campcrane"
extraction$course <- ifelse(extraction$course == "campcrane1", "campcrane", extraction$course) # change "campcrane1" to "campcrane"
unique(extraction$course)

# same number of rows for "nobo" and "extract"
nrow(nobo) #71625
length(extraction$course) #71625

# cbind course with bird observations
nobo$course <- extraction$course
head(nobo) 

# make sure it worked
acb <- subset(nobo, course == "allenscreek")
acb_sp <- SpatialPoints(coords = data.frame("x" = acb$x, "y" = acb$y)) # convert DF to Spatial Points
crs(acb_sp) <- CRS("+init=epsg:4326") # define CRS for the spatial points (EPSG 4326 == lat/lon WGS84 etc)
acb_sp <- spTransform(acb_sp, crs(OP)) # transform nobo_sp from WGS84 to match "roads"
plot(OP); plot(acb_sp, add = TRUE, cex = 1, pch = ".")

# Check to see if we are gucci
head(nobo)
names(nobo) # will remove extra columns later 


##### Adding Burn Status ---- 
# read in OP shapefile
burn22 <- readOGR("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2022/Adult data/Resource Use/shapefiles/BurnMap2022.shp")
burn23 = readOGR("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2022/Adult data/Resource Use/shapefiles/BurnMap2023.shp")

nobo$Date = ymd(nobo$Date) # make this into a date column and not a character column
nobo22 = nobo[nobo$Date >= "2022-01-01" & nobo$Date <="2022-12-31", ] # dataframe to hold 2022 birds and randoms
nobo23 = nobo[nobo$Date >= "2023-01-01" & nobo$Date <="2023-12-31", ] # dataframe to hold 2023 birds and randoms

# 2022
nobo22_sp <- SpatialPoints(coords = data.frame("x" = nobo22$x, "y" = nobo22$y)) # convert DF to Spatial Points
crs(nobo22_sp) <- CRS("+init=epsg:4326") # define CRS for the spatial points (EPSG 4326 == lat/lon WGS84 etc)
nobo22_sp <- spTransform(nobo22_sp, crs(burn22)) # transform nobo_sp from WGS84 to match "roads"
plot(burn22); plot(nobo22_sp, add = TRUE) # check to see if it worked
extraction22 <- over(nobo22_sp, burn22)# extract burn status for points
extraction22["Burns_2022"][is.na(extraction22["Burns_2022"])] <- "no" 
unique(extraction22$Burns_2022)
nobo22$burn_stat = extraction22$Burns_2022

nobo22$year = 2022 # giving it a year (honestly, might be unnecesary)
nobo22$yearBinary = 0 #making year binary  YEAR 2022 = 0

# 2023
nobo23_sp <- SpatialPoints(coords = data.frame("x" = nobo23$x, "y" = nobo23$y)) # convert DF to Spatial Points
crs(nobo23_sp) <- CRS("+init=epsg:4326") # define CRS for the spatial points (EPSG 4326 == lat/lon WGS84 etc)
nobo23_sp <- spTransform(nobo23_sp, crs(burn23)) # transform nobo_sp from WGS84 to match "roads"
plot(burn23);plot(nobo23_sp, add = TRUE) # check to see if it worked
extraction23 <- over(nobo23_sp, burn23)# extract burn status for points
extraction23["Burns_2023"][is.na(extraction23["Burns_2023"])] <- "no" 
nobo23$burn_stat = extraction23$Burns_2023

nobo23$year = 2023 # giving it a year (honestly, might be unnecesary)
nobo23$yearBinary = 1 #making year binary YEAR 2023 = 1

#rbind the 2022 and 2023 back together 
nobo = rbind(nobo23, nobo22)
names(nobo)

nrow(nobo)
# remove old, shitty "burn status" that techs record in the field

##### Burn stat and course end ----


##### Individual Covariates ####


length(unique(nobo$Bird.ID)) # 592

#inddata <- read.csv("C:/Users/User/Desktop/Autumn_analyses/rawdata/Master_Frequency_sheet_djm.csv")
inddata = read.csv("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Trap_data_0_21Nov2023.csv") # Autumn's pathway
inddata$bird_id = paste0(inddata$Frequency_at_Release, "_", inddata$Band.ID)
nrow(inddata) #1663 - NOT THAT THIS MATTERS 
head(inddata)

# merge trap data and nobo to give ind covs (sex, age, weight) to birds 
nobo.merge = merge(x = nobo, y = inddata, by.x = "Bird.ID", by.y = "BirdID", all.x = TRUE) 
# TO NOTE: 'BirdID' column i manually fixed to make it so that every bird with a freq ending in a 0 
# would combine properly with the band id during concatenate. Previously, it would remove the 0 and we would lose over 7000 pts 

nrow(nobo.merge) # 72948 

# remove birds that lack "individual" data
nobo.merge1 <- subset(nobo.merge, !is.na(nobo.merge[,"Sex"]))
nrow(nobo.merge1) # 72561 -- this could be due to birds caught in fall of 2023 may not have trap data 
#View(nobo.merge1)

# for now  I am going to leave birds that may not have sex, age, weight 
# this is to give us as many pts as possible
# and for resource selection I do not thiiinnnkkk I tested against individual cov.. 
# which makes me start to question why we even add them... 
# whattevvverrrr: should probs come back to this. rip 🪦 

##### Add Treatment ----
unique(nobo.merge1$course)

nobo_blower <- subset(nobo.merge1, course == "bigbay" | course == "bluepond" | course == "billjones")
nobo_blower$treatment = "blower"
P = nrow(nobo_blower)
P #33119

nobo_spinner = subset(nobo.merge1, course == "other" | course == "fencecove" | course == "allenscreek" | course == "campcrane" | course == "darkbranch")
nobo_spinner$treatment = "spinner"
U = nrow(nobo_spinner)
U # 39442
P + U # should be 72561 

nobo.merge1 = rbind(nobo_blower, nobo_spinner)
nrow(nobo.merge1)


##### Extract Habitat Covs  - Raster ----
nobo = nobo.merge1 # putting this into an easier object


# EXTRACTING RASTER VALUES

nobo_sp <- SpatialPoints(coords = data.frame("x" = nobo$x, "y" = nobo$y)) # convert DF to Spatial Points
crs(nobo_sp) <- CRS("+init=epsg:4326") # define CRS for the spatial points (EPSG 4326 == lat/lon WGS84 etc)

# read in rasters
list.files("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/rasters")
NDVI <- raster("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/rasters/2019NDVI.tif")
perc_mpine <- raster("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/rasters/PercentMaturePine.tif")
perc_grassy <- raster("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/rasters/Percentgrassy.tif")
perc_decid <- raster("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/rasters/PercentDeciduous.tif")
perc_bf <- raster("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/rasters/PercentBroodField.tif")
perc_water <- raster("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/rasters/PercentWater.tif")
DTN_road <- raster("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/rasters/DTN_road.tif")
DTN_mpine <- raster("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/rasters/DTN_maturepine.tif")
DTN_grassy <- raster("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/rasters/DTN_grassy.tif")
DTN_decid <- raster("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/rasters/DTN_deciduous.tif")
DTN_bf <- raster("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/rasters/DTN_broodfield1.tif")
DTN_water <- raster("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/rasters/DTN_water.tif")

# reproject nobo and extract
nobo_sp1 <- spTransform(nobo_sp, crs(NDVI)) # transform to NAD83
plot(NDVI); plot(nobo_sp1, add = TRUE)
ndv1_ex <- raster::extract(x = NDVI, y = nobo_sp1)
perc_mpine_ex <- raster::extract(x = perc_mpine, y = nobo_sp1)
perc_grassy_ex <- raster::extract(x = perc_grassy, y = nobo_sp1)
perc_decid_ex <- raster::extract(x = perc_decid, y = nobo_sp1)
perc_bf_ex <- raster::extract(x = perc_bf, y = nobo_sp1)
perc_water_ex <- raster::extract(x = perc_water, y = nobo_sp1)

nobo_sp2 <- spTransform(nobo_sp, crs(DTN_road)) # transform to pseudo-mercator
plot(DTN_road); plot(nobo_sp2, add = TRUE)
DTN_road_ex <- raster::extract(x = DTN_road, y = nobo_sp2)
DTN_mpine_ex <- raster::extract(x = DTN_mpine, y = nobo_sp2)
DTN_grassy_ex <- raster::extract(x = DTN_grassy, y = nobo_sp2)
DTN_decid_ex <- raster::extract(x = DTN_decid, y = nobo_sp2)
DTN_bf_ex <- raster::extract(x = DTN_bf, y = nobo_sp2)
DTN_water_ex <- raster::extract(x = DTN_water, y = nobo_sp2)

hist(DTN_water_ex)

# add columns to NOBO and export
nobo$ndvi <- ndv1_ex
nobo$perc_mpine <- perc_mpine_ex
nobo$perc_grassy <- perc_grassy_ex
nobo$perc_decid <- perc_decid_ex
nobo$perc_bf <- perc_bf_ex
nobo$perc_water <- perc_water_ex
nobo$DTN_road <- DTN_road_ex
nobo$DTN_mpine <- DTN_mpine_ex
nobo$DTN_grassy <- DTN_grassy_ex
nobo$DTN_decid <- DTN_decid_ex
nobo$DTN_bf <- DTN_bf_ex
nobo$DTN_water <- DTN_water_ex 
###

# View(nobo)
nrow(nobo)

# make burn stat binary: 0 = burned, 1 = unburned 
head(nobo)
nobo$burn_stat <- str_replace(nobo$burn_stat , "no", "1")
nobo$burn_stat <- str_replace(nobo$burn_stat , "Yes", "0")

# Remove Extra columns ---- 
nobo1 = select(nobo, -"X", -"chick", -"encounter", -"breedingseasonCov", -"Capture.Date", -"Course.ID", -"Trap", 
               -"Band.ID", -"Recapture", -"Immature", -"Condition", -"Leg.Band", -"Wing.Band", -"Tag.Status.at.Capture",
               -"Manufacturer_at_Capture", -"SerialNum_at_Capture", -"Frequency_at_Capture", 
               -"Tag.Status.at.Release", -"Manufacturer_at_Release", -"SerialNum_at_Release", -"Frequency_at_Release",
               -"Comments", -"ObjectId", -"WingChord", -"Tarsus", -"RadioSize", "bird_id", -"n",
               -"Observer", -"ObjectID", -"Bird.Status", -"Fate", -"year", -"Location.Type", -"Sex",
               -"Age", -"Weight", -"bird_id", -"treatment", -"CentroidCourses", -"ordinal", -"breedingseason")
# response: 0 = random, 1 = real 
# year: 0 = 2022, 1 = 2023 
# I thinned all columns to the bare minimum so if we want anything we just have to take out what we want from the above code 
names(nobo1)
head(nobo1)
# write.csv(nobo1, "./ResSelData_MCP.csv") # this file has real vs random points generated for the MCP Scale 
