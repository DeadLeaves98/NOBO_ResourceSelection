#### Title: Nonbreeding Season 2022 - 2023 _Resource Use_ ALL PARTS 
#### Author: Autumn Randall 
#### PI and reference: Dr. DJ McNeil 
#### Date: 9.25.2023
#### Last Editted: 9.25.2023 
##################################################################

# Bobwhites are super cool!

library(rgdal); library(raster); library(adehabitatHR); library(rgeos); library(sf); library(dplyr)
library(lubridate); library(stringr); library(hablar); library(AICcmodavg);  library(lme4)

# Goal: Buffer random points 
#################################### read in NOBO data and remove "extra" rows and columns

#set.seed -- this part is important because it ensures that we get the same random
#numbers each time we re-run the code
set.seed(12345)

#############################################################################

#### Part 1: MCP Buffer ####

# Importing the telemetry data that was seperated temporally in R (ResourceUse_DateandFilePrep.R) 
# from therons OG telemetrydata file 
# This does not include all of breeding season 2023 (stops at roughly 8/25/2023)

nobo1 <- read.csv("E:/NOBO Project Data/Analyses/NonBreeding Season/Telemetry Data/NonbreedingSeason2022_2023.csv")
head(nobo1)
nrow(nobo1)

# how many NOBOs in the dataset?
length(unique(nobo1$Bird.ID)) # 406 birds

# check what kinds of "bird.status" are present; need to remove nests, fates, etc.
unique(nobo1$Bird.Status) 

# remove nests, fates, etc. by only keeping "alive" points
nobo1 <- subset(nobo1, Bird.Status == "Alive & Active" | Bird.Status ==  "Alive & Inactive" | Bird.Status ==  "Alive - Mort Check Only")
unique(nobo1$Bird.Status) # confirm that it worked by viewing all unique bird statuses - should just be the "alive" ones

# how many birds remain?
length(unique(nobo1$Bird.ID)) # 733 birds

# a bunch of techs put chicks/broods/nests as "alive and active"... gotta filter those too
unique(nobo1$Location.Type)
nobo1 <- subset(nobo1, Location.Type == "Regular")
unique(nobo1$Location.Type)

# how many birds remain?
length(unique(nobo1$Bird.ID)) # 624 birds

# remove extra columns we don't need
nobo1 <- nobo1[, c(2:3, 5:7, 9:11, 29:30)]
nrow(nobo1) #5518 rows (individual location points)

# need to generate table of bird observation frequencies b/c mcp can only be made for birds w > 4 obs
lookup1 <- data.frame("birdID" = NA, "n" = 1)
for(i in 1:length(unique(nobo1$Bird.ID))){
  bob_i <- subset(nobo1, Bird.ID == unique(nobo1$Bird.ID)[i])  # subset ith bird
  newrow_i <- c(bob_i$Bird.ID[1], nrow(bob_i))
  lookup1 <- rbind(lookup1, newrow_i)
}

nobo1 <- merge(x = nobo1, y = lookup1, by.x = "Bird.ID", by.y = "birdID", all.x = TRUE)
length(unique(nobo1$Bird.ID))# still 349 birds
nobo1$n = as.numeric(nobo1$n) # change to numeric

nobo1 <- subset(nobo1, n > 4)
length(unique(nobo1$Bird.ID)) # only 484 birds...

################
#################################### Make MCP, buffer, and randoms for one bird to make sure I have the code working right

# subset one bird using the subset function
bob1 <- subset(nobo1, Bird.ID == unique(nobo1$Bird.ID)[100]) # randomly subsetting the data and taking the 200th bird

# Make MCP using the MCP function; need to convert bob1 to spatial object first though
bob_sp <- SpatialPoints(coords = data.frame("x" = bob1$x, "y" = bob1$y)) # convert to spatial object
crs(bob_sp) <- CRS("+init=epsg:4326") # define CRS 
plot(bob_sp) # make sure it worked
mcp <- mcp(bob_sp, percent=100) # create MCP for bob_sp
# NOTE: throws warning "GEOS support is provided by..." but I think it's fine
plot(mcp, add = TRUE) # overlay MCP to make sure it worked

# Make 200m buffer around the MCP
albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
mcp1 <- spTransform(mcp, CRS(albers)) # transform MCP to projected coordinate system
bob_sp1 <- spTransform(bob_sp, CRS(albers)) # transform points to projected coordinate system
plot(mcp1); plot(bob_sp1, add = TRUE)
buff1 <- gBuffer(mcp1, width=200)
# NOTE: throws warning "In proj4string(xy) : CRS object has comment..." but I think it's fine
plot(buff1, add = TRUE)

# generate random points (requires st_sample() which can only be done using sf)
buff2 <- st_as_sf(buff1) # convert to sf
points1 = st_sample(buff2, size=nrow(bob1) * 2) # generate 2 random points for each "used" point using st_sample()
points1 <- as_Spatial(points1) # convert back to sp
plot(buff1); plot(points1, add = TRUE, col = "red"); plot(bob_sp1, add = TRUE, col = "blue") # plot to confirm it worked
points2 <- spTransform(points1, CRS("+init=epsg:4326")) # convert to lat/long because this is what nobo1 uses
randomcoords <- data.frame(points2@coords) # extract lat/long from the points and convert to data.frame

#################################### make the dataframe for randoms suitable for rbind()

head(nobo1) # take a look at NOBO data.frame
# next, make the new piece of data to be r-binded
newrandoms <- data.frame("GlobalID" = NA, "Date" = NA, "Observer" = NA, "Bird.ID" = bob1$Bird.ID,
                         "Bird.Status" = NA, "Location.Type" = "Random", "Habitat.Type" = NA,
                         "Burn.Status" = NA, "x" = NA, "y" = NA)
bob1 <- dplyr::select(bob1, -n)
bob1 <- rbind(bob1, newrandoms) # combine the new "randoms" with the real data
#View(bob1)

#######################################
####################################### Now let's do all of the above with a for() loop
#######################################

# save Alber's Equal Area Conic projection
albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

# blankDF
RandomsDF <- data.frame("GlobalID" = NA, "Date" = NA, "Observer" = NA, "Bird.ID" = NA,
                        "Bird.Status" = NA, "Location.Type" = NA, "Habitat.Type" = NA,
                        "Burn.Status" = NA, "x" = NA, "y" = NA)

###################### The for() loop:

for(i in 1:length(unique(nobo1$Bird.ID))){
  
  # i = 33
  # subset ith bird
  bob_i <- subset(nobo1, Bird.ID == unique(nobo1$Bird.ID)[i]) 
  
  # Make MCP 
  bob_i_sp <- SpatialPoints(coords = data.frame("x" = bob_i$x, "y" = bob_i$y)) # convert to spatial object
  crs(bob_i_sp) <- CRS("+init=epsg:4326") # define CRS 
  mcp_i <- mcp(bob_i_sp, percent=100) # create MCP for bob_sp
  
  # Make 200m buffer around the MCP
  mcp_i <- spTransform(mcp_i, CRS(albers)) # transform MCP to projected coordinate system
  bob_i_sp <- spTransform(bob_i_sp, CRS(albers)) # transform points to projected coordinate system
  buff_i <- gBuffer(mcp_i, width=200)
  title_i = paste0("Bird ID = ", bob_i$Bird.ID[1], " (", i, " of ", length(unique(nobo1$Bird.ID)), ")")
  plot(buff_i, main = title_i); plot(mcp_i, add = TRUE); plot(bob_i_sp, add = TRUE)
  
  # generate random points (requires st_sample() which can only be done using sf)
  buff_i_2 <- st_as_sf(buff_i) # convert to sf
  rand_i = st_sample(buff_i_2, size=nrow(bob_i) * 3) # generate 2 random points for each "used" point using st_sample()
  rand_i <- as_Spatial(rand_i) # convert back to sp
  plot(rand_i, add = TRUE, col = "red") # plot to confirm it worked
  rand_i_2 <- spTransform(rand_i, CRS("+init=epsg:4326")) # convert to lat/long because this is what nobo1 uses
  randomcoords_i <- data.frame(rand_i_2@coords) # extract lat/long from the points and convert to data.frame
  
  # generate pseudo-dates for the randoms
  random_dates <- c(bob_i$Date, bob_i$Date, bob_i$Date)
  
  # new rows to be added to the "randoms" data.frame
  # next, make the new piece of data to be r-binded
  newrandoms <- data.frame("GlobalID" = NA, "Date" = random_dates, "Observer" = NA, "Bird.ID" = bob_i$Bird.ID[1],
                           "Bird.Status" = NA, "Location.Type" = "Random", "Habitat.Type" = NA,
                           "Burn.Status" = NA, "x" = randomcoords_i$coords.x1, "y" = randomcoords_i$coords.x2)
  RandomsDF <- rbind(RandomsDF, newrandoms) # combine the new "randoms" with the real data
}

#View(RandomsDF)
RandomsDF <- RandomsDF[2:nrow(RandomsDF),] # get rid of blank row at the begining
RandomsDF$n = NA # add blank column for "n"

nobo2 <- rbind(nobo1, RandomsDF)
nobo2$response <- ifelse(nobo2$Location.Type == "Regular", 1, 0)



##############################################################################

#### Part 2: Adding Course ID & Burn Status ####



# Turn nobo2 to nobo for the sake of ease because I definitely just copied this code 
nobo = nobo2
# read in OP shapefile
OP <- readOGR("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2022/Adult data/Resource Use/shapefiles/OrtonCourses_JustTreatmentSites.shp")
#OP <- readOGR("C:/Users/User/Desktop/Autumn_analyses/shapefiles/OrtonCourses_JustTreatmentSites.shp")

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
nrow(nobo)
length(extraction$course)

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
names(nobo)


############################ Adding Burn Status ########################################

# read in OP shapefile
Burn <- readOGR("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2022/Adult data/Resource Use/shapefiles/Master_Burn_Plan.shp")

# make nobo match burn map
nobo_sp1 <- spTransform(nobo_sp, crs(Burn)) # transform nobo_sp from WGS84 to match "burn"

# Check Coordinate systems by plotting 
plot(Burn); plot(nobo_sp1, add = TRUE)

extraction <- over(nobo_sp1, Burn)# extract burn status for points

# turn all nas within burnstatus.extract column into no 
extraction["Burns_2022"][is.na(extraction["Burns_2022"])] <- "no" 
unique(extraction$Burns_2022)

# add burn status to NOBO
nobo$burn_stat <- extraction$Burns_2022

# remove old, shitty "burn status" that techs record in the field
nobo <- dplyr::select(nobo, -Burn.Status)
head(nobo)



##############################################################################

#### Part 3: Individual Covariates ####



length(unique(nobo$Bird.ID)) #484

#inddata <- read.csv("C:/Users/User/Desktop/Autumn_analyses/rawdata/Master_Frequency_sheet_djm.csv")
inddata = read.csv("E:/NOBO Project Data/Raw Data/TrapData_08_29_2023/Trap_Data_0.csv") # Autumn's pathway
inddata$bird_id = paste0(inddata$Frequency_at_Release, "_", inddata$Band.ID)
nrow(inddata)

nobo.merge = merge(x = nobo, y = inddata, by.x = "Bird.ID", by.y = "bird_id", all.x = TRUE)
nrow(nobo.merge)

# remove birds that lack "individual" data
nobo.merge1 <- subset(nobo.merge, !is.na(nobo.merge[,"Sex"]))
#View(nobo.merge1)

# explore to make sure it worked
unique(nobo.merge1$Sex)
unique(nobo.merge1$Age)
par(mar=c(1, 1, 1, 1))
hist(nobo.merge1$Weight)
length(unique(nobo.merge1$Bird.ID))

###########

View(nobo.merge1)




################## Adding weather to nobo file
weather <- read.csv("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/weather/Temperature and Precipitation data.csv")

# Date column is currently a character column in both nobo.merge1 and weather
weather$Date = mdy(weather$Date) # Makes it a date column
nobo.merge1$Date = ymd(nobo.merge1$Date) # Makes it a date column

head(weather) 
head(nobo.merge)
nobo.merge3 = left_join(nobo.merge1, weather, by = "Date") 
head(nobo.merge3) # Check to make sure temperature and date combined 


#######################   ADDING TREATMENT TO MAIN NOBO DATABASE 

nobo_blower <- subset(nobo.merge3, course == "bigbay" | course == "bluepond" | course == "billjones")
nobo_blower$treatment = "blower"
nobo_spinner = subset(nobo.merge3, course == "other" |course == "fencecove" | course == "allenscreek" | course == "campcrane1" | course == "darkbranch" | course == "campcrane2")
nobo_spinner$treatment = "spinner"

nobo.merge4 = rbind(nobo_blower, nobo_spinner)
#View(nobo.merge4)



###############################################################################

#### Part 4: Extract Raster Values ####

nobo = nobo.merge4 # putting this into an easier object


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


# write.csv(nobo, "E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/BreedingSeason 2022_2023_ResourceUse_Adult_ALLparts_09sept23.csv", row.names = FALSE)





#############################################################################

#### Part 5: MAKING MODELLLSSSS ####
head(nobo)
nobo1 = nobo # changing to match following syntax 

null_mod <- glmer(response ~ 1 + (1|Bird.ID), family = binomial, data = nobo1)
dtn_decid_mod <- glmer(response ~ DTN_decid + (1|Bird.ID), family = binomial, data = nobo1)
dtn_road_mod <- glmer(response ~ DTN_road + (1|Bird.ID), family = binomial, data = nobo1)
dtn_mpine_mod <- glmer(response ~ DTN_mpine + (1|Bird.ID), family = binomial, data = nobo1)
dtn_grassy_mod <- glmer(response ~ DTN_grassy + (1|Bird.ID), family = binomial, data = nobo1)
dtn_bf_mod <- glmer(response ~ DTN_bf + (1|Bird.ID), family = binomial, data = nobo1)
dtn_water_mod <- glmer(response ~ DTN_water + (1|Bird.ID), family = binomial, data = nobo1)
ndvi_mod <- glmer(response ~ ndvi + (1|Bird.ID), family = binomial, data = nobo1)
per_mpine_mod <- glmer(response ~ perc_mpine + (1|Bird.ID), family = binomial, data = nobo1)
per_grassy_mod <- glmer(response ~ perc_grassy + (1|Bird.ID), family = binomial, data = nobo1)
per_decid_mod <- glmer(response ~ perc_decid + (1|Bird.ID), family = binomial, data = nobo1)
per_bf_mod <- glmer(response ~ perc_bf + (1|Bird.ID), family = binomial, data = nobo1)
per_water_mod <- glmer(response ~ perc_water + (1|Bird.ID), family = binomial, data = nobo1)
trt_mod <- glmer(response ~ treatment + (1|Bird.ID), family = binomial, data = nobo1)
burnstatus_mod <- glmer(response ~ burn_stat + (1|Bird.ID), family = binomial, data = nobo1)

modlist1 <- list(null_mod = null_mod, 
                 dtn_decid_mod = dtn_decid_mod, 
                 dtn_road_mod = dtn_road_mod,
                 dtn_mpine_mod = dtn_mpine_mod,
                 dtn_grassy_mod = dtn_grassy_mod,
                 dtn_bf_mod = dtn_bf_mod,
                 dtn_water_mod = dtn_water_mod,
                 ndvi_mod = ndvi_mod,
                 per_mpine_mod = per_mpine_mod,
                 per_grassy_mod = per_grassy_mod, 
                 per_decid_mod = per_decid_mod,
                 per_bf_mod = per_bf_mod,
                 per_water_mod = per_water_mod,
                 trt_mod = trt_mod,
                 burnstatus_mod = burnstatus_mod)

aictab(modlist1)

# ----------------------------------------------------------#
install.packages("MuMIn")
library(MuMIn)

#remove some of the extra columns that are unnecessary
nobo1 = nobo1[,-42:-47] 
nobo1 = nobo1[,-27:-28]

cor(nobo1[,41:50]) # removed perc_mpine

global_mod <- glmer(response ~ DTN_decid + DTN_road + DTN_mpine + DTN_grassy + 
                      DTN_bf + DTN_water + ndvi + perc_grassy + 
                      perc_decid +perc_bf + perc_water + burn_stat + 
                      (1|Bird.ID), family = binomial, data = nobo1, na.action = "na.fail")

dredge1 <- dredge(global_mod, m.max = 3)
View(dredge1)

#write.csv(dredge1, "E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/dredgeAdultNonBreeding.csv", row.names = FALSE)

top_mod <- glmer(response ~ DTN_road + perc_bf + ndvi + (1|Bird.ID), family = binomial, data = nobo1)

# saveRDS(top_mod, "E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/Top_mod_NonBreedingResourceUse2022_2023.rds")

q = read.csv("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/Resource Use/dredgeAdultNONBreeding.csv")
q
summary(a)
# ----------------------------------------------------------#

ModelToPredict <- glmer(response ~ DTN_road +  ndvi + perc_bf + 
                          (1|Bird.ID), family = binomial, data = nobo1, na.action = "na.fail")


newdat <- data.frame(
  ndvi = seq(min(nobo1$ndvi), max(nobo1$ndvi), length.out = 100),
  DTN_road = mean(nobo1$DTN_road), 
  perc_bf = mean(nobo1$perc_bf),
  response = 0)

newdat$response <- predict(ModelToPredict, newdata = newdat, type = "response", re.form = NA)

mm <- model.matrix(terms(ModelToPredict), newdat) 
y <- mm %*% fixef(ModelToPredict) 
pvar1 <- diag(mm %*% tcrossprod(vcov(ModelToPredict), mm)) 
tvar1 <- pvar1 + VarCorr(ModelToPredict)$Bird.ID[1] ### RANDOM EFFECT

newdat <- data.frame(
  ndvi = newdat$ndvi, # predictor variable
  y=(y)
  , plo = (y-1.96*sqrt(pvar1))
  , phi = (y+1.96*sqrt(pvar1))
  , tlo = (y-1.96*sqrt(tvar1))
  , thi = (y+1.96*sqrt(tvar1))
)

### exp(y)/((1+exp(y)) is the equation to back-tranform

newdat <- data.frame(
  ndvi = newdat$ndvi,
  y =((exp(newdat$y))/(1+exp(newdat$y)))
  , plo = exp(newdat$plo)/((1+exp(newdat$plo)))
  , phi = exp(newdat$phi)/((1+exp(newdat$phi)))
  , tlo = exp(newdat$tlo)/((1+exp(newdat$tlo)))
  , thi = exp(newdat$thi)/((1+exp(newdat$thi)))
)

# create plot
plot(-10,-10, ylim = c(0, 0.4), xlim = c(0, max(nobo1$ndvi)), 
     xlab = "NDVI", 
     ylab = "Probability of Use")
lines(newdat$ndvi, newdat$y)
lines(newdat$ndvi, newdat$plo, lty=2)
lines(newdat$ndvi, newdat$phi, lty=2)



# The data for plots is within 'newdat' 
# write.csv(newdat, "E:/NOBO Project Data/Analyses/NB_ndvi_DataForPlot.csv", row.names = F)




# ----------------------------------------------------------#

ModelToPredict <- ndvi_mod

newdat <- data.frame(
  ndvi = seq(min(nobo1$ndvi), max(nobo1$ndvi), length.out = 100),
  response = 0)

newdat$response <- predict(ModelToPredict, newdata = newdat, type = "response", re.form = NA)

mm <- model.matrix(terms(ModelToPredict), newdat) 
y <- mm %*% fixef(ModelToPredict) 
pvar1 <- diag(mm %*% tcrossprod(vcov(ModelToPredict), mm)) 
tvar1 <- pvar1 + VarCorr(ModelToPredict)$Bird.ID[1] ### RANDOM EFFECT

newdat <- data.frame(
  ndvi = newdat$ndvi, # predictor variable
  y=(y)
  , plo = (y-1.96*sqrt(pvar1))
  , phi = (y+1.96*sqrt(pvar1))
  , tlo = (y-1.96*sqrt(tvar1))
  , thi = (y+1.96*sqrt(tvar1))
)

### exp(y)/((1+exp(y)) is the equation to back-tranform

newdat <- data.frame(
  ndvi = newdat$ndvi, # predictor variable
  y =((exp(newdat$y))/(1+exp(newdat$y)))
  , plo = exp(newdat$plo)/((1+exp(newdat$plo)))
  , phi = exp(newdat$phi)/((1+exp(newdat$phi)))
  , tlo = exp(newdat$tlo)/((1+exp(newdat$tlo)))
  , thi = exp(newdat$thi)/((1+exp(newdat$thi)))
)

# create plot
plot(-10,-10, ylim = c(0, 0.4), xlim = c(min(nobo1$ndvi), max(nobo1$ndvi)), 
     xlab = "Normalized difference vegetation index (NDVI)", 
     ylab = "Probability of Use")
lines(newdat$ndvi, newdat$y)
lines(newdat$ndvi, newdat$plo, lty=2)
lines(newdat$ndvi, newdat$phi, lty=2)

# ------------------------------------------------------------------


ModelToPredict <- per_grassy_mod

newdat <- data.frame(
  perc_grassy = seq(min(nobo1$perc_grassy), max(nobo1$perc_grassy), length.out = 100),
  response = 0)

newdat$response <- predict(ModelToPredict, newdata = newdat, type = "response", re.form = NA)

mm <- model.matrix(terms(ModelToPredict), newdat) 
y <- mm %*% fixef(ModelToPredict) 
pvar1 <- diag(mm %*% tcrossprod(vcov(ModelToPredict), mm)) 
tvar1 <- pvar1 + VarCorr(ModelToPredict)$Bird.ID[1] ### RANDOM EFFECT

newdat <- data.frame(
  perc_grassy = newdat$perc_grassy, # predictor variable
  y=(y)
  , plo = (y-1.96*sqrt(pvar1))
  , phi = (y+1.96*sqrt(pvar1))
  , tlo = (y-1.96*sqrt(tvar1))
  , thi = (y+1.96*sqrt(tvar1))
)

### exp(y)/((1+exp(y)) is the equation to back-tranform

newdat <- data.frame(
  perc_grassy = newdat$perc_grassy, # predictor variable
  y =((exp(newdat$y))/(1+exp(newdat$y)))
  , plo = exp(newdat$plo)/((1+exp(newdat$plo)))
  , phi = exp(newdat$phi)/((1+exp(newdat$phi)))
  , tlo = exp(newdat$tlo)/((1+exp(newdat$tlo)))
  , thi = exp(newdat$thi)/((1+exp(newdat$thi)))
)

# create plot
plot(-10,-10, ylim = c(0, 0.4), xlim = c(min(nobo1$perc_grassy), max(nobo1$perc_grassy)), 
     xlab = "Percent Grassy", 
     ylab = "Probability of Use")
lines(newdat$perc_grassy, newdat$y)
lines(newdat$perc_grassy, newdat$plo, lty=2)
lines(newdat$perc_grassy, newdat$phi, lty=2)




