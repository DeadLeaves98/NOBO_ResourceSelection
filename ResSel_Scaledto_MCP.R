# Title: Resource Selection _ Real vs Random _ Scale to MCP ----
# Author: Autumn Randall 
# PI: Dr. DJ McNeil
# Date finished: 
# Date last Edited: 1/16/2024 ----
##################################################

# Read in the "clean" csv for telemetry data from github repository
library(rgdal); library(raster); library(adehabitatHR); library(rgeos); library(sf); library(dplyr)
library(lubridate); library(stringr); library(hablar); library(AICcmodavg);  library(lme4)
nobo1 <- read.csv("./cleaned_NOBO_telem.csv")

# make blank data.frame() to hold randoms
#nobo2 <- nobo1[0,] # blank data.frame

#############################
############################# Part 1 MCP FORLOOP()----

# save Alber's Equal Area Conic projection
albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
OP <- readOGR("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2022/Adult data/Resource Use/shapefiles/OrtonCourses_JustTreatmentSites.shp")

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


## This has been checked and everything run properly as of 1/16/2023 - asr 








################################################################################
################################################################################

                #### PART 2: ADDING COVARIATES ####

#### Course  ---- 
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
names(nobo) # will remove extra columns later 


##### Adding Burn Status ---- 
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








