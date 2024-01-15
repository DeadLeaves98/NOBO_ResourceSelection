# Title: 
# Author: Autumn Randall 
# PI: Dr. DJ McNeil
# Date finished: 
# Date last Edited: 1/15/2024 @ noon 
##################################################

# Read in the "clean" csv for telemetry data from github repository
library(rgdal); library(raster); library(adehabitatHR); library(rgeos); library(sf); library(dplyr)
library(lubridate); library(stringr); library(hablar); library(AICcmodavg);  library(lme4)
nobo1 <- read.csv("./cleaned_NOBO_telem.csv")
nrow(nobo1) # 23979
ncol(nobo1) #19
unique(nobo1$Location.Type) # This should be brood and regular 
unique(nobo1$Bird.Status) # If this says nest that is okay -- usually means bird is off its nest but its status is considered a nest

# one last tweak so at the end we only have regular and random pts
nobo1 = within(nobo1, Location.Type[Location.Type == 'Brood'] <- 'Regular')

#### TESTER BIRD ----
# subset one bird using the subset function
bob1 <- subset(nobo1, Bird.ID == unique(nobo1$Bird.ID)[100]) # randomly subsetting the data and taking the 200th bird

# Make MCP using the MCP function; need to convert bob1 to spatial object first though
bob_sp <- SpatialPoints(coords = data.frame("x" = bob1$x, "y" = bob1$y)) # convert to spatial object
crs(bob_sp) <- CRS("+init=epsg:4326") # define CRS 
plot(bob_sp) # make sure it worked
mcp <- mcp(bob_sp, percent=100) # create MCP for bob_sp WITH 100% of points 
# NOTE: throws warning "GEOS support is provided by..." but I think it's fine
plot(mcp, add = TRUE) # overlay MCP to make sure it worked

# Make 0m buffer around the MCP - keeping the code just making the buffer nonexistent 
albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
mcp1 <- spTransform(mcp, CRS(albers)) # transform MCP to projected coordinate system
bob_sp1 <- spTransform(bob_sp, CRS(albers)) # transform points to projected coordinate system
plot(mcp1); plot(bob_sp1, add = TRUE)
buff1 <- gBuffer(mcp1, width=0) # notice its at 0m and not 200
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
bob1 = select(bob1, -chick, -n)
newrandoms <- data.frame("X" = NA, "Bird.ID" = bob1$Bird.ID, "ObjectID" = NA, "Date" = NA, "Observer" = NA, 
                         "Bird.Status" = NA, "Fate" = NA, "Location.Type" = "Random", "x" = NA, "y" = NA, "encounter" = NA,
                         "year" = NA, "ordinal" = NA, "week" = NA, "breedingseason" = NA, "breedingseasonCov" = NA, "yearBinary" = NA)
bob1 <- rbind(bob1, newrandoms) # combine the new "randoms" with the real data
nrow(bob1) #68 = 34 observations * 2 
  #View(bob1)
#### TEST BIRD: END ---- 


#######################################
####################################### MCP FORLOOP ALL----
#######################################

# save Alber's Equal Area Conic projection
albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

# blankDF
RandomsDF <- data.frame("X" = NA, "Bird.ID" = NA, "ObjectID" = NA, "Date" = NA, "Observer" = NA, 
                        "Bird.Status" = NA, "Fate" = NA, "Location.Type" = "Random", "x" = NA, "y" = NA, "encounter" = NA,
                        "year" = NA, "ordinal" = NA, "week" = NA, "breedingseason" = NA, "breedingseasonCov" = NA, "yearBinary" = NA)

###################### The for() loop:

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
                           "Bird.Status" = NA, "Fate" = NA, "Location.Type" = "Random", "x" = randomcoords_i$coords.x1, "y" = randomcoords_i$coords.x2, "encounter" = NA,
                           "year" = NA, "ordinal" = NA, "week" = NA, "breedingseason" = NA, "breedingseasonCov" = NA, "yearBinary" = NA )
  RandomsDF <- rbind(RandomsDF, newrandoms) # combine the new "randoms" with the real data
}

#### END FORLOOP() ----

#View(RandomsDF)
RandomsDF1 <- RandomsDF[2:nrow(RandomsDF),] # get rid of blank row at the begining
RandomsDF1$n = NA # add blank column for "n"
RandomsDF1$chick = NA # add a blank column for chick 

nobo2 <- rbind(nobo1, RandomsDF1)
nobo2$response <- ifelse(nobo2$Location.Type == "Regular", 1, 0)
unique(nobo2$Location.Type) # now I need to fix this.. 

################################################################################
################################################################################

                #### ADDING COVARIATES ####








