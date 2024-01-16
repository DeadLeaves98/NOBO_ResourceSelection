# Title: 
# Author: Autumn Randall 
# PI: Dr. DJ McNeil
# Date finished: 
# Date last Edited: 1/15/2024 @ noon 
##################################################

# Read in the "clean" csv for telemetry data from github repository
library(rgdal); library(raster); library(adehabitatHR); library(rgeos); library(sf); library(dplyr)
library(lubridate); library(stringr); library(hablar); library(AICcmodavg);  library(lme4); library(lwgeom)
nobo1 <- read.csv("./cleaned_NOBO_telem.csv")
nrow(nobo1) # 23979
ncol(nobo1) #19
unique(nobo1$Location.Type) # This should be brood and regular 
unique(nobo1$Bird.Status) # If this says nest that is okay -- usually means bird is off its nest but its status is considered a nest

# one last tweak so at the end we only have regular and random pts
nobo1 = within(nobo1, Location.Type[Location.Type == 'Brood'] <- 'Regular')

#############################
############################# MCP FORLOOP()----

# save Alber's Equal Area Conic projection
albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

# blankDF
RandomsDF <- data.frame("X" = NA, "Bird.ID" = NA, "ObjectID" = NA, "Date" = NA, "Observer" = NA, 
                        "Bird.Status" = NA, "Fate" = NA, "Location.Type" = "Random", "x" = NA, "y" = NA, "encounter" = NA,
                        "year" = NA, "ordinal" = NA, "week" = NA, "breedingseason" = NA, "breedingseasonCov" = NA, "yearBinary" = NA)

#### for() loop that identifies the bird's course via centroid
# This is needed b/c the NEXT for() loop generates random points
# within the course that each bird lives in

CentroidCourses <- c() # blank object to hold course info

for(i in 1:length(unique(nobo1$Bird.ID))){
  
  # i = 100
  # subset one bird using the subset function
  nobo_i <- subset(nobo1, Bird.ID == unique(nobo1$Bird.ID)[i]) # randomly subsetting the data and taking the 100th bird'
  
  # get the average point and make IT also a spatial object
  nobo_i_avg_sp <- SpatialPoints(coords = data.frame("x" = mean(nobo_i$x), "y" = mean(nobo_i$y))) # convert DF to Spatial Points
  crs(nobo_i_avg_sp) <- CRS("+init=epsg:4326") # define CRS for the spatial points (EPSG 4326 == lat/lon WGS84 etc)
  nobo_i_avg_sp <- spTransform(nobo_i_avg_sp, crs(OP))
  
  # make sure they look OK
  plot(OP); plot(nobo_i_avg_sp, add = TRUE, col = "blue") 
  # extract course ID for points
  extraction <- over(nobo_i_avg_sp, OP) # place the centroid over the top of the course shapefile
  course_i <- extraction$course # extract course from the centroid 'bob_avg_sp'
  course_i <- ifelse(is.na(course_i) == TRUE, "other", course_i)
  
  # add new course info to CentroidCourses
  NewCourseData <- rep(course_i, nrow(nobo_i))
  CentroidCourses <- c(CentroidCourses, NewCourseData)
  
  # progress bar
  compl <- round(i/length(unique(nobo1$Bird.ID))*50,0)
  print(paste0("[",strrep("|", compl),strrep(".", 50-compl),"] ", round(100*i/length(unique(nobo1$Bird.ID)),0), "% complete"))
}

# add CentroidCourses to nobo1
nobo1$CentroidCourses <- CentroidCourses

# combine campcranes 1 and 2
nobo1$CentroidCourses <- ifelse(nobo1$CentroidCourses == "campcrane2", "campcrane", nobo1$CentroidCourses) # change "campcrane2" to "campcrane"
nobo1$CentroidCourses <- ifelse(nobo1$CentroidCourses == "campcrane1", "campcrane", nobo1$CentroidCourses) # change "campcrane2" to "campcrane"

# Also fix the names in the shapefile
OP$course <- ifelse(OP$course == "campcrane2", "campcrane", OP$course) # change "campcrane2" to "campcrane"
OP$course <- ifelse(OP$course == "campcrane1", "campcrane", OP$course) # change "campcrane2" to "campcrane"

# remove birds that are not in real courses
unique(nobo1$CentroidCourses)
nobo1 <- subset(nobo1, CentroidCourses != "other")

# make blank data.frame() to hold randoms
nobo2 <- nobo1[0,] # blank data.frame

#### The for() loop:
for(i in 1:length(unique(nobo1$Bird.ID))){
  
  # i = 100
  # i = 100 # 162.515_220688 
  # subset one bird using the subset function
  nobo_i <- subset(nobo1, Bird.ID == unique(nobo1$Bird.ID)[i]) # randomly subsetting the data and taking the 100th bird'
   
  # turn nobo_i into spatial. Used for a figure below
  nobo_i_spatial <- SpatialPoints(coords = data.frame("x" = nobo_i$x, "y" = nobo_i$y)) # convert DF to Spatial Points
  crs(nobo_i_spatial) <- CRS("+init=epsg:4326") # define CRS for the spatial points (EPSG 4326 == lat/lon WGS84 etc)
  nobo_i_spatial <- spTransform(nobo_i_spatial, crs(OP)) # make sure it matches OP shapefile: "North American Datum 1983" 
  plot(OP); plot(nobo_i_spatial, add = TRUE, col = "blue") 
  
  # isolate course_i
  course_i_sh <- subset(OP, course == nobo_i$CentroidCourses[1]) 
  plot(course_i_sh); plot(nobo_i_spatial, add = TRUE, col = "blue") 
  
  # DJ generates random points
  course_i_sh2 <- st_as_sf(course_i_sh) # convert to sf
  points1 = st_sample(course_i_sh2, size = nrow(nobo_i) * 2) # generate 2 random points for each "used" point using st_sample()
  points1 <- as_Spatial(points1) # convert back to sp
  plot(course_i_sh); plot(points1, add = TRUE, col = "red"); plot(nobo_i_spatial, add = TRUE, col = "blue") # plot to confirm it worked
  points2 <- spTransform(points1, CRS("+init=epsg:4326")) # convert to lat/long because this is what nobo1 uses
  randomcoords <- data.frame(points2@coords) # extract lat/long from the points and convert to data.frame
  # NEW: We need to add random coordinates to a dataframe that will 
  
  # Create an empty for randoms suitable for rbind()
  head(nobo1) # take a look at NOBO data.frame
  # next, make the new piece of data to be r-binded
  newrandoms <- data.frame("X" = NA, "Bird.ID" = nobo_i$Bird.ID, "ObjectID" = NA, "Date" = NA, 
                           "Observer" = NA, "Bird.Status" = NA, "Fate" = NA, 
                           "Location.Type" = "Random", "x" = randomcoords$coords.x1, 
                           "y" = randomcoords$coords.x2, "chick" = NA, "encounter" = NA,
                           "year" = NA, "ordinal" = NA, "week" = NA, "breedingseason" = NA, 
                           "breedingseasonCov" = NA, "yearBinary" = NA, "n" = nobo_i$n,
                           "CentroidCourses" = nobo_i$CentroidCourses)
  
  nobo2 <- rbind(nobo2, newrandoms)
  
  # progress bar aka DJs child 
  compl <- round(i/length(unique(nobo1$Bird.ID))*10,0)
  print(paste0("[",strrep("🐣", compl), strrep("⚪️", 10-compl),"] ", round(100*i/length(unique(nobo1$Bird.ID)),0), "% complete, bird #", i, " done"))
  }

nrow(nobo1) == nrow(nobo2)/2
nobo3 <- rbind(nobo1, nobo2)

#### END FORLOOP() ----





#View(RandomsDF)
RandomsDF1 <- RandomsDF[2:nrow(RandomsDF),] # get rid of blank row at the begining
RandomsDF1$n = NA # add blank column for "n"
RandomsDF1$chick = NA # add a blank column for chick 

nobo2 <- rbind(nobo1, RandomsDF1)
nobo2$response <- ifelse(nobo3Location.Type == "Regular", 1, 0)
unique(nobo2$Location.Type) # now I need to fix this.. 



