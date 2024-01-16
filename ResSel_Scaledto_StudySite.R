# Title: Resource Selection _ Real vs Random _ Scale to Course ----
# Author: Autumn Randall 
# PI: Dr. DJ McNeil
# Date finished: 
# Date last Edited: 1/15/2024 @ noon 
##################################################

# Read in the "clean" csv for telemetry data from github repository
library(rgdal); library(raster); library(adehabitatHR); library(rgeos); library(sf); library(dplyr)
library(lubridate); library(stringr); library(hablar); library(AICcmodavg);  library(lme4); library(lwgeom)
nobo1 <- read.csv("./cleaned_NOBO_telem.csv")
OP <- readOGR("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2022/Adult data/Resource Use/shapefiles/OrtonCourses_JustTreatmentSites.shp")

#############################
############################# Part 1: StudySite FORLOOP()----

# save Alber's Equal Area Conic projection
albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

# blankDF
RandomsDF <- data.frame("X" = NA, "Bird.ID" = NA, "ObjectID" = NA, "Date" = NA, "Observer" = NA, 
                        "Bird.Status" = NA, "Fate" = NA, "Location.Type" = "Random", "x" = NA, "y" = NA, "encounter" = NA,
                        "year" = NA, "ordinal" = NA, "week" = NA, "breedingseason" = NA, "breedingseasonCov" = NA, "yearBinary" = NA)

# make blank data.frame() to hold randoms
nobo2 <- nobo1[0,] # blank data.frame

#### The for() loop:
for(i in 1:length(unique(nobo1$Bird.ID))){
  

  # i = 100 # 162.515_220688 
  # subset one bird using the subset function
  nobo_i <- subset(nobo1, Bird.ID == unique(nobo1$Bird.ID)[i]) # randomly subsetting the data and taking the 100th bird'
  
  # turn nobo_i into spatial. Used for a figure below
  nobo_i_spatial <- SpatialPoints(coords = data.frame("x" = nobo_i$x, "y" = nobo_i$y)) # convert DF to Spatial Points
  crs(nobo_i_spatial) <- CRS("+init=epsg:4326") # define CRS for the spatial points (EPSG 4326 == lat/lon WGS84 etc)
  nobo_i_spatial <- spTransform(nobo_i_spatial, crs(OP)) # make sure it matches OP shapefile: "North American Datum 1983" 
  plot(OP); plot(nobo_i_spatial, add = TRUE, col = "blue") 
  
  # isolate course_i
    # course_i_sh <- subset(OP, course == nobo_i$CentroidCourses[1]) 
    # plot(course_i_sh); plot(nobo_i_spatial, add = TRUE, col = "blue") 
  
  # DJ generates random points
  OP_i_sf <- st_as_sf(OP) # convert to sf
  points1 = st_sample(OP_i_sf, size = nrow(nobo_i) * 2) # generate 2 random points for each "used" point using st_sample()
  points1 <- as_Spatial(points1) # convert back to sp
  plot(OP); plot(points1, add = TRUE, col = "red"); plot(nobo_i_spatial, add = TRUE, col = "blue") # plot to confirm it worked
  points2 <- spTransform(points1, CRS("+init=epsg:4326")) # convert to lat/long because this is what nobo1 uses
  randomcoords <- data.frame(points2@coords) # extract lat/long from the points and convert to data.frame
  # NEW: We need to add random coordinates to a dataframe that will 
  
  # generate pseudo-dates for the randoms
  random_dates <- c(nobo_i$Date, nobo_i$Date)
  
  # Create an empty for randoms suitable for rbind()
  head(nobo1) # take a look at NOBO data.frame
  # next, make the new piece of data to be r-binded
  newrandoms <- data.frame("X" = NA, "Bird.ID" = nobo_i$Bird.ID, "ObjectID" = NA, "Date" = random_dates, 
                           "Observer" = NA, "Bird.Status" = NA, "Fate" = NA, 
                           "Location.Type" = "Random", "x" = randomcoords$coords.x1, 
                           "y" = randomcoords$coords.x2, "chick" = NA, "encounter" = NA,
                           "year" = NA, "ordinal" = NA, "week" = NA, "breedingseason" = NA, 
                           "breedingseasonCov" = NA, "yearBinary" = NA, "n" = nobo_i$n,
                           "CentroidCourses" = nobo_i$CentroidCourses)
  
  nobo2 <- rbind(nobo2, newrandoms)
  
  # progress bar aka DJs child 
  compl <- round(i/length(unique(nobo1$Bird.ID))*15,0)
  cat(paste0("\r [",strrep("🐣", compl), strrep("⚪️", 15-compl),"] ", round(100*i/length(unique(nobo1$Bird.ID)),0), "% complete, bird #", i, " done"))
}


nrow(nobo1) == nrow(nobo2)/2

nobo3 <- rbind(nobo1, nobo2) # if there is 71k pts we should be gucci 

#### END FORLOOP() ----

View(nobo3)

nobo3$response <- ifelse(nobo3$Location.Type == "Regular", 1, 0)
unique(nobo3$Location.Type) # now I need to fix this.. 
names(nobo3)
