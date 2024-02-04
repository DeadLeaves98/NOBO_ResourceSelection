# Title: Randoms Comparison for Dark Branch  
# Author: Autumn 
# Date started: 2/2/2024
##############################
#   1) Within this code I will be generating randoms in a grid like formation across 
#   the property. The resolution to genenrate the grid formation is currently about 0.0001 which is small 
#   previouslym it was 0.0005. By clipping to each course, I then used these random points to 
#   find the average for each of our predictor variables.
#   2) Next, I will be generating randoms with a 2:1 ratio to the reals. 
#   Then, I will be extracting values for each of the predictor variables via our rasters 
#   that dj created. 
#   3) I will be creating box whisker plots for easy interpretation
#   for each of the covertypes for the randoms that were generated using the reals (2:1) 
#   and adding an abline at for the mean covertype of each course generated in part 1.
#   4) if the whiskers do not overlap the abline I increased the ratio for randoms to reals (i.e. 5:1 ratio) 
##############################


# Part 1 

# The base code ---- 
library(rgdal); library(raster); library(adehabitatHR); library(rgeos); library(sf); library(dplyr)
library(lubridate); library(stringr); library(hablar); library(AICcmodavg);  library(lme4); library(lwgeom)

# save Alber's Equal Area Conic projection
albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
OP <- readOGR("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2022/Adult data/Resource Use/shapefiles/OrtonCourses_JustTreatmentSites.shp")

# Also fix the names in the shapefile
OP$course <- ifelse(OP$course == "campcrane2", "campcrane", OP$course) # change "campcrane2" to "campcrane"
OP$course <- ifelse(OP$course == "campcrane1", "campcrane", OP$course) # change "campcrane2" to "campcrane"

plot(OP)
crs(OP)<- CRS("+init=epsg:4326") # define CRS for the spatial points (EPSG 4326 == lat/lon WGS84 etc)

#convert to spatial feature 
op_sf <- st_as_sf(OP) # convert to sf

# generate 10,000 random points within op_sf 
r1 <- raster(crs = crs(OP), ext = extent(OP), res = 0.0003) # create a raster that has the same crs as op, to the same extent as op, and small resoutino
values(r1) <- runif(ncell(r1)) 
plot(r1); plot(OP, add = T)
r2 <- data.frame(rasterToPoints(r1))
head(r2)
nrow(r2)

# cut to course and check # of randoms generated 
rands_sp <- SpatialPoints(coords = data.frame("x" = r2$x, "y" = r2$y)) # convert DF to Spatial Points
crs(rands_sp) <- CRS("+init=epsg:4326") # define CRS for the spatial points (EPSG 4326 == lat/lon WGS84 etc)
rands_sp = spTransform(rands_sp, crs(OP)) # rands sp crs = crs OP 
plot(OP);plot(rands_sp, add = TRUE) # make sure it works

db_shp = subset(OP, course == "darkbranch")
plot(db_shp) # sick it worked 

fall.within.poly <- rands_sp[db_shp,] # select for just the points that fall in our study area 
plot(fall.within.poly)

class(fall.within.poly) #spatial 
rands = as.data.frame(fall.within.poly) # makes it into a dataframe
nrow(rands) # check to see if it worked 
# with the res set to 0.0001 it produces ~ 45,000 pts 
# a res with 0.0002 gives only 8000 so I called it a day and went with more 
# and I will be sticking to this res for every course 
##############################################################################

# make it very clear it is a random within the dataframe 
rands$Location.Type = "Random" # just to make sure its in their for my own sake 

# EXTRACTING RASTER VALUES
rands_sp <- SpatialPoints(coords = data.frame("x" = rands$x, "y" = rands$y)) # convert DF to Spatial Points
crs(rands_sp) <- CRS("+init=epsg:4326") # define CRS for the spatial points (EPSG 4326 == lat/lon WGS84 etc)

# read in rasters
# hard copied into it because unsure if git can handle
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
rands_sp1 <- spTransform(rands_sp, crs(NDVI)) # transform to NAD83
plot(NDVI); plot(rands_sp1, add = TRUE)
ndv1_ex <- raster::extract(x = NDVI, y = rands_sp1)
perc_mpine_ex <- raster::extract(x = perc_mpine, y = rands_sp1)
perc_grassy_ex <- raster::extract(x = perc_grassy, y = rands_sp1)
perc_decid_ex <- raster::extract(x = perc_decid, y = rands_sp1)
perc_bf_ex <- raster::extract(x = perc_bf, y = rands_sp1)
perc_water_ex <- raster::extract(x = perc_water, y = rands_sp1)

rands_sp2 <- spTransform(rands_sp, crs(DTN_road)) # transform to pseudo-mercator
plot(DTN_road); plot(rands_sp2, add = TRUE)
DTN_road_ex <- raster::extract(x = DTN_road, y = rands_sp2)
DTN_mpine_ex <- raster::extract(x = DTN_mpine, y = rands_sp2)
DTN_grassy_ex <- raster::extract(x = DTN_grassy, y = rands_sp2)
DTN_decid_ex <- raster::extract(x = DTN_decid, y = rands_sp2)
DTN_bf_ex <- raster::extract(x = DTN_bf, y = rands_sp2)
DTN_water_ex <- raster::extract(x = DTN_water, y = rands_sp2)

# add columns to NOBO and export
rands$ndvi <- ndv1_ex
rands$perc_mpine <- perc_mpine_ex
rands$perc_grassy <- perc_grassy_ex
rands$perc_decid <- perc_decid_ex
rands$perc_bf <- perc_bf_ex
rands$perc_water <- perc_water_ex
rands$DTN_road <- DTN_road_ex
rands$DTN_mpine <- DTN_mpine_ex
rands$DTN_grassy <- DTN_grassy_ex
rands$DTN_decid <- DTN_decid_ex
rands$DTN_bf <- DTN_bf_ex
rands$DTN_water <- DTN_water_ex 

###
head(rands)
library(ggplot2); library(tidyr)
avgcover <- data.frame("ndvi" = mean(rands$ndvi), "perc_mpine" = mean(rands$perc_mpine), 
                       "perc_grassy" = mean(rands$perc_grassy), "perc_decid" = mean(rands$perc_decid), 
                       "perc_bf" = mean(rands$perc_bf), "perc_water" = mean(rands$perc_water),
                       "DTN_road" = mean(rands$DTN_road), "DTN_mpine" = mean(rands$DTN_mpine),
                       "DTN_grassy" = mean(rands$DTN_grassy), "DTN_decid" = mean(rands$DTN_decid), 
                       "DTN_bf" = mean(rands$DTN_bf), "DTN_water" = mean(rands$DTN_water))


# ALL VARIABLE MEANS 
db_rands_avg  = avgcover %>% 
  pivot_longer(c('ndvi', 'perc_mpine', 'perc_grassy', 'perc_decid', 'perc_bf', 
                 'perc_water', 'DTN_road', 'DTN_mpine', 'DTN_grassy', 'DTN_decid', 'DTN_bf', 
                 'DTN_water'), names_to = "covertype", values_to = "Average")
db_rands_avg
# write.csv(DB_rands_avg, "./DB_CoverType_AVG")

###############################################################################
###############################################################################

# Part 2
# Randoms per bird 

# Read in the "clean" csv for telemetry data from github repository
nobo1 <- read.csv("./cleaned_Data.csv")
head(nobo1)
unique(nobo1$CentroidCourses)

# split the data up by course 
DB = subset(nobo1, CentroidCourses == "darkbranch") # birds 
plot(db_shp) # course <-- as a refresher 

length(unique(DB$Bird.ID)) # 71 birds in bp which seems rather low considering every 
# season we put out like 40 collars but its fine. 

# begin generating randoms based on # of birds within course 
# make blank data.frame() to hold randoms
DB2 <- DB[0,] # blank data.frame


#### The for() loop:
for(i in 1:length(unique(DB$Bird.ID))){
  
  #i = 5; 60 locations for bird 162.282_220906    
  # subset one bird using the subset function
  db_nobo_i <- subset(DB, Bird.ID == unique(DB$Bird.ID)[i]) # randomly subsetting the data and taking the 100th bird'
  
  # turn nobo_i into spatial. Used for a figure below
  db_nobo_i_spatial <- SpatialPoints(coords = data.frame("x" = db_nobo_i$x, "y" = db_nobo_i$y)) # convert DF to Spatial Points
  crs(db_nobo_i_spatial) <- CRS("+init=epsg:4326") # define CRS for the spatial points (EPSG 4326 == lat/lon WGS84 etc)
  db_nobo_i_spatial <- spTransform(db_nobo_i_spatial, crs(db_shp)) # make sure it matches OP shapefile: "North American Datum 1983" 
  plot(db_shp); plot(db_nobo_i_spatial, add = TRUE, col = "blue") 
  
  # generates random points
  db_course_i_sh2 <- st_as_sf(db_shp) # convert to sf
  db_points1 = st_sample(db_course_i_sh2, size = nrow(db_nobo_i) * 2) # generate 2 random points for each "used" point using st_sample()
  db_points1 <- as_Spatial(db_points1) # convert back to sp
  plot(db_shp); plot(db_points1, add = TRUE, col = "red"); plot(db_nobo_i_spatial, add = TRUE, col = "blue") # plot to confirm it worked
  db_points2 <- spTransform(db_points1, CRS("+init=epsg:4326")) # convert to lat/long because this is what nobo1 uses
  randomcoords <- data.frame(db_points2@coords) # extract lat/long from the points and convert to data.frame
  # NEW: We need to add random coordinates to a dataframe that will 
  
  # generate pseudo-dates for the randoms
  random_dates <- c(db_nobo_i$Date, db_nobo_i$Date)
  
  # Create an empty for randoms suitable for rbind()
  head(DB2) # take a look at NOBO data.frame
  # next, make the new piece of data to be r-binded
  newrandoms <- data.frame("X" = NA, "Bird.ID" = db_nobo_i$Bird.ID, "ObjectID" = NA, "Date" = random_dates, 
                           "Observer" = NA, "Bird.Status" = NA, "Fate" = NA, 
                           "Location.Type" = "Random", "x" = randomcoords$coords.x1, 
                           "y" = randomcoords$coords.x2, "chick" = NA, "encounter" = NA,
                           "year" = NA, "ordinal" = NA, "week" = NA, "breedingseason" = NA, 
                           "breedingseasonCov" = NA, "yearBinary" = NA, "n" = db_nobo_i$n,
                           "CentroidCourses" = db_nobo_i$CentroidCourses)
  
  DB2 <- rbind(DB2, newrandoms)
  
  # progress bar aka DJs child 
  compl <- round(i/length(unique(DB$Bird.ID))*15,0)
  cat(paste0("\r [",strrep("🐣", compl), strrep("⚪️", 15-compl),"] ", round(100*i/length(unique(DB$Bird.ID)),0), "% complete, bird #", i, " done"))
}

DB2 # our dataframe for the randoms for db
unique(DB2$Location.Type) # random
nrow(DB2) #6112 --> this should be good. 2 * the number of originals. 3056 * 2 = 6112

###############################################################################

# now to extract all the covertype values for each point generated from the birds 

################################################################################

# part 3: COVERTYPE EXTRACTION 

# EXTRACTING RASTER VALUES
DB2_sp <- SpatialPoints(coords = data.frame("x" = DB2$x, "y" = DB2$y)) # convert DF to Spatial Points
crs(DB2_sp) <- CRS("+init=epsg:4326") # define CRS for the spatial points (EPSG 4326 == lat/lon WGS84 etc)
plot(DB2_sp)
# read in rasters
# hard copied into it because unsure if git can handle
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
DB2_sp1 <- spTransform(DB2_sp, crs(NDVI)) # transform to NAD83
plot(NDVI); plot(DB2_sp1, add = TRUE)
ndv1_ex <- raster::extract(x = NDVI, y = DB2_sp1)
perc_mpine_ex <- raster::extract(x = perc_mpine, y = DB2_sp1)
perc_grassy_ex <- raster::extract(x = perc_grassy, y = DB2_sp1)
perc_decid_ex <- raster::extract(x = perc_decid, y = DB2_sp1)
perc_bf_ex <- raster::extract(x = perc_bf, y = DB2_sp1)
perc_water_ex <- raster::extract(x = perc_water, y = DB2_sp1)

DB2_sp2 <- spTransform(DB2_sp1, crs(DTN_road)) # transform to pseudo-mercator
plot(DTN_road); plot(DB2_sp2, add = TRUE)
DTN_road_ex <- raster::extract(x = DTN_road, y = DB2_sp2)
DTN_mpine_ex <- raster::extract(x = DTN_mpine, y = DB2_sp2)
DTN_grassy_ex <- raster::extract(x = DTN_grassy, y = DB2_sp2)
DTN_decid_ex <- raster::extract(x = DTN_decid, y = DB2_sp2)
DTN_bf_ex <- raster::extract(x = DTN_bf, y = DB2_sp2)
DTN_water_ex <- raster::extract(x = DTN_water, y = DB2_sp2)

# add columns to NOBO and export
DB2$ndvi <- ndv1_ex
DB2$perc_mpine <- perc_mpine_ex
DB2$perc_grassy <- perc_grassy_ex
DB2$perc_decid <- perc_decid_ex
DB2$perc_bf <- perc_bf_ex
DB2$perc_water <- perc_water_ex
DB2$DTN_road <- DTN_road_ex
DB2$DTN_mpine <- DTN_mpine_ex
DB2$DTN_grassy <- DTN_grassy_ex
DB2$DTN_decid <- DTN_decid_ex
DB2$DTN_bf <- DTN_bf_ex
DB2$DTN_water <- DTN_water_ex 

# View(DB2)
##############################################################################

# now to compare the randoms to the covertype means per corresponding course 

##############################################################################
#### Part 4 ----

# boxwhisker plots 
head(db_rands_avg) # the mean cover type for fence cove AKA what we will be comparing 
# our randoms generated per bird to to make sure the randoms are accurately representing 
# the course 

head(DB2)
#### NDVI ----
ndvi_bw_darkbranch = boxplot(DB2$ndvi, 
                           main = "Random point distribution: NDVI in DB", 
                           ylab = "Average", 
                           xlab = "NDVI") 
abline(h = 0.544, col = "purple")

#### % mpine ----
perc_mpine_bw_darkbranch = boxplot(DB2$perc_mpine, 
                                 main = "Randoms for % mpine in DB", 
                                 ylab = "Average", 
                                 xlab = "% mpine") 
abline(h = 0.474, col = "purple")

#### % grassy ----
perc_grassy_bw_darkbranch = boxplot(DB2$perc_grassy, 
                                  main = "Randoms for % grassy in DB", 
                                  ylab = "Average", 
                                  xlab = "% grassy") 
abline(h = 0.000277, col = "purple")

#### % decid ----
perc_decid_bw_darkbranch = boxplot(DB2$perc_decid, 
                                 main = "Randoms for % decid in DB", 
                                 ylab = "Average", 
                                 xlab = "% decid") 
abline(h = 0.424, col = "purple")

#### % BF ----
perc_bf_bw_darkbranch = boxplot(DB2$perc_bf, 
                              main = "Randoms for % bf in DB", 
                              ylab = "Average", 
                              xlab = "% bf") 
abline(h = 0.00945, col = "purple")

#### % water ----
perc_water_bw_darkbranch = boxplot(DB2$perc_water, 
                                 main = "Randoms for % water in DB", 
                                 ylab = "Average", 
                                 xlab = "% water") 
abline(h = 0.00760, col = "purple") #TO NOTE: WILL BE REMOVING THIS FROM THE DATA ANYWAY 


#### dtn road  ----
dtn_road_bw_darkbranch = boxplot(DB2$DTN_road, 
                               main = "Randoms for DTN road in DB", 
                               ylab = "Average", 
                               xlab = "DTN road") 
abline(h = 37.2, col = "purple")

#### dtn mpine  ----
dtn_mpine_bw_darkbranch = boxplot(DB2$DTN_mpine, 
                                main = "Randoms for DTN mpine in DB", 
                                ylab = "Average", 
                                xlab = "DTN mpine") 
abline(h = 15.4, col = "purple")

#### dtn grassy   ----
dtn_grassy_bw_darkbranch = boxplot(DB2$DTN_grassy, 
                                 main = "Randoms for DTN grassy in DB", 
                                 ylab = "Average", 
                                 xlab = "DTN grassy") 
abline(h = 485, col = "purple")

#### dtn decid  ----
dtn_mpine_bw_darkbranch = boxplot(DB2$DTN_decid, 
                                main = "Randoms for DTN decid in DB", 
                                ylab = "Average", 
                                xlab = "DTN decid") 
abline(h = 20.8, col = "purple")

#### dtn bf  ----
dtn_bf_bw_darkbranch = boxplot(DB2$DTN_bf, 
                             main = "Randoms for DTN bf in DB", 
                             ylab = "Average", 
                             xlab = "DTN bf") 
abline(h = 108.0, col = "purple")

#### dtn water  ----
dtn_water_bw_darkbranch = boxplot(DB2$DTN_water, 
                                main = "Randoms for DTN water in DB", 
                                ylab = "Average", 
                                xlab = "DTN water") 
abline(h = 256.0, col = "purple")


# IN SUM: 
#   The randoms that are generated for bird are distributed enough to represent the actual 
#   cover types on the landscape for all except % water which will be removed anyway 


#################################################################################################

