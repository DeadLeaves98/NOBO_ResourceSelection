# Title: Randoms COmarison for fence cove 
# Author: Autumn 
# Date started: 2/2/2024
##############################
#   1) Within this code I will be generating randoms in a grid like formation across 
#   fence cove for a relatively even distribution. I will be taking the mean value 
#   for each cover type. 
#   2) Next, I will be generating randoms with a 2:1 ratio to the reals. 
#   Then, I will be extracting values for each of the covertype via our rasters 
#   that dj created. 
#   3) I will be creating box whisker plots for easy interpretation
#   for each of the covertypes for the randoms that were generated using the reals (2:1) 
#   and adding an abline at for the mean covertype of each course generated in part 1.
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
r1 <- raster(crs = crs(OP), ext = extent(OP), res = 0.0001) # create a raster that has the same crs as op, to the same extent as op, and small resoutino
values(r1) <- runif(ncell(r1)) 
plot(r1); plot(OP, add = T)
r2 <- data.frame(rasterToPoints(r1))
head(r2)
nrow(r2)
###############################################################################
# cut to course and check # of randoms generated 

rands_sp <- SpatialPoints(coords = data.frame("x" = r2$x, "y" = r2$y)) # convert DF to Spatial Points
crs(rands_sp) <- CRS("+init=epsg:4326") # define CRS for the spatial points (EPSG 4326 == lat/lon WGS84 etc)
rands_sp = spTransform(rands_sp, crs(OP)) # rands sp crs = crs OP 
plot(OP);plot(rands_sp, add = TRUE) # make sure it works

fc_shp = subset(OP, course == "fencecove")
plot(fc_shp) # sick it worked 

fall.within.poly <- rands_sp[fc_shp,] # select for just the points that fall in our study area 
plot(fall.within.poly)

class(fall.within.poly) #spatial 
rands = as.data.frame(fall.within.poly) # makes it into a dataframe
nrow(rands) # check to see if it worked 
# with the res set to 0.0001 it produces ~ 32,000 pts 
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
FC_rands_avg  = avgcover %>% 
  pivot_longer(c('ndvi', 'perc_mpine', 'perc_grassy', 'perc_decid', 'perc_bf', 
                 'perc_water', 'DTN_road', 'DTN_mpine', 'DTN_grassy', 'DTN_decid', 'DTN_bf', 
                 'DTN_water'), names_to = "covertype", values_to = "Average")

head(FC_rands_avg)

# write.csv(FC_rands_avg, "./FC_CoverType_AVG")

###############################################################################
###############################################################################

# Part 2
# Randoms per bird 

# Read in the "clean" csv for telemetry data from github repository
nobo1 <- read.csv("./cleaned_Data.csv")
head(nobo1)
unique(nobo1$CentroidCourses)

# split the data up by course 
FC = subset(nobo1, CentroidCourses == "fencecove") # birds 
plot(fc_shp) # course <-- as a refresher 

length(unique(FC$Bird.ID)) # 67 birds in FC which seems rather low considering every 
# season we put out like 40 collars but its fine. 

# begin generating randoms based on # of birds within course 
# make blank data.frame() to hold randoms
FC2 <- FC[0,] # blank data.frame


#### The for() loop:
for(i in 1:length(unique(FC$Bird.ID))){
  
  #i = 19; 57 locations for bird 162.805_231014
  # subset one bird using the subset function
  fc_nobo_i <- subset(FC, Bird.ID == unique(FC$Bird.ID)[i]) # randomly subsetting the data and taking the 100th bird'
  
  # turn nobo_i into spatial. Used for a figure below
  fc_nobo_i_spatial <- SpatialPoints(coords = data.frame("x" = fc_nobo_i$x, "y" = fc_nobo_i$y)) # convert DF to Spatial Points
  crs(fc_nobo_i_spatial) <- CRS("+init=epsg:4326") # define CRS for the spatial points (EPSG 4326 == lat/lon WGS84 etc)
  fc_nobo_i_spatial <- spTransform(fc_nobo_i_spatial, crs(fence_shp)) # make sure it matches OP shapefile: "North American Datum 1983" 
  plot(fence_shp); plot(fc_nobo_i_spatial, add = TRUE, col = "blue") 
  
  # generates random points
  fc_course_i_sh2 <- st_as_sf(fence_shp) # convert to sf
  fc_points1 = st_sample(fc_course_i_sh2, size = nrow(fc_nobo_i) * 2) # generate 2 random points for each "used" point using st_sample()
  fc_points1 <- as_Spatial(fc_points1) # convert back to sp
  plot(fence_shp); plot(fc_points1, add = TRUE, col = "red"); plot(fc_nobo_i_spatial, add = TRUE, col = "blue") # plot to confirm it worked
  fc_points2 <- spTransform(fc_points1, CRS("+init=epsg:4326")) # convert to lat/long because this is what nobo1 uses
  randomcoords <- data.frame(fc_points2@coords) # extract lat/long from the points and convert to data.frame
  # NEW: We need to add random coordinates to a dataframe that will 
  
  # generate pseudo-dates for the randoms
  random_dates <- c(fc_nobo_i$Date, fc_nobo_i$Date)
  
  # Create an empty for randoms suitable for rbind()
  head(FC2) # take a look at NOBO data.frame
  # next, make the new piece of data to be r-binded
  newrandoms <- data.frame("X" = NA, "Bird.ID" = fc_nobo_i$Bird.ID, "ObjectID" = NA, "Date" = random_dates, 
                           "Observer" = NA, "Bird.Status" = NA, "Fate" = NA, 
                           "Location.Type" = "Random", "x" = randomcoords$coords.x1, 
                           "y" = randomcoords$coords.x2, "chick" = NA, "encounter" = NA,
                           "year" = NA, "ordinal" = NA, "week" = NA, "breedingseason" = NA, 
                           "breedingseasonCov" = NA, "yearBinary" = NA, "n" = fc_nobo_i$n,
                           "CentroidCourses" = fc_nobo_i$CentroidCourses)
  
  FC2 <- rbind(FC2, newrandoms)
  
  # progress bar aka DJs child 
  compl <- round(i/length(unique(FC$Bird.ID))*15,0)
  cat(paste0("\r [",strrep("🐣", compl), strrep("⚪️", 15-compl),"] ", round(100*i/length(unique(FC$Bird.ID)),0), "% complete, bird #", i, " done"))
}

FC2 # our dataframe for the randoms for fence cove 
unique(FC2$Location.Type) # random
nrow(FC2) #6148 --> this should be good. 2 * the number of originals. 3074 * 2 = 6148

###############################################################################

# now to extract all the covertype values for each point generated from the birds 

################################################################################

# part 3: COVERTYPE EXTRACTION 

# EXTRACTING RASTER VALUES
fc2_sp <- SpatialPoints(coords = data.frame("x" = FC2$x, "y" = FC2$y)) # convert DF to Spatial Points
crs(fc2_sp) <- CRS("+init=epsg:4326") # define CRS for the spatial points (EPSG 4326 == lat/lon WGS84 etc)
plot(fc2_sp)
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
fc2_sp1 <- spTransform(fc2_sp, crs(NDVI)) # transform to NAD83
plot(NDVI); plot(fc2_sp1, add = TRUE)
ndv1_ex <- raster::extract(x = NDVI, y = fc2_sp1)
perc_mpine_ex <- raster::extract(x = perc_mpine, y = fc2_sp1)
perc_grassy_ex <- raster::extract(x = perc_grassy, y = fc2_sp1)
perc_decid_ex <- raster::extract(x = perc_decid, y = fc2_sp1)
perc_bf_ex <- raster::extract(x = perc_bf, y = fc2_sp1)
perc_water_ex <- raster::extract(x = perc_water, y = fc2_sp1)

fc2_sp2 <- spTransform(fc2_sp, crs(DTN_road)) # transform to pseudo-mercator
plot(DTN_road); plot(fc2_sp2, add = TRUE)
DTN_road_ex <- raster::extract(x = DTN_road, y = fc2_sp2)
DTN_mpine_ex <- raster::extract(x = DTN_mpine, y = fc2_sp2)
DTN_grassy_ex <- raster::extract(x = DTN_grassy, y = fc2_sp2)
DTN_decid_ex <- raster::extract(x = DTN_decid, y = fc2_sp2)
DTN_bf_ex <- raster::extract(x = DTN_bf, y = fc2_sp2)
DTN_water_ex <- raster::extract(x = DTN_water, y = fc2_sp2)

# add columns to NOBO and export
FC2$ndvi <- ndv1_ex
FC2$perc_mpine <- perc_mpine_ex
FC2$perc_grassy <- perc_grassy_ex
FC2$perc_decid <- perc_decid_ex
FC2$perc_bf <- perc_bf_ex
FC2$perc_water <- perc_water_ex
FC2$DTN_road <- DTN_road_ex
FC2$DTN_mpine <- DTN_mpine_ex
FC2$DTN_grassy <- DTN_grassy_ex
FC2$DTN_decid <- DTN_decid_ex
FC2$DTN_bf <- DTN_bf_ex
FC2$DTN_water <- DTN_water_ex 

# View(FC2)
##############################################################################

# now to compare the randoms to the covertype means per corresponding course 

##############################################################################
#### Part 4 ----

# boxwhisker plots 
head(FC_rands_avg) # the mean cover type for fence cove AKA what we will be comparing 
# our randoms generated per bird to to make sure the randoms are accurately representing 
# the course 

head(FC2)
#### NDVI ----
ndvi_bw_fencecove = boxplot(FC2$ndvi, 
                            main = "Random point distribution: NDVI in FC", 
                            ylab = "Average", 
                            xlab = "NDVI") 
abline(h = 0.463, col = "purple")

#### % mpine ----
perc_mpine_bw_fencecove = boxplot(FC2$perc_mpine, 
                                  main = "Randoms for % mpine in FC", 
                                  ylab = "Average", 
                                  xlab = "% mpine") 
abline(h = 0.432, col = "purple")

#### % grassy ----
perc_grassy_bw_fencecove = boxplot(FC2$perc_grassy, 
                                   main = "Randoms for % grassy in FC", 
                                   ylab = "Average", 
                                   xlab = "% grassy") 
abline(h = 0.267, col = "purple")

#### % decid ----
perc_decid_bw_fencecove = boxplot(FC2$perc_decid, 
                                  main = "Randoms for % decid in FC", 
                                  ylab = "Average", 
                                  xlab = "% decid") 
abline(h = 0.188, col = "purple")

#### % BF ----
perc_bf_bw_fencecove = boxplot(FC2$perc_bf, 
                               main = "Randoms for % bf in FC", 
                               ylab = "Average", 
                               xlab = "% bf") 
abline(h = 0.0744, col = "purple")

#### % water ----
perc_water_bw_fencecove = boxplot(FC2$perc_water, 
                                  main = "Randoms for % water in fC", 
                                  ylab = "Average", 
                                  xlab = "% water") 
abline(h = 0.0197, col = "purple") #TO NOTE: WILL BE REMOVING THIS FROM THE DATA ANYWAY 


#### dtn road  ----
dtn_road_bw_fencecove = boxplot(FC2$DTN_road, 
                                main = "Randoms for DTN road in FC", 
                                ylab = "Average", 
                                xlab = "DTN road") 
abline(h = 26.8, col = "purple")

#### dtn mpine  ----
dtn_mpine_bw_fencecove = boxplot(FC2$DTN_mpine, 
                                 main = "Randoms for DTN mpine in FC", 
                                 ylab = "Average", 
                                 xlab = "DTN mpine") 
abline(h = 11.8, col = "purple")

#### dtn grassy   ----
dtn_grassy_bw_fencecove = boxplot(FC2$DTN_grassy, 
                                  main = "Randoms for DTN grassy in FC", 
                                  ylab = "Average", 
                                  xlab = "DTN grassy") 
abline(h = 32.6, col = "purple")

#### dtn decid  ----
dtn_mpine_bw_fencecove = boxplot(FC2$DTN_decid, 
                                 main = "Randoms for DTN decid in FC", 
                                 ylab = "Average", 
                                 xlab = "DTN decid") 
abline(h = 30.9, col = "purple")

#### dtn bf  ----
dtn_bf_bw_fencecove = boxplot(FC2$DTN_bf, 
                              main = "Randoms for DTN bf in FC", 
                              ylab = "Average", 
                              xlab = "DTN bf") 
abline(h = 46.6, col = "purple")

#### dtn water  ----
dtn_water_bw_fencecove = boxplot(FC2$DTN_water, 
                                 main = "Randoms for DTN water in FC", 
                                 ylab = "Average", 
                                 xlab = "DTN water") 
abline(h = 126.0, col = "purple")


# IN SUM: 
#   The randoms that are generated for bird are distributed enough to represent the actual 
#   cover types on the landscape for all except % water which will be removed anyway 


#################################################################################################






