#### Title: Generating randoms for Property Scale on a course by course basis 
#### Author: Autumn 
#### Date started: 2/1/2024
 
#Read in the data 

# Read in the "clean" csv for telemetry data from github repository
library(ggplot2); library(tidyr)
library(rgdal); library(raster); library(adehabitatHR); library(rgeos); library(sf); library(dplyr)
library(lubridate); library(stringr); library(hablar); library(AICcmodavg);  library(lme4); library(lwgeom)
nobo1 <- read.csv("./cleaned_Data.csv")

head(nobo1)
unique(nobo1$CentroidCourses)

# split the data up by course 
AC = subset(nobo1, CentroidCourses == "allenscreek")
FC = subset(nobo1, CentroidCourses == "fencecove")
BJ = subset(nobo1, CentroidCourses == "billjones")
BB = subset(nobo1, CentroidCourses == "bigbay")
DB = subset(nobo1, CentroidCourses == "darkbranch")
BP = subset(nobo1, CentroidCourses == "bluepond")
DB = subset(nobo1, CentroidCourses == "darkbranch")

length(unique(FC$Bird.ID))

# this is going to be one lengthy bitch I guess.. 

# split the property up by course 
# save Alber's Equal Area Conic projection
albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
OP <- readOGR("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2022/Adult data/Resource Use/shapefiles/OrtonCourses_JustTreatmentSites.shp")
head(OP)

# Also fix the names in the shapefile
OP$course <- ifelse(OP$course == "campcrane2", "campcrane", OP$course) # change "campcrane2" to "campcrane"
OP$course <- ifelse(OP$course == "campcrane1", "campcrane", OP$course) # change "campcrane2" to "campcrane"

# subset property by course 
allens_shp = subset(OP, course == "allenscreek")
fence_shp = subset(OP, course == "fencecove")
bill_shp = subset(OP, course == "billjones")
big_shp = subset(OP, course == "bigbay")
camp_shp = subset(OP, course == "campcrane")
dark_shp = subset(OP, course == "darkbranch")
blue_shp = subset(OP, course == "bluepond")

# plot the courses to see if it worked 
plot(blue_shp)
plot(dark_shp)
plot(big_shp)
plot(allens_shp)
plot(fence_shp)
plot(camp_shp)

# now that the data and courses are seperated based off course I want to most likely 
# run 7 forloops... one for every course... that will generate the randoms seperately 
# I was thinking about running it based off course and then subsetting after for course but this would 
# ensure a quick easy fix if I need to generate more randoms later on for one or two specific courses 

# Fence Cove ---- 
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
  fc_points1 = st_sample(fc_course_i_sh2, size = nrow(fc_nobo_i) * 3) # generate 2 random points for each "used" point using st_sample()
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
nrow(FC2) #6148

 #############################################################################
# fyi: there is probs an easier way 
# First, I am going to go through fence cove first and generate and test the randoms all together 
# for each course. This way it is easier to follow in the outline and it stays 
# relatively coherent throughout 
##############################################################################

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

## BOXPLOTS ----
summary(FC2)
fig1_prop_perc = boxplot(FC2[,21:26])
fig1_prop_dtn = boxplot(FC2[,27:32])

# for reference --> what we will be comparing the randoms at each scale against 
rands_avg = read.csv("./covertype_averages.csv")
rands_avg # the 10,000 ish randomly generated points to compare 

#### NDVI ----
ndvi_bw_fencecove = boxplot(FC2$ndvi, 
                           main = "Random point distribution: NDVI in FC", 
                           ylab = "Average", 
                           xlab = "NDVI") 
abline(h = 0.491, col = "purple")

#### % mpine ----
perc_mpine_bw_fencecove = boxplot(FC2$perc_mpine, 
                                 main = "Randoms for % mpine in FC", 
                                 ylab = "Average", 
                                 xlab = "% mpine") 
abline(h = 0.529, col = "purple")

#### % grassy ----
perc_grassy_bw_fencecove = boxplot(FC2$perc_grassy, 
                                  main = "Randoms for % grassy in FC", 
                                  ylab = "Average", 
                                  xlab = "% grassy") 
abline(h = 0.0784, col = "purple")

#### % decid ----
perc_decid_bw_fencecove = boxplot(FC2$perc_decid, 
                                 main = "Randoms for % decid in FC", 
                                 ylab = "Average", 
                                 xlab = "% decid") 
abline(h = 0.269, col = "purple")

#### % BF ----
perc_bf_bw_fencecove = boxplot(FC2$perc_bf, 
                              main = "Randoms for % bf in FC", 
                              ylab = "Average", 
                              xlab = "% bf") 
abline(h = 0.0371, col = "purple")

#### % water ----
perc_water_bw_fencecove = boxplot(FC2$perc_water, 
                                 main = "Randoms for % water in fC", 
                                 ylab = "Average", 
                                 xlab = "% water") 
abline(h = 0.0371, col = "purple")

#### dtn road  ----
dtn_road_bw_fencecove = boxplot(FC2$DTN_road, 
                               main = "Randoms for DTN road in FC", 
                               ylab = "Average", 
                               xlab = "DTN road") 
abline(h = 30.4, col = "purple")

#### dtn mpine  ----
dtn_mpine_bw_fencecove = boxplot(FC2$DTN_mpine, 
                                main = "Randoms for DTN mpine in FC", 
                                ylab = "Average", 
                                xlab = "DTN mpine") 
abline(h = 12.4, col = "purple")

#### dtn grassy   ----
dtn_grassy_bw_fencecove = boxplot(FC2$DTN_grassy, 
                                 main = "Randoms for DTN grassy in FC", 
                                 ylab = "Average", 
                                 xlab = "DTN grassy") 
abline(h = 238, col = "purple")

#### dtn decid  ----
dtn_mpine_bw_fencecove = boxplot(FC2$DTN_decid, 
                                main = "Randoms for DTN decid in FC", 
                                ylab = "Average", 
                                xlab = "DTN decid") 
abline(h = 28.9, col = "purple")

#### dtn bf  ----
dtn_bf_bw_fencecove = boxplot(FC2$DTN_bf, 
                             main = "Randoms for DTN bf in FC", 
                             ylab = "Average", 
                             xlab = "DTN bf") 
abline(h = 84.1, col = "purple")

#### dtn water  ----
dtn_water_bw_fencecove = boxplot(FC2$DTN_water, 
                                main = "Randoms for DTN water in FC", 
                                ylab = "Average", 
                                xlab = "DTN water") 
abline(h = 149.0, col = "purple")





