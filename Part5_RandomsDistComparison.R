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
points1 = st_sample(op_sf, size = 10000)
plot(points1, add = TRUE) # plot to check 

points1 <- as_Spatial(points1) # convert back to sp
points2 <- spTransform(points1, CRS("+init=epsg:4326")) # convert to lat/long because this is what nobo1 uses
op_randcoords <- data.frame(points2@coords) # extract lat/long from the points and convert to data.frame

# create a dataframe to hold the randoms 
rands <- data.frame("Location.Type" = "Random", "x" = op_randcoords$coords.x1, 
                    "y" = op_randcoords$coords.x2)

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

perc_cov = avgcover %>% 
  pivot_longer(c('ndvi', 'perc_mpine', 'perc_grassy', 'perc_decid', 'perc_bf', 'perc_water'), names_to = "covertype", values_to = "Average")
randoms_perc_fig1 = ggplot(perc_cov, aes(x=covertype, y=Average)) + 
  geom_bar(stat = "identity")

DTN = avgcover %>% 
  pivot_longer(c('DTN_road', 'DTN_mpine', 'DTN_grassy', 'DTN_decid', 'DTN_bf', 
                 'DTN_water'), names_to = "covertype", values_to = "Average")
randoms_dtn_fig2 = ggplot(DTN, aes(x=covertype, y=Average)) + 
  geom_bar(stat = "identity")


################################################################################
################################################################################

prop = read.csv("./ResSelData_Property.csv") # draws in the file used in the property scaled data frame 
head(prop)
unique(prop$response)
nrow(prop)

prop_rands = subset(prop, response == 0)
nrow(prop_rands)# 48374 GUCCI 

#create an empty dataframe for this bad boi 
prop_avgcover <- data.frame("ndvi" = mean(prop_rands$ndvi), "perc_mpine" = mean(prop_rands$perc_mpine), 
                       "perc_grassy" = mean(prop_rands$perc_grassy), "perc_decid" = mean(prop_rands$perc_decid), 
                       "perc_bf" = mean(prop_rands$perc_bf), "perc_water" = mean(prop_rands$perc_water),
                       "DTN_road" = mean(prop_rands$DTN_road), "DTN_mpine" = mean(prop_rands$DTN_mpine),
                       "DTN_grassy" = mean(prop_rands$DTN_grassy), "DTN_decid" = mean(prop_rands$DTN_decid), 
                       "DTN_bf" = mean(prop_rands$DTN_bf), "DTN_water" = mean(prop_rands$DTN_water))

#this is creating a dataframe with columns "Covertype" and "Average 
prop_avg  = prop_avgcover %>% 
  pivot_longer(c('ndvi', 'perc_mpine', 'perc_grassy', 'perc_decid', 'perc_bf', 
                 'perc_water', 'DTN_road', 'DTN_mpine', 'DTN_grassy', 'DTN_decid', 'DTN_bf', 
                 'DTN_water'), names_to = "covertype", values_to = "Average")


# create a plot for % cover (and splits the dataframe up to make it a little easier to see)
prop_perc_cov = prop_avgcover %>% 
  pivot_longer(c('ndvi', 'perc_mpine', 'perc_grassy', 'perc_decid', 'perc_bf', 'perc_water'), names_to = "covertype", values_to = "Average")
randoms_PROP_perc_fig1 = ggplot(prop_perc_cov, aes(x=covertype, y=Average)) + 
  geom_bar(stat = "identity")


# create a plot for dtn (and splits the dataframe up to make it a little easier to see)
prop_DTN = prop_avgcover %>% 
  pivot_longer(c('DTN_road', 'DTN_mpine', 'DTN_grassy', 'DTN_decid', 'DTN_bf', 
                 'DTN_water'), names_to = "covertype", values_to = "Average")
randoms_PROP_dtn_fig2 = ggplot(prop_DTN, aes(x=covertype, y=Average)) + 
  geom_bar(stat = "identity")

################################################################################
################################################################################
course = read.csv("./ResSelData_Course.csv") # draws in the file used in the property scaled data frame 
head(course)
unique(course$response)
nrow(course)

course_rands = subset(course, response == 0)
nrow(course_rands)# 48374 GUCCI 

#create an empty dataframe for this bad boi 
course_avgcover <- data.frame("ndvi" = mean(course_rands$ndvi), "perc_mpine" = mean(course_rands$perc_mpine), 
                            "perc_grassy" = mean(course_rands$perc_grassy), "perc_decid" = mean(course_rands$perc_decid), 
                            "perc_bf" = mean(course_rands$perc_bf), "perc_water" = mean(course_rands$perc_water),
                            "DTN_road" = mean(course_rands$DTN_road), "DTN_mpine" = mean(course_rands$DTN_mpine),
                            "DTN_grassy" = mean(course_rands$DTN_grassy), "DTN_decid" = mean(course_rands$DTN_decid), 
                            "DTN_bf" = mean(course_rands$DTN_bf), "DTN_water" = mean(course_rands$DTN_water))

#this is creating a dataframe with columns "Covertype" and "Average 
course_avg  = course_avgcover %>% 
  pivot_longer(c('ndvi', 'perc_mpine', 'perc_grassy', 'perc_decid', 'perc_bf', 
                 'perc_water', 'DTN_road', 'DTN_mpine', 'DTN_grassy', 'DTN_decid', 'DTN_bf', 
                 'DTN_water'), names_to = "covertype", values_to = "Average")


# create a plot for % cover (and splits the dataframe up to make it a little easier to see)
course_perc_cov = course_avgcover %>% 
  pivot_longer(c('ndvi', 'perc_mpine', 'perc_grassy', 'perc_decid', 'perc_bf', 'perc_water'), names_to = "covertype", values_to = "Average")
randoms_course_perc_fig1 = ggplot(course_perc_cov, aes(x=covertype, y=Average)) + 
  geom_bar(stat = "identity")


# create a plot for dtn (and splits the dataframe up to make it a little easier to see)
course_DTN = course_avgcover %>% 
  pivot_longer(c('DTN_road', 'DTN_mpine', 'DTN_grassy', 'DTN_decid', 'DTN_bf', 
                 'DTN_water'), names_to = "covertype", values_to = "Average")
randoms_course_dtn_fig2 = ggplot(course_DTN, aes(x=covertype, y=Average)) + 
  geom_bar(stat = "identity")


###############################################################################
###############################################################################

mcp = read.csv("./ResSelData_MCP.csv") # draws in the file used in the property scaled data frame 
head(mcp)
unique(mcp$response)
nrow(mcp)

mcp_rands = subset(mcp, response == 0)
nrow(mcp_rands)# 48374 GUCCI 

#create an empty dataframe for this bad boi 
mcp_avgcover <- data.frame("ndvi" = mean(mcp_rands$ndvi), "perc_mpine" = mean(mcp_rands$perc_mpine), 
                              "perc_grassy" = mean(mcp_rands$perc_grassy), "perc_decid" = mean(mcp_rands$perc_decid), 
                              "perc_bf" = mean(mcp_rands$perc_bf), "perc_water" = mean(mcp_rands$perc_water),
                              "DTN_road" = mean(mcp_rands$DTN_road), "DTN_mpine" = mean(mcp_rands$DTN_mpine),
                              "DTN_grassy" = mean(mcp_rands$DTN_grassy), "DTN_decid" = mean(mcp_rands$DTN_decid), 
                              "DTN_bf" = mean(mcp_rands$DTN_bf), "DTN_water" = mean(mcp_rands$DTN_water))

#this is creating a dataframe with columns "Covertype" and "Average 
mcp_avg  = mcp_avgcover %>% 
  pivot_longer(c('ndvi', 'perc_mpine', 'perc_grassy', 'perc_decid', 'perc_bf', 
                 'perc_water', 'DTN_road', 'DTN_mpine', 'DTN_grassy', 'DTN_decid', 'DTN_bf', 
                 'DTN_water'), names_to = "covertype", values_to = "Average")


# create a plot for % cover (and splits the dataframe up to make it a little easier to see)
mcp_perc_cov = mcp_avgcover %>% 
  pivot_longer(c('ndvi', 'perc_mpine', 'perc_grassy', 'perc_decid', 'perc_bf', 'perc_water'), names_to = "covertype", values_to = "Average")
randoms_mcp_perc_fig1 = ggplot(mcp_perc_cov, aes(x=covertype, y=Average)) + 
  geom_bar(stat = "identity")


# create a plot for dtn (and splits the dataframe up to make it a little easier to see)
mcp_DTN = mcp_avgcover %>% 
  pivot_longer(c('DTN_road', 'DTN_mpine', 'DTN_grassy', 'DTN_decid', 'DTN_bf', 
                 'DTN_water'), names_to = "covertype", values_to = "Average")
randoms_mcp_dtn_fig2 = ggplot(mcp_DTN, aes(x=covertype, y=Average)) + 
  geom_bar(stat = "identity")

