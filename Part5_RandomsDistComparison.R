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
r1 <- raster(crs = crs(OP), ext = extent(OP), res = 0.0005) # create a raster that has the same crs as op, to the same extent as op, and small resoutino
values(r1) <- runif(ncell(r1)) 
plot(r1); plot(OP, add = T)
r2 <- data.frame(rasterToPoints(r1))
head(r2)
nrow(r2)

# EXTRACTING RASTER VALUES
rands_sp <- SpatialPoints(coords = data.frame("x" = r2$x, "y" = r2$y)) # convert DF to Spatial Points
crs(rands_sp) <- CRS("+init=epsg:4326") # define CRS for the spatial points (EPSG 4326 == lat/lon WGS84 etc)
rands_sp = spTransform(rands_sp, crs(OP)) # rands sp crs = crs OP 
plot(OP);plot(rands_sp, add = TRUE) # make sure it works

fall.within.poly <- rands_sp[OP,] # select for just the points that fall in our study area 
plot(fall.within.poly)

class(fall.within.poly) #spatial 
rands = as.data.frame(fall.within.poly) # makes it into a dataframe
nrow(rands) # check to see if it worked 
rands$Location.Type = "Random" # just to make sure its in their for my own sake 

head(rands)
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


# I want to split this up into 2 datasets for observational convenience 
# DTN and percent both have vastly different averages 
# maybe this is wrong, IDK, but this will allow comparison to be a bit easier 

# ALL VARIABLES 
rands_avg  = avgcover %>% 
  pivot_longer(c('ndvi', 'perc_mpine', 'perc_grassy', 'perc_decid', 'perc_bf', 
                 'perc_water', 'DTN_road', 'DTN_mpine', 'DTN_grassy', 'DTN_decid', 'DTN_bf', 
                 'DTN_water'), names_to = "covertype", values_to = "Average")

# perc and ndvi 
#perc_cov = avgcover %>% 
#  pivot_longer(c('ndvi', 'perc_mpine', 'perc_grassy', 'perc_decid', 'perc_bf', 'perc_water'), names_to = "covertype", values_to = "Average")
#randoms_perc_fig1 = ggplot(perc_cov, aes(x=covertype, y=Average)) + 
# geom_bar(stat = "identity")
# dtn 
#DTN = avgcover %>% 
#  pivot_longer(c('DTN_road', 'DTN_mpine', 'DTN_grassy', 'DTN_decid', 'DTN_bf', 
#                'DTN_water'), names_to = "covertype", values_to = "Average")
#randoms_dtn_fig2 = ggplot(DTN, aes(x=covertype, y=Average)) + 
# geom_bar(stat = "identity")
#

################################################################################
################################################################################
########## PROPERTY ----
prop = read.csv("./ResSelData_Property.csv") # draws in the file used in the property scaled data frame 
head(prop)
unique(prop$response)
nrow(prop)
prop_rands = subset(prop, response == 0) # only include randoms 
nrow(prop_rands)# 48374 GUCCI 

## BOXPLOTS PROP. ----
head(prop_rands)

summary(prop_rands)
fig1_prop_perc = boxplot(prop_rands[,11:16])
fig1_prop_dtn = boxplot(prop_rands[,17:22])

# for reference --> what we will be comparing the randoms at each scale against 
rands_avg # the 10,000 ish randomly generated points to compare 

#### NDVI ----
ndvi_bw_property = boxplot(prop_rands$ndvi, 
            main = "Random point distribution: NDVI at the property level", 
            ylab = "Average", 
            xlab = "NDVI") 
abline(h = 0.491, col = "purple")

# I am sticking to these simple plots because they get the point across without 
# me absolutely suffering 

#### % mpine ----
perc_mpine_bw_property = boxplot(prop_rands$perc_mpine, 
            main = "Randoms for % mpine at Property Level", 
            ylab = "Average", 
            xlab = "% mpine") 
abline(h = 0.529, col = "purple")

#### % grassy ----
perc_grassy_bw_property = boxplot(prop_rands$perc_grassy, 
                                 main = "Randoms for % grassy at Property Level", 
                                 ylab = "Average", 
                                 xlab = "% grassy") 
abline(h = 0.0784, col = "purple")

#### % decid ----
perc_decid_bw_property = boxplot(prop_rands$perc_decid, 
                                  main = "Randoms for % decid at Property Level", 
                                  ylab = "Average", 
                                  xlab = "% decid") 
abline(h = 0.269, col = "purple")

#### % BF ----
perc_bf_bw_property = boxplot(prop_rands$perc_bf, 
                                  main = "Randoms for % bf at Property Level", 
                                  ylab = "Average", 
                                  xlab = "% bf") 
abline(h = 0.0371, col = "purple")

#### % water ----
perc_water_bw_property = boxplot(prop_rands$perc_water, 
                              main = "Randoms for % water at Property Level", 
                              ylab = "Average", 
                              xlab = "% water") 
abline(h = 0.0371, col = "purple")

#### dtn road  ----
dtn_road_bw_property = boxplot(prop_rands$DTN_road, 
                                 main = "Randoms for DTN road at Property Level", 
                                 ylab = "Average", 
                                 xlab = "DTN road") 
abline(h = 30.4, col = "purple")

#### dtn mpine  ----
dtn_mpine_bw_property = boxplot(prop_rands$DTN_mpine, 
                               main = "Randoms for DTN mpine at Property Level", 
                               ylab = "Average", 
                               xlab = "DTN mpine") 
abline(h = 12.4, col = "purple")

#### dtn grassy   ----
dtn_grassy_bw_property = boxplot(prop_rands$DTN_grassy, 
                                main = "Randoms for DTN grassy at Property Level", 
                                ylab = "Average", 
                                xlab = "DTN grassy") 
abline(h = 238, col = "purple")

#### dtn decid  ----
dtn_mpine_bw_property = boxplot(prop_rands$DTN_decid, 
                                main = "Randoms for DTN decid at Property Level", 
                                ylab = "Average", 
                                xlab = "DTN decid") 
abline(h = 28.9, col = "purple")

#### dtn bf  ----
dtn_bf_bw_property = boxplot(prop_rands$DTN_bf, 
                                main = "Randoms for DTN bf at Property Level", 
                                ylab = "Average", 
                                xlab = "DTN bf") 
abline(h = 84.1, col = "purple")

#### dtn water  ----
dtn_water_bw_property = boxplot(prop_rands$DTN_water, 
                                main = "Randoms for DTN water at Property Level", 
                                ylab = "Average", 
                                xlab = "DTN water") 
abline(h = 149.0, col = "purple")



# barchart ---- 
#create an empty dataframe for this bad boi 
#prop_avgcover <- data.frame("ndvi" = mean(prop_rands$ndvi), "perc_mpine" = mean(prop_rands$perc_mpine), 
#                       "perc_grassy" = mean(prop_rands$perc_grassy), "perc_decid" = mean(prop_rands$perc_decid), 
#                      "perc_bf" = mean(prop_rands$perc_bf), "perc_water" = mean(prop_rands$perc_water),
#                       "DTN_road" = mean(prop_rands$DTN_road), "DTN_mpine" = mean(prop_rands$DTN_mpine),
#                       "DTN_grassy" = mean(prop_rands$DTN_grassy), "DTN_decid" = mean(prop_rands$DTN_decid), 
#                       "DTN_bf" = mean(prop_rands$DTN_bf), "DTN_water" = mean(prop_rands$DTN_water))

#this is creating a dataframe with columns "Covertype" and "Average 
#prop_avg  = prop_avgcover %>% 
#  pivot_longer(c('ndvi', 'perc_mpine', 'perc_grassy', 'perc_decid', 'perc_bf', 
#                 'perc_water', 'DTN_road', 'DTN_mpine', 'DTN_grassy', 'DTN_decid', 'DTN_bf', 
#                 'DTN_water'), names_to = "covertype", values_to = "Average")
# create a df  for % cover (and splits the dataframe up to make it a little easier to see)
#prop_perc_cov = prop_avgcover %>% 
#  pivot_longer(c('ndvi', 'perc_mpine', 'perc_grassy', 'perc_decid', 'perc_bf', 'perc_water'), names_to = "covertype", values_to = "Average")
# create a df  for dtn (and splits the dataframe up to make it a little easier to see)
#prop_DTN = prop_avgcover %>% 
#  pivot_longer(c('DTN_road', 'DTN_mpine', 'DTN_grassy', 'DTN_decid', 'DTN_bf', 
#                 'DTN_water'), names_to = "covertype", values_to = "Average")

#figure
#randoms_PROP_perc_fig1 = ggplot(prop_perc_cov, aes(x=covertype, y=Average)) + 
#  geom_bar(stat = "identity")

# figure 
#randoms_PROP_dtn_fig2 = ggplot(prop_DTN, aes(x=covertype, y=Average)) + geom_boxplot() + geom_bar(stat = "identity") + stat_boxplot(geom = 'errorbar')


################################################################################
###############################################################################
############ COURSE: -----
course = read.csv("./ResSelData_Course.csv") # draws in the file used in the property scaled data frame 
head(course)
unique(course$response)
nrow(course)

course_rands = subset(course, response == 0)
nrow(course_rands)# 48374 GUCCI 

### BOXPLOTS ----
head(course_rands)

summary(course_rands)
fig1_course_perc = boxplot(course_rands[,11:16])
fig1_course_dtn = boxplot(course_rands[,17:22])

# for reference 
rands_avg # the 10,000 ish randomly generated points to compare 

#### NDVI ----
ndvi_bw_course = boxplot(course_rands$ndvi, 
                           main = "Random point distribution: NDVI at the course level", 
                           ylab = "Average", 
                           xlab = "NDVI") 
abline(h = 0.491, col = "purple")

# I am sticking to these simple plots because they get the point across without 
# me absolutely suffering 

#### % mpine ----
perc_mpine_bw_course = boxplot(course_rands$perc_mpine, 
                                 main = "Randoms for % mpine at course Level", 
                                 ylab = "Average", 
                                 xlab = "% mpine") 
abline(h = 0.529, col = "purple")

#### % grassy ----
perc_grassy_bw_course = boxplot(course_rands$perc_grassy, 
                                  main = "Randoms for % grassy at course Level", 
                                  ylab = "Average", 
                                  xlab = "% grassy") 
abline(h = 0.0784, col = "purple")

#### % decid ----
perc_decid_bw_course = boxplot(course_rands$perc_decid, 
                                 main = "Randoms for % decid at course Level", 
                                 ylab = "Average", 
                                 xlab = "% decid") 
abline(h = 0.269, col = "purple")

#### % BF ----
perc_bf_bw_course = boxplot(course_rands$perc_bf, 
                              main = "Randoms for % bf at course Level", 
                              ylab = "Average", 
                              xlab = "% bf") 
abline(h = 0.0371, col = "purple")

#### % water ----
perc_water_bw_course = boxplot(course_rands$perc_water, 
                                 main = "Randoms for % water at course Level", 
                                 ylab = "Average", 
                                 xlab = "% water") 
abline(h = 0.0371, col = "purple")

#### dtn road  ----
dtn_road_bw_course = boxplot(course_rands$DTN_road, 
                               main = "Randoms for DTN road at course Level", 
                               ylab = "Average", 
                               xlab = "DTN road") 
abline(h = 30.4, col = "purple")

#### dtn mpine  ----
dtn_mpine_bw_course = boxplot(course_rands$DTN_mpine, 
                                main = "Randoms for DTN mpine at course Level", 
                                ylab = "Average", 
                                xlab = "DTN mpine") 
abline(h = 12.4, col = "purple")

#### dtn grassy   ----
dtn_grassy_bw_course = boxplot(course_rands$DTN_grassy, 
                                 main = "Randoms for DTN grassy at course Level", 
                                 ylab = "Average", 
                                 xlab = "DTN grassy") 
abline(h = 238, col = "purple")

#### dtn decid  ----
dtn_mpine_bw_course = boxplot(course_rands$DTN_decid, 
                                main = "Randoms for DTN decid at course Level", 
                                ylab = "Average", 
                                xlab = "DTN decid") 
abline(h = 28.9, col = "purple")

#### dtn bf  ----
dtn_bf_bw_course = boxplot(course_rands$DTN_bf, 
                             main = "Randoms for DTN bf at course Level", 
                             ylab = "Average", 
                             xlab = "DTN bf") 
abline(h = 84.1, col = "purple")

#### dtn water  ----
dtn_water_bw_course = boxplot(course_rands$DTN_water, 
                                main = "Randoms for DTN water at course Level", 
                                ylab = "Average", 
                                xlab = "DTN water") 
abline(h = 149.0, col = "purple")

## bar graphs 
#create an empty dataframe for this bad boi 
#course_avgcover <- data.frame("ndvi" = mean(course_rands$ndvi), "perc_mpine" = mean(course_rands$perc_mpine), 
#                            "perc_grassy" = mean(course_rands$perc_grassy), "perc_decid" = mean(course_rands$perc_decid), 
#                            "perc_bf" = mean(course_rands$perc_bf), "perc_water" = mean(course_rands$perc_water),
#                            "DTN_road" = mean(course_rands$DTN_road), "DTN_mpine" = mean(course_rands$DTN_mpine),
#                            "DTN_grassy" = mean(course_rands$DTN_grassy), "DTN_decid" = mean(course_rands$DTN_decid), 
#                            "DTN_bf" = mean(course_rands$DTN_bf), "DTN_water" = mean(course_rands$DTN_water))
#
#this is creating a dataframe with columns "Covertype" and "Average 
#course_avg  = course_avgcover %>% 
#  pivot_longer(c('ndvi', 'perc_mpine', 'perc_grassy', 'perc_decid', 'perc_bf', 
#                 'perc_water', 'DTN_road', 'DTN_mpine', 'DTN_grassy', 'DTN_decid', 'DTN_bf', 
#                 'DTN_water'), names_to = "covertype", values_to = "Average")
#
#
# create a plot for % cover (and splits the dataframe up to make it a little easier to see)
#course_perc_cov = course_avgcover %>% 
#  pivot_longer(c('ndvi', 'perc_mpine', 'perc_grassy', 'perc_decid', 'perc_bf', 'perc_water'), names_to = "covertype", values_to = "Average")
#randoms_course_perc_fig1 = ggplot(course_perc_cov, aes(x=covertype, y=Average)) + 
#  geom_bar(stat = "identity")


# create a plot for dtn (and splits the dataframe up to make it a little easier to see)
#course_DTN = course_avgcover %>% 
#  pivot_longer(c('DTN_road', 'DTN_mpine', 'DTN_grassy', 'DTN_decid', 'DTN_bf', 
#                 'DTN_water'), names_to = "covertype", values_to = "Average")
#randoms_course_dtn_fig2 = ggplot(course_DTN, aes(x=covertype, y=Average)) + 
#  geom_bar(stat = "identity")


###############################################################################
###############################################################################
# HOMERANGE 
mcp = read.csv("./ResSelData_MCP.csv") # draws in the file used in the property scaled data frame 
head(mcp)
unique(mcp$response)
nrow(mcp)

mcp_rands = subset(mcp, response == 0)
nrow(mcp_rands)# 48374 GUCCI 

### BOXPLOTS ----
head(mcp_rands)

summary(mcp_rands)
fig1_mcp_perc = boxplot(mcp_rands[,11:16])
fig1_mcp_dtn = boxplot(mcp_rands[,17:22])

# for reference 
rands_avg # the 10,000 ish randomly generated points to compare 

#### NDVI ----
ndvi_bw_mcp = boxplot(mcp_rands$ndvi, 
                           main = "Random point distribution: NDVI at the MCP level", 
                           ylab = "Average", 
                           xlab = "NDVI") 
abline(h = 0.491, col = "purple")

# I am sticking to these simple plots because they get the point across without 
# me absolutely suffering 

#### % mpine ----
perc_mpine_bw_mcp = boxplot(mcp_rands$perc_mpine, 
                                 main = "Randoms for % mpine at MCP Level", 
                                 ylab = "Average", 
                                 xlab = "% mpine") 
abline(h = 0.529, col = "purple")

#### % grassy ----
perc_grassy_bw_mcp = boxplot(mcp_rands$perc_grassy, 
                                  main = "Randoms for % grassy at MCP Level", 
                                  ylab = "Average", 
                                  xlab = "% grassy") 
abline(h = 0.0784, col = "purple")

#### % decid ----
perc_decid_bw_mcp = boxplot(mcp_rands$perc_decid, 
                                 main = "Randoms for % decid at mcp Level", 
                                 ylab = "Average", 
                                 xlab = "% decid") 
abline(h = 0.269, col = "purple")

#### % BF ----
perc_bf_bw_mcp = boxplot(mcp_rands$perc_bf, 
                              main = "Randoms for % bf at mcp Level", 
                              ylab = "Average", 
                              xlab = "% bf") 
abline(h = 0.0371, col = "purple")

#### % water ----
perc_water_bw_mcp = boxplot(mcp_rands$perc_water, 
                                 main = "Randoms for % water at mcp Level", 
                                 ylab = "Average", 
                                 xlab = "% water") 
abline(h = 0.0371, col = "purple")

#### dtn road  ----
dtn_road_bw_mcp = boxplot(mcp_rands$DTN_road, 
                               main = "Randoms for DTN road at mcp Level", 
                               ylab = "Average", 
                               xlab = "DTN road") 
abline(h = 30.4, col = "purple")

#### dtn mpine  ----
dtn_mpine_bw_mcp = boxplot(mcp_rands$DTN_mpine, 
                                main = "Randoms for DTN mpine at mcp Level", 
                                ylab = "Average", 
                                xlab = "DTN mpine") 
abline(h = 12.4, col = "purple")

#### dtn grassy   ----
dtn_grassy_bw_mcp = boxplot(mcp_rands$DTN_grassy, 
                                 main = "Randoms for DTN grassy at mcp Level", 
                                 ylab = "Average", 
                                 xlab = "DTN grassy") 
abline(h = 238, col = "purple")

#### dtn decid  ----
dtn_mpine_bw_mcp = boxplot(mcp_rands$DTN_decid, 
                                main = "Randoms for DTN decid at mcp Level", 
                                ylab = "Average", 
                                xlab = "DTN decid") 
abline(h = 28.9, col = "purple")

#### dtn bf  ----
dtn_bf_bw_mcp = boxplot(mcp_rands$DTN_bf, 
                             main = "Randoms for DTN bf at mcp Level", 
                             ylab = "Average", 
                             xlab = "DTN bf") 
abline(h = 84.1, col = "purple")

#### dtn water  ----
dtn_water_bw_mcp = boxplot(prop_rands$DTN_water, 
                                main = "Randoms for DTN water at mcp Level", 
                                ylab = "Average", 
                                xlab = "DTN water") 
abline(h = 149.0, col = "purple")






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


