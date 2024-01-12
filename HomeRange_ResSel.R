


library(rgdal); library(raster); library(adehabitatHR); library(rgeos); library(sf); library(dplyr)
library(lubridate); library(stringr); library(hablar); library(AICcmodavg);  library(lme4)
nobo1 <- read.csv("Orton_Bobwhite_Telemetry_Data_Entry_0.csv") # read in the newest telemetry file 

# Exploratory ----
# how many NOBOs in the dataset?
length(unique(nobo1$Bird.ID)) # 1181

# check what kinds of "bird.status" are present; need to remove nests, fates, etc.
unique(nobo1$Bird.Status) # contains "Alive & Active"  "Nest" "Alive & Inactive Suspected Nest" 
  #"Suspected Fate - RIP" "Fate"  "Alive - Mort Check Only" "Suspected Fate - DNH""Suspected Fate - RF" "Brood"        

# remove nests, fates, etc. by only keeping "alive" points
nobo1 <- subset(nobo1, Bird.Status == "Alive & Active" | Bird.Status ==  "Alive & Inactive" | Bird.Status ==  "Alive - Mort Check Only" | Bird.Status ==  "Brood") 
unique(nobo1$Bird.Status) # confirm that it worked by viewing all unique bird statuses - should just be the "alive" ones
# NOTICE:  I kept broods within the analyses because they are still adult NOBO 
# brooding adults is a norm during the breeding season, therefore to truly understand what they are selecting for we should be considering 
# resource during brooding 

# how many birds remain?
length(unique(nobo1$Bird.ID)) # 1082 birds

# a bunch of techs put chicks/broods/nests as "alive and active"... gotta filter those too
unique(nobo1$Location.Type)
nobo1 <- subset(nobo1, Location.Type == "Regular" | Location.Type == "Brood") #NOTICE: i kept broods in, we will need to manually remove chicks
unique(nobo1$Location.Type) 

# how many birds remain?
length(unique(nobo1$Bird.ID)) # 943 birds

# remove extra columns we don't need
nobo1 <- nobo1[, c(2:3, 5:7, 9:11, 29:30)]
nrow(nobo1) #26599 rows (individual location points)

# need to generate table of bird observation frequencies b/c mcp can only be made for birds w > 4 obs
lookup1 <- data.frame("birdID" = NA, "n" = 1)
for(i in 1:length(unique(nobo1$Bird.ID))){
  bob_i <- subset(nobo1, Bird.ID == unique(nobo1$Bird.ID)[i])  # subset ith bird
  newrow_i <- c(bob_i$Bird.ID[1], nrow(bob_i))
  lookup1 <- rbind(lookup1, newrow_i)
}

nobo1 <- merge(x = nobo1, y = lookup1, by.x = "Bird.ID", by.y = "birdID", all.x = TRUE)
length(unique(nobo1$Bird.ID))# still 943 birds
nobo1$n = as.numeric(nobo1$n) # change to numeric

l = nrow(nobo1[nobo1$n == '1', ]) # 88 
m = nrow(nobo1[nobo1$n == '2', ]) # 422  
a = nrow(nobo1[nobo1$n == '3', ]) # 87
o = nrow(nobo1[nobo1$n == '4', ]) # 76 

#lets dive into figuring this out because this is wild to me 
m2 = subset(nobo1, n == 2)
head(m2) # 
# by looking into th ebirds that have less than 4 observations in the data file (the most recent up to date file[10/2023]) 597 birds died before the 
# 4th observation was done -- this does include some birds that were tracked immediately after trapping in fall of 2023 so it may not be AS drastic as this 
# will need to update this with the most recent telemetry file 

sum(l,m,a)

nobo1 <- subset(nobo1, n >= 4) # only include birds greater than or equal to 4 
length(unique(nobo1$Bird.ID)) # only 615  birds... UNSURE AS TO WHY THIS DELETED LIKE 300 BIRDS 


################ MCP ----
#################################### Make MCP, buffer, and randoms for one bird to make sure I have the code working right

# Update: Will remove the buffer for newest analysis that we decided on during a meeting on 1/12/2024

# subset one bird using the subset function
bob1 <- subset(nobo1, Bird.ID == unique(nobo1$Bird.ID)[100]) # randomly subsetting the data and taking the 100th bird

# Make MCP using the MCP function; need to convert bob1 to spatial object first though
bob_sp <- SpatialPoints(coords = data.frame("x" = bob1$x, "y" = bob1$y)) # convert to spatial object
crs(bob_sp) <- CRS("+init=epsg:4326") # define CRS 
plot(bob_sp) # make sure it worked
mcp <- mcp(bob_sp, percent=100) # create MCP for bob_sp
# NOTE: throws warning "GEOS support is provided by..." but I think it's fine
plot(mcp, add = TRUE) # overlay MCP to make sure it worked
crs(mcp)



# do I need to do this next step? (referring to lines 89 to 96) 

# Make 200m buffer around the MCP
albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
mcp1 <- spTransform(mcp, CRS(albers)) # transform MCP to projected coordinate system
bob_sp1 <- spTransform(bob_sp, CRS(albers)) # transform points to projected coordinate system
plot(mcp1); plot(bob_sp1, add = TRUE) # coordinate systems should match: North American Datum 1983
  # buff1 <- gBuffer(mcp1, width=200)
  # NOTE: throws warning "In proj4string(xy) : CRS object has comment..." but I think it's fine
  # plot(buff1, add = TRUE)


# generate random points (requires st_sample() which can only be done using sf)
buff2 <- st_as_sf(buff1) # convert to sf
points1 = st_sample(buff2, size=nrow(bob1) * 3) # generate 3 random points for each "used" point using st_sample()
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


