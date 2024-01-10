library(dplyr)
nobo1 <- read.csv("Orton_Bobwhite_Telemetry_Data_Entry_0.csv")
head(nobo1)
nrow(nobo1) # 38810

# clean this file up by removing the columns we do not need
nobo1 <- dplyr::select(nobo1, -GlobalID, -Time, -Burn.Status, -Habitat.Type,
                       -Associated.Bird..1, -Associated.Bird..2, -Associated.Bird..3,
                       -Associated.Bird..4, -Associated.Bird..5, -Associated.Bird..6,
                       -Associated.Bird..7, -Associated.Bird..8, -Associated.Bird..9,
                       -Associated.Bird..10, -Parent.Present., -Enter.Adult.ID,
                       -Comments, -CreationDate, -Creator, -EditDate, -Editor)

head(nobo1)
nobo1 <- subset(nobo1, Location.Type != "Chick") # remove chicks
nobo1 <- arrange(nobo1, ObjectID) # re-order rows in order of objectID

# dates are currently characters; need to convert to date format (mm/dd/yyyy)
nobo1 <- nobo1 %>% mutate(Date = as.Date(Date, format = "%m/%d/%Y"))

# Some chicks were entered as broods and we need to remove these chicks from the data 
nrow(nobo1) # 31877
nobo1$chick = data.frame("band" = substr(nobo1$Bird.ID, 9, 11)) # isolate the first 3 numbers of band number

################### LEFT OFF HERE!!!


nobosub2022 = nobo1[nobo1$Date >= "2022-01-01" & nobo1$Date <= "2022-12-31", ]
length(unique(nobosub2022$Bird.ID))
nobosub2022 = subset(nobosub2022, chick!=225)
length(unique(nobosub2022$Bird.ID))

nobosub2023 = nobo1[nobo1$Date >= "2023-01-01" & nobo1$Date <= "2023-12-31", ]
length(unique(nobosub2023$Bird.ID))
nobosub2023 = subset(nobosub2023, chick!=235)
length(unique(nobosub2023$Bird.ID))

nobo2 = rbind(nobosub2022, nobosub2023)

nobo1 = nobo2

nrow(nobo1) # 28506

# 162.603_220346 should actually be called 162.376_220019 so we need to fix that
nobo1$Bird.ID <- str_replace(nobo1$Bird.ID , "162.603_220346", "162.376_220019")

#### Add "encounter" as 0 (censor), 1 (alive), or 2 (dead)
unique(nobo1$Bird.Status)
# alive
alives <- subset(nobo1, Bird.Status == "Alive & Active" | Bird.Status == "Nest" | Bird.Status == "Alive & Inactive" |
                   Bird.Status == "Brood" | Bird.Status == "Alive - Mort Check Only" | Bird.Status == "Suspected Nest") # 13201 rows
# fate (also includes some censors)
fates <- subset(nobo1, Bird.Status == "Fate") # all birds with status = "Fate" = fate
fatedead <- subset(fates, Fate != "Censor") # all fate birds where fate is not "censor", fatedead
fatecensor <- subset(fates, Fate == "Censor") # all fate birds where fate IS "censor", censor
nrow(fatedead) + nrow(fatecensor) == nrow(fates) # make sure the math adds up... should say TRUE
# censors
censors <- subset(nobo1, Bird.Status == "Suspected Fate - RF" | Bird.Status == "Suspected Fate - DNH" | Bird.Status == "Suspected Fate - RIP") # 351 rows
nrow(alives) + nrow(fatedead) + nrow(fatecensor) + nrow(censors) == nrow(nobo1) # 33814 rows
# add encounter
alives$encounter = 1
fatedead$encounter = 2
fatecensor$encounter = 0
censors$encounter = 0
nobo1 <- rbind(alives, fatedead, fatecensor, censors)
nobo1 <- nobo1 %>% arrange(ObjectID) # re-order columns

# modifying the dates to something easier to handle
nobodates <- data.frame("Date" = nobo1$Date)
nobodates$Date <- str_replace(nobodates$Date, " AM", "") #replace AM with nothing
nobodates$Date <- str_replace(nobodates$Date, " PM", "") #replace PM with nothing
nobodates <- separate(nobodates, Date, into = c("date", "time"), sep = " ") # split up day and time
# WARNING MESSAGE! "Expected 2 pieces. Missing pieces..." this is b/c some records have no times - just dates
nobodates <- separate(nobodates, date, into = c("year", "month", "day"), sep = "-") # split up month, day, year


# IF we need the code that fixes the issue that occurred within the year column that had some years 
# as 202 it is what is below
# error somewhere that turned some years (dates) into 202 rather than 2022 
# this fixes it in a wild roundabout way 
# nobodates$year <- str_replace(nobodates$year, "2023", "a") 
# nobodates$year <- str_replace(nobodates$year, "2022", "b") 
# nobodates$year <- str_replace(nobodates$year, "202", "2022") 
# nobodates$year <- str_replace(nobodates$year, "b", "2022") 
# nobodates$year <- str_replace(nobodates$year, "a", "2023") 

nobodates <- separate(nobodates, time, into = c("hour", "min"), sep = ":") # split up hour, minute, and second
nobo2 <- cbind(nobo1, nobodates)

# make month, day, and year numeric
nobo2$month <- as.numeric(nobo2$month); nobo2$day <- as.numeric(nobo2$day); nobo2$year <- as.numeric(nobo2$year)
nobo2$CombinedDate0 <- paste0(nobo2$day, "_", nobo2$month)

# add ordinal date using the lookup table
lookuptable1 <- read.csv("E:/NOBO Project Data/Analyses/NonBreeding Season/Nonbreeding_Survival/week_lookuptable.csv")
# lookuptable1 <- read.csv("C:/Users/User/Desktop/Autumn_analyses/TimeVaryingCovPrep/week_lookuptable_10oct2023.csv")
nobo_ord <- data.frame("ObjectID" = nobo2$ObjectID,"CombinedDate0" = nobo2$CombinedDate0)
merge1 <- merge(x = nobo_ord, by.x = "CombinedDate0", y = lookuptable1, by.y = "day_month", all.x = TRUE)
merge1 <- merge1 %>% arrange(ObjectID) # re-order columns...  NO FUCKING CLUE why merge() re-orders our columns...
nobo2 <-cbind(nobo2, merge1[,5:8]) # adding breeding season, ordinal, and week
nobo1 <- nobo2 # turn it back into nobo1

# clean nobo1 up by removing extra columns
nobo1 <- dplyr::select(nobo1, -month, -day, -hour, -min, -CombinedDate0, -Date)
nrow(nobo1) # 28506

# filter to be both breeding seasons 
breeding1 = subset(nobo1, breedingseasonCov == 1) # filters for BOTH summers 
nrow(breeding1) # 23744

# Now add a column that coincides with year that will be a; 0 = 2022, 1 = 2023
breeding1$year = ifelse(breeding1$year == 2023, 1, 0)

## Extract covariate values at each point
breeding_sp <- SpatialPoints(coords = data.frame("x" = breeding1$x, "y" = breeding1$y)) # convert DF to Spatial Points
crs(breeding_sp) <- CRS("+init=epsg:4326") # define CRS for the spatial points (EPSG 4326 == lat/lon WGS84 etc)

courses <- readOGR("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/shapefiles/OrtonCourses.shp")
# courses <- readOGR("C:/Users/User/Desktop/Autumn_analyses/TimeVaryingCovPrep/shapefiles/OrtonCourses.shp")
plot(courses)
crs(courses) # NAD83