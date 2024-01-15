# Title: Data wrangling and data thinning for Resource Selection 
# Author: Autumn Randall 
# PI: Dr. DJ McNeil
# Date finished: 1/15/2024 @ noon 
# Date last Edited: 1/15/2024 @ noon 
##################################################

library(dplyr); library(stringr); library(lubridate); library(tidyr)
nobo1 <- read.csv("Orton_Bobwhite_Telemetry_Data_Entry_0.csv")
head(nobo1)
nrow(nobo1) # 38810

##########################################
########################################## NORMAL DATA WRANGLING/CLEANING ----
##########################################

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
nobo1$chick <- paste0("band" = substr(nobo1$Bird.ID, 9, 11)) # isolate the first 3 numbers of band number

# subset the entire year of 2022 (1st line) then eliminate chick bands (which start with 225)
nobosub2022 <- nobo1[nobo1$Date >= "2022-01-01" & nobo1$Date <= "2022-12-31", ]
nobosub2022 <- subset(nobosub2022, chick!=225)

# subset the entire year of 2023 (1st line) then eliminate chick bands (which start with 235)
nobosub2023 <- nobo1[nobo1$Date >= "2023-01-01" & nobo1$Date <= "2023-12-31", ]
nobosub2023 <- subset(nobosub2023, chick!=235)

# re-combine the two chunks from 2022-23
nobo2 <- rbind(nobosub2022, nobosub2023)

nobo1 <- nobo2 # rename nobo1
nrow(nobo1) # 31828

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
nobo1 <- alives
#nobo1 <- rbind(alives, fatedead, fatecensor, censors) # Restore this (and uncheck prev line) if fated birds are desired
nobo1 <- nobo1 %>% arrange(ObjectID) # re-order columns

# Date Modification ----
# modifying the dates to something easier to handle
nobodates <- data.frame("Date" = nobo1$Date)
nobodates$Date <- str_replace(nobodates$Date, " AM", "") #replace AM with nothing
nobodates$Date <- str_replace(nobodates$Date, " PM", "") #replace PM with nothing
nobodates <- separate(nobodates, Date, into = c("date", "time"), sep = " ") # split up day and time
# WARNING MESSAGE! "Expected 2 pieces. Missing pieces..." this is b/c some records have no times - just dates
nobodates <- separate(nobodates, date, into = c("year", "month", "day"), sep = "-") # split up month, day, year
head(nobodates)
nobodates <- select(nobodates, -time) # remove time
nobo2 <- cbind(nobo1, nobodates) # add the year, month, and day back to nobo1

# make month, day, and year numeric
nobo2$month <- as.numeric(nobo2$month); nobo2$day <- as.numeric(nobo2$day); nobo2$year <- as.numeric(nobo2$year)
nobo2$CombinedDate0 <- paste0(nobo2$day, "_", nobo2$month) # create a "combined" date

# add ordinal date using the lookup table
lookuptable1 <- read.csv("week_lookuptable.csv")
nobo_ord <- data.frame("ObjectID" = nobo2$ObjectID,"CombinedDate0" = nobo2$CombinedDate0)
merge1 <- merge(x = nobo_ord, by.x = "CombinedDate0", y = lookuptable1, by.y = "day_month", all.x = TRUE)
merge1 <- merge1 %>% arrange(ObjectID) # re-order columns...  NO F---ing CLUE why merge() re-orders our columns...
nobo2 <-cbind(nobo2, merge1[,5:8]) # adding breeding season, ordinal, and week
nobo1 <- nobo2 # turn it back into nobo1

# clean nobo1 up by removing extra columns
nobo1 <- dplyr::select(nobo1, -month, -day, -CombinedDate0)

# Now add a column that coincides with year that will be a; 0 = 2022, 1 = 2023
nobo1$yearBinary = ifelse(nobo1$year == 2023, 1, 0)

unique(nobo1$Bird.Status)
length(unique(nobo1$Bird.ID)) #918 -- this is before anything is done

# isolate the broods and remove the nests 
broods = subset(nobo1, Bird.Status == "Brood")
notbroods = subset(nobo1, Bird.Status == "Alive & Active" | Bird.Status == "Alive and Inactive" | Bird.Status == "Suspected Nest" | Bird.Status == "Alive - Mort Check Only")
nests = subset(nobo1, Bird.Status == "Nest") #just to see how many there are 
length(unique(nests$Bird.ID)) #190 unique Bird.IDs labeled nest

x = length(unique(notbroods$Bird.ID)) #694 
y = length(unique(broods$Bird.ID)) # 85 brooooooods  !!!!
x+y #779 - at the end of this next chunk it should be the same number as this 

# View(broods)
#### Brood points ---- 
# Randomly select 1 occasion for every day tracked 

# create a column that combines bird id and date -- helps identify the duplicate values 
broods$bird.day = paste0(broods$Bird.ID, "_", broods$Date) 
length(unique(broods$Bird.ID)) #85 check to see how many broods their are before we handle the duplicate dates 
nrow(broods) # 1080 

broods1 = broods[sample(1:nrow(broods)), ]  # randomize the order by shuffling the dataset
broods1 = broods[!duplicated(broods$bird.day),] # this will take the first instance of every duplicate for bird.day
length(unique(broods1$Bird.ID)) # 85 - to make sure the same number of broods are left 
broods1 <- broods1[,-18] #remove the unneeded row to be able to rbind
nrow(broods1) # 656 

nobo2 = rbind(notbroods, broods1) #rbind notbroods and the new broods dataframe back together 
length(unique(nobo2$Bird.ID)) #695 - most likely because nests were removed 

nobo1 = nobo2 # revert back to original name for ease 
# Something to think about -- Should we do this section to the entire dataframe and not just broods??

#### MCP prep ----
# thin the data to show only birds that have 4 or more observations


# need to generate table of bird observation frequencies b/c mcp can only be made for birds w > 4 obs
lookup1 <- data.frame("birdID" = NA, "n" = 1) 
for(i in 1:length(unique(nobo1$Bird.ID))){
  bob_i <- subset(nobo1, Bird.ID == unique(nobo1$Bird.ID)[i])  # subset ith bird
  newrow_i <- c(bob_i$Bird.ID[1], nrow(bob_i))
  lookup1 <- rbind(lookup1, newrow_i)
}

nobo1 <- merge(x = nobo1, y = lookup1, by.x = "Bird.ID", by.y = "birdID", all.x = TRUE)
length(unique(nobo1$Bird.ID))# still 695 birds
nobo1$n = as.numeric(nobo1$n) # change to numeric

l = nrow(nobo1[nobo1$n == '1', ]) # 88 
m = nrow(nobo1[nobo1$n == '2', ]) # 422  
a = nrow(nobo1[nobo1$n == '3', ]) # 87
o = nrow(nobo1[nobo1$n == '4', ]) # 76 

#lets dive into figuring this out because this is wild to me 
m2 = subset(nobo1, n == 2)
head(m2) # 
# by looking into th ebirds that have less than 4 observations in the data file (the most recent up to date file[10/2023]) 597 birds were removed before the 
# -- Most died before they were observation #4. Some, however were tracked immediately after trapping in fall of 2023 so it may not be AS drastic as this 
# will need to update this with the most recent telemetry file 

sum(l,m,a) # 169

nobo1 <- subset(nobo1, n > 4) # over 4 locations (5 or more or else during the mcp i get an error)
length(unique(nobo1$Bird.ID)) # only 604  birds...  
nrow(nobo1)
########################## FINISH DATA WRANGLING ############################

#nobo2 <- nobo1[,1:9]
#nobo2 <- nobo1[,1:10]

write.csv(nobo1, "./cleaned_NOBO_telem.csv")
