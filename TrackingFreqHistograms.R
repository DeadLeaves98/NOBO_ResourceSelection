library(dplyr); library(stringr); library(lubridate); library(tidyr)
nobo1 <- read.csv("Orton_Bobwhite_Telemetry_Data_Entry_0.csv")
head(nobo1)
nrow(nobo1) # 38810

##########################################
########################################## NORMAL DATA WRANGLING/CLEANING
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
nobo1$chick <- data.frame("band" = substr(nobo1$Bird.ID, 9, 11)) # isolate the first 3 numbers of band number

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
nobo1 <- dplyr::select(nobo1, -month, -day, -CombinedDate0, -Date)

# Now add a column that coincides with year that will be a; 0 = 2022, 1 = 2023
nobo1$yearBinary = ifelse(nobo1$year == 2023, 1, 0)

##########################################
########################################## END DATA WRANGLING/CLEANING
##########################################


# how many birds do we have?
head(unique(nobo1$Bird.ID))

focB <- subset(nobo1, Bird.ID == "165.535_220574")
durB <- max(focB$ordinal) - min(focB$ordinal) # duration of tracking in days
durBW <- durB/7 # duration of tracking in weeks
freqB <- nrow(focB)/durBW # times tracked per week
freqB

df1 <- data.frame("birdID" = 0, "WeeklyFreq" = 0, "NTimesTracked" = 0)

head(df1)

for(i in 1:length(unique(nobo1$Bird.ID))){
  # i = 12
  focB <- subset(nobo1, Bird.ID == unique(nobo1$Bird.ID)[i])
  durB <- max(focB$ordinal) - min(focB$ordinal) # duration of tracking in days
  durBW <- durB/7 # duration of tracking in weeks
  freqB <- nrow(focB)/durBW # times tracked per week
  newrow <- data.frame("birdID" = focB$Bird.ID[1], "WeeklyFreq" = freqB, "NTimesTracked" = nrow(focB))
  df1 <- rbind(df1, newrow)
}
df1 <- df1[2:nrow(df1),] # remove first row
df1 <- subset(df1, NTimesTracked >= 5)
df1
hist(df1$WeeklyFreq)


