# run after bikeshare_wrangling

# inspect January dataframe
str(rides_202201)
summary(rides_202201)

# locate row of out of bounds ride
rides_202201 %>% 
  filter(start_lng > -86) # 1 ride

# locate rides with no end_lat
rides_202201_v2 %>% 
  filter(is.na(end_lat) | is.na(end_lng))

# create columns with starting, ending datetimes
rides_202201$start_datetime <- ymd_hms(rides_202201$started_at)
rides_202201$end_datetime <- ymd_hms(rides_202201$ended_at)

# create column ride_length (in seconds) as numeric 
rides_202201$ride_length <- rides_202201$end_datetime - rides_202201$start_datetime
rides_202201$ride_length <- as.numeric(rides_202201$ride_length)

# locate rides with ride_length < 60 or ride_length > 86400
rides_202201 %>% 
  filter(ride_length < 60 | ride_length > 86400) #1940 rides

# create column for day of week Sunday = 1
rides_202201$weekday <- wday(rides_202201$start_datetime)
summary(rides_202201$weekday)

# create column for month of year, day of month
rides_202201$month <- month(rides_202201$start_datetime)
rides_202201$day <- day(rides_202201$start_datetime)
summary(rides_202201$day)

# create new data frame with cleaned January data
rides_202201_v2 <- 
  rides_202201[!(rides_202201$ride_length < 60 | rides_202201$ride_length > 86400 | rides_202201$start_lng > -86 | is.na(rides_202201$end_lat) | is.na(rides_202201$end_lng)),]

# remove started_at, ended_at columns
rides_202201_v2 <- 
  rides_202201_v2 %>% 
 select(-c(started_at, ended_at))

# remove original df
rm(rides_202201)

# repeat for February
str(rides_202202)
summary(rides_202202) # check for outliers
rides_202202$start_datetime <- ymd_hms(rides_202202$started_at) # create start datetime variable
rides_202202$end_datetime <- ymd_hms(rides_202202$ended_at) # create end datetime variable
rides_202202$ride_length <- rides_202202$end_datetime - rides_202202$start_datetime # calculate ride length in seconds
rides_202202$ride_length <- as.numeric(rides_202202$ride_length) # change ride length to numeric
rides_202202 %>% 
  filter(ride_length < 60 | ride_length > 86400) #2610 rides
rides_202202$weekday <- wday(rides_202202$start_datetime) # create weekday variable
rides_202202$month <- month(rides_202202$start_datetime) # create month variable
rides_202202$day <- day(rides_202202$start_datetime) # create day number variable - non leap-year
rides_202202_v2 <- rides_202202[!(rides_202202$ride_length < 60 | rides_202202$ride_length > 86400 | is.na(rides_202201$end_lat) | is.na(rides_202201$end_lng)),]
rides_202202_v2 <- rides_202202_v2 %>% 
  select(-c(started_at, ended_at))
rm(rides_202202)

# repeat for March
str(rides_202203)
summary(rides_202203) # check for outliers
rides_202203$start_datetime <- ymd_hms(rides_202203$started_at) # create start datetime variable
rides_202203$end_datetime <- ymd_hms(rides_202203$ended_at) # create end datetime variable
rides_202203$ride_length <- rides_202203$end_datetime - rides_202203$start_datetime # calculate ride length in seconds
rides_202203$ride_length <- as.numeric(rides_202203$ride_length) # change ride length to numeric
# find rides that start before 2AM and end after 3AM 03/13/2022
rides_202203 %>% 
  filter(start_datetime <= '2022-03-13 02:00:00' & end_datetime >= '2022-03-13 03:00:00') %>%
  mutate(ride_length = ride_length - 360) # subtract 1 hour from those rides
# cleaning cont'd
rides_202203 %>% 
  filter(ride_length < 60 | ride_length > 86400) # 5360 rides
rides_202203$weekday <- wday(rides_202203$start_datetime) # create weekday variable
rides_202203$month <- month(rides_202203$start_datetime) # create month variable
rides_202203$day <- day(rides_202203$start_datetime) # create day number variable
rides_202203_v2 <- rides_202203[!(rides_202203$ride_length < 60 | rides_202203$ride_length > 86400 | is.na(rides_202201$end_lat) | is.na(rides_202201$end_lng)),] 
rides_202203_v2 <- rides_202203_v2 %>% 
  select(-c(started_at, ended_at, ride_hour, ride_start_time))
rm(rides_202203)

# repeat for April
str(rides_202204)
summary(rides_202204) # check for outliers
rides_202204$start_datetime <- ymd_hms(rides_202204$started_at) # create start datetime variable
rides_202204$end_datetime <- ymd_hms(rides_202204$ended_at) # create end datetime variable
rides_202204$ride_length <- rides_202204$end_datetime - rides_202204$start_datetime # calculate ride length in seconds
rides_202204$ride_length <- as.numeric(rides_202204$ride_length) # change ride length to numeric
rides_202204 %>% 
  filter(ride_length < 60 | ride_length > 86400) #7619 rides
rides_202204$weekday <- wday(rides_202204$start_datetime) # create weekday variable
rides_202204$month <- month(rides_202204$start_datetime) # create month variable
rides_202204$day <- day(rides_202204$start_datetime) # create day number variable
rides_202204_v2 <- rides_202204[!(rides_202204$ride_length < 60 | rides_202204$ride_length > 86400 | is.na(rides_202201$end_lat) | is.na(rides_202201$end_lng)),]
rides_202204_v2 <- rides_202204_v2 %>% 
  select(-c(started_at, ended_at))
rm(rides_202204)

# repeat for May
str(rides_202205)
summary(rides_202205) # check for outliers
rides_202205$start_datetime <- ymd_hms(rides_202205$started_at) # create start datetime variable
rides_202205$end_datetime <- ymd_hms(rides_202205$ended_at) # create end datetime variable
rides_202205$ride_length <- rides_202205$end_datetime - rides_202205$start_datetime # calculate ride length in seconds
rides_202205$ride_length <- as.numeric(rides_202205$ride_length) # change ride length to numeric
rides_202205 %>% 
  filter(ride_length < 60 | ride_length > 86400) #13076 rides
rides_202205$weekday <- wday(rides_202205$start_datetime) # create weekday variable
rides_202205$month <- month(rides_202205$start_datetime) # create month variable
rides_202205$day <- day(rides_202205$start_datetime) # create day number variable
rides_202205_v2 <- rides_202205[!(rides_202205$ride_length < 60 | rides_202205$ride_length > 86400) | is.na(rides_202201$end_lat) | is.na(rides_202201$end_lng)),] 
rides_202205_v2 <- rides_202205_v2 %>% 
  select(-c(started_at, ended_at))
rm(rides_202205)

# repeat for June
str(rides_202206)
summary(rides_202206) # check for outliers
rides_202206$start_datetime <- ymd_hms(rides_202206$started_at) # create start datetime variable
rides_202206$end_datetime <- ymd_hms(rides_202206$ended_at) # create end datetime variable
rides_202206$ride_length <- rides_202206$end_datetime - rides_202206$start_datetime # calculate ride length in seconds
rides_202206$ride_length <- as.numeric(rides_202206$ride_length) # change ride length to numeric
rides_202206 %>% 
  filter(ride_length < 60 | ride_length > 86400) #13076 rides
rides_202206$weekday <- wday(rides_202206$start_datetime) # create weekday variable
rides_202206$month <- month(rides_202206$start_datetime) # create month variable
rides_202206$day <- day(rides_202206$start_datetime) # create day number variable
rides_202206_v2 <- rides_202206[!(rides_202206$ride_length < 60 | rides_202206$ride_length > 86400 | is.na(rides_202201$end_lat) | is.na(rides_202201$end_lng)),] 
rides_202206_v2 <- rides_202206_v2 %>% 
  select(-c(started_at, ended_at))
rm(rides_202206)

# repeat for July
str(rides_202207)
summary(rides_202207) # check for outliers
rides_202207$start_datetime <- ymd_hms(rides_202207$started_at) # create start datetime variable
rides_202207$end_datetime <- ymd_hms(rides_202207$ended_at) # create end datetime variable
rides_202207$ride_length <- rides_202207$end_datetime - rides_202207$start_datetime # calculate ride length in seconds
rides_202207$ride_length <- as.numeric(rides_202207$ride_length) # change ride length to numeric
rides_202207 %>% 
  filter(ride_length < 60 | ride_length > 86400) #13076 rides
rides_202207$weekday <- wday(rides_202207$start_datetime) # create weekday variable
rides_202207$month <- month(rides_202207$start_datetime) # create month variable
rides_202207$day <- day(rides_202207$start_datetime) # create day number variable
rides_202207_v2 <- rides_202207[!(rides_202207$ride_length < 60 | rides_202207$ride_length > 86400 | is.na(rides_202201$end_lat) | is.na(rides_202201$end_lng)),] 
rides_202207_v2 <- rides_202207_v2 %>% 
  select(-c(started_at, ended_at))
rm(rides_202207)

# repeat for August
str(rides_202208)
summary(rides_202208) # check for outliers
rides_202208$start_datetime <- ymd_hms(rides_202208$started_at) # create start datetime variable
rides_202208$end_datetime <- ymd_hms(rides_202208$ended_at) # create end datetime variable
rides_202208$ride_length <- rides_202208$end_datetime - rides_202208$start_datetime # calculate ride length in seconds
rides_202208$ride_length <- as.numeric(rides_202208$ride_length) # change ride length to numeric
rides_202208 %>% 
  filter(ride_length < 60 | ride_length > 86400) #13076 rides
rides_202208$weekday <- wday(rides_202208$start_datetime) # create weekday variable
rides_202208$month <- month(rides_202208$start_datetime) # create month variable
rides_202208$day <- day(rides_202208$start_datetime) # create day number variable
rides_202208_v2 <- rides_202208[!(rides_202208$ride_length < 60 | rides_202208$ride_length > 86400 | is.na(rides_202201$end_lat) | is.na(rides_202201$end_lng)),]
rides_202208_v2 <- rides_202208_v2 %>% 
  select(-c(started_at, ended_at))
rm(rides_202208)

# repeat for September
str(rides_202209)
summary(rides_202209) # check for outliers
rides_202209$start_datetime <- ymd_hms(rides_202209$started_at) # create start datetime variable
rides_202209$end_datetime <- ymd_hms(rides_202209$ended_at) # create end datetime variable
rides_202209$ride_length <- rides_202209$end_datetime - rides_202209$start_datetime # calculate ride length in seconds
rides_202209$ride_length <- as.numeric(rides_202209$ride_length) # change ride length to numeric
rides_202209 %>% 
  filter(ride_length < 60 | ride_length > 86400) #13076 rides
rides_202209$weekday <- wday(rides_202209$start_datetime) # create weekday variable
rides_202209$month <- month(rides_202209$start_datetime) # create month variable
rides_202209$day <- day(rides_202209$start_datetime) # create day number variable
rides_202209_v2 <- rides_202209[!(rides_202209$ride_length < 60 | rides_202209$ride_length > 86400) | is.na(rides_202201$end_lat) | is.na(rides_202201$end_lng)),] 
rides_202209_v2 <- rides_202209_v2 %>% 
  select(-c(started_at, ended_at))
rm(rides_202209)

# repeat for October
str(rides_202210)
summary(rides_202210) # check for outliers
rides_202210$start_datetime <- ymd_hms(rides_202210$started_at) # create start datetime variable
rides_202210$end_datetime <- ymd_hms(rides_202210$ended_at) # create end datetime variable
rides_202210$ride_length <- rides_202210$end_datetime - rides_202210$start_datetime # calculate ride length in seconds
rides_202210$ride_length <- as.numeric(rides_202210$ride_length) # change ride length to numeric
rides_202210 %>% 
  filter(ride_length < 60 | ride_length > 86400) #13076 rides
rides_202210$weekday <- wday(rides_202210$start_datetime) # create weekday variable
rides_202210$month <- month(rides_202210$start_datetime) # create month variable
rides_202210$day <- day(rides_202210$start_datetime) # create day number variable
rides_202210_v2 <- rides_202210[!(rides_202210$ride_length < 60 | rides_202210$ride_length > 86400 | is.na(rides_202201$end_lat) | is.na(rides_202201$end_lng)),] 
rides_202210_v2 <- rides_202210_v2 %>% 
  select(-c(started_at, ended_at))
rm(rides_202210)

# repeat for March
str(rides_202211)
summary(rides_202211) # check for outliers
rides_202211$start_datetime <- ymd_hms(rides_202211$started_at) # create start datetime variable
rides_202211$end_datetime <- ymd_hms(rides_202211$ended_at) # create end datetime variable
rides_202211$ride_length <- rides_202211$end_datetime - rides_202211$start_datetime # calculate ride length in seconds
rides_202211$ride_length <- as.numeric(rides_202211$ride_length) # change ride length to numeric
# find rides that start before 2AM and end after 2AM 11/06/2022
rides_202211 %>% 
  filter(start_datetime < '2022-11-06 02:00:00' & end_datetime > '2022-11-06 02:00:00')
# cleaning cont'd
rides_202211 %>% 
  filter(ride_length < 60 | ride_length > 86400) # 5360 rides
rides_202211$weekday <- wday(rides_202211$start_datetime) # create weekday variable
rides_202211$month <- month(rides_202211$start_datetime) # create month variable
rides_202211$day <- day(rides_202211$start_datetime) # create day number variable
rides_202211_v2 <- rides_202211[!(rides_202211$ride_length < 60 | rides_202211$ride_length > 86400 | (rides_202211$start_datetime < '2022-11-06 02:00:00' & rides_202211$end_datetime > '2022-11-06 02:00:00') | is.na(rides_202201$end_lat) | is.na(rides_202201$end_lng)),] 
rides_202211_v2 <- rides_202211_v2 %>% 
  select(-c(started_at, ended_at))
rm(rides_202211)

# repeat for December
str(rides_202212)
summary(rides_202212) # check for outliers
rides_202212$start_datetime <- ymd_hms(rides_202212$started_at) # create start datetime variable
rides_202212$end_datetime <- ymd_hms(rides_202212$ended_at) # create end datetime variable
rides_202212$ride_length <- rides_202212$end_datetime - rides_202212$start_datetime # calculate ride length in seconds
rides_202212$ride_length <- as.numeric(rides_202212$ride_length) # change ride length to numeric
rides_202212 %>% 
  filter(ride_length < 60 | ride_length > 86400) #13076 rides
rides_202212$weekday <- wday(rides_202212$start_datetime) # create weekday variable
rides_202212$month <- month(rides_202212$start_datetime) # create month variable
rides_202212$day <- day(rides_202212$start_datetime) # create day number variable
rides_202212_v2 <- rides_202212[!(rides_202212$ride_length < 60 | rides_202212$ride_length > 86400 | is.na(rides_202201$end_lat) | is.na(rides_202201$end_lng)),]
rides_202212_v2 <- rides_202212_v2 %>% 
  select(-c(started_at, ended_at))
rm(rides_202212)
