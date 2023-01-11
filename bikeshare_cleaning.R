# run after bikeshare_wrangling

# inspect January dataframe
str(rides_202201)
summary(rides_202201)

# locate row of out of bounds ride
rides_202201 %>% 
  filter(start_lng > -86) # 1 ride

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
  rides_202201[!(rides_202201$ride_length < 60 | rides_202201$ride_length > 86400 | rides_202201$start_lng > -86),]

# remove started_at, ended_at columns
rides_202201_v2 <- 
  rides_202201_v2 %>% 
 select(-c(started_at, ended_at))
