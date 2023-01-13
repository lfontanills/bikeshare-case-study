# run after bikeshare_wrangling.R


###
# inspect January data frame
str(rides_202201)
summary(rides_202201)

# locate rows with rides not in Chicago area
rides_202201 %>% 
  filter(start_lng > -86)

# locate rides with no end coordinates
rides_202201 %>% 
  filter(is.na(end_lat) | is.na(end_lng))

# create columns with starting, ending ride datetimes
rides_202201$start_datetime <- ymd_hms(rides_202201$started_at)
rides_202201$end_datetime <- ymd_hms(rides_202201$ended_at)

# create column ride_length (in seconds) as numeric 
rides_202201$ride_length <- as.numeric(rides_202201$end_datetime - rides_202201$start_datetime)

# locate rides with ride_length < 60 seconds or ride_length > 86400 seconds
rides_202201 %>% 
  filter(ride_length < 60 | ride_length > 86400)

# create column for start date only, start time only, start month only 
rides_202201$ride_date <- as.Date(rides_202201$start_datetime) # Date format
rides_202201$ride_hour <- hour(rides_202201$start_datetime) # integer
rides_202201$ride_month <- month(rides_202201$start_datetime, label = TRUE)

# create column for weekdays
rides_202201$ride_weekday <- wday(rides_202201$start_datetime, label = TRUE)

# create new data frame with cleaned January data
rides_202201_v2 <- 
  rides_202201[!(rides_202201$ride_length < 60 | rides_202201$ride_length > 86400 | rides_202201$start_lng > -86 | is.na(rides_202201$end_lat) | is.na(rides_202201$end_lng)),]

# remove started_at, ended_at columns
rides_202201_v2 <- 
  rides_202201_v2 %>% 
 select(-c(started_at, ended_at))

# remove old df
rm(rides_202201)


###
# could loop over rest of months or inspect each month individually


###
# inspect February data frame
str(rides_202202)
summary(rides_202202)

rides_202202 %>% # locate rides with no end coordinates
  filter(is.na(end_lat) | is.na(end_lng))

rides_202202$start_datetime <- ymd_hms(rides_202202$started_at) # create start datetime variable
rides_202202$end_datetime <- ymd_hms(rides_202202$ended_at) # create end datetime variable

rides_202202$ride_length <- as.numeric(rides_202202$end_datetime - rides_202202$start_datetime) # create column ride_length (in seconds) as numeric 

rides_202202 %>% 
  filter(ride_length < 60 | ride_length > 86400) # filter out too-short, too-long rides

rides_202202$ride_date <- as.Date(rides_202202$start_datetime) # Date format
rides_202202$ride_hour <- hour(rides_202202$start_datetime) # hour as integer
rides_202202$ride_month <- month(rides_202202$start_datetime, label = TRUE) # month as ord factor w/ 12 levels
rides_202202$ride_weekday <- wday(rides_202202$start_datetime, label = TRUE) # weekday  as ord factor w/ 7 levels

rides_202202_v2 <- rides_202202[!(rides_202202$ride_length < 60 |  # filter out too-short rides
                                    rides_202202$ride_length > 86400 | # filter out too-long rides
                                    is.na(rides_202202$end_lat) |  # filter out rides with no end latitude
                                    is.na(rides_202202$end_lng)), # filter out rides with no end longitude
                                ]
rides_202202_v2 <- rides_202202_v2 %>% 
  select(-c(started_at, ended_at)) # remove started_at, ended_at columns

rm(rides_202202) # remove old data frame


###
# inspect March data frame
str(rides_202203)
summary(rides_202203)

rides_202203 %>% # locate rides with no end coordinates
  filter(is.na(end_lat) | is.na(end_lng))

rides_202203$start_datetime <- ymd_hms(rides_202203$started_at) # create start datetime variable
rides_202203$end_datetime <- ymd_hms(rides_202203$ended_at) # create end datetime variable
rides_202203$ride_length <- as.numeric(rides_202203$end_datetime - rides_202203$start_datetime) # create column ride_length (in seconds) as numeric 

# Account for DST: find rides that start before 2AM and end after 3AM 03/13/2022
rides_202203 %>% 
  filter(start_datetime <= '2022-03-13 02:00:00' & end_datetime >= '2022-03-13 03:00:00') %>%
  mutate(ride_length = ride_length - 360) # subtract 1 hour from ride durations

# March cleaning cont'd
rides_202203 %>% 
  filter(ride_length < 60 | ride_length > 86400) # filter out too-short, too-long rides

rides_202203$ride_date <- as.Date(rides_202203$start_datetime) # Date format
rides_202203$ride_hour <- hour(rides_202203$start_datetime) # hour as integer
rides_202203$ride_month <- month(rides_202203$start_datetime, label = TRUE) # month as ord factor w/ 12 levels
rides_202203$ride_weekday <- wday(rides_202203$start_datetime, label = TRUE) # weekday  as ord factor w/ 7 levels

rides_202203_v2 <- rides_202203[!(rides_202203$ride_length < 60 |  # filter out too-short rides
                                    rides_202203$ride_length > 86400 | # filter out too-long rides
                                    is.na(rides_202203$end_lat) | # filter out rides with no end latitude
                                    is.na(rides_202203$end_lng)),]  # filter out rides with no end longitude

rides_202203_v2 <- rides_202203_v2 %>% 
  select(-c(started_at, ended_at)) # remove started_at, ended_at columns

rm(rides_202203)  # remove old data frame


###
# inspect April data frame
str(rides_202204)
summary(rides_202204)

rides_202204 %>% # locate rides with no end coordinates
  filter(is.na(end_lat) | is.na(end_lng))

rides_202204$start_datetime <- ymd_hms(rides_202204$started_at) # create start datetime variable
rides_202204$end_datetime <- ymd_hms(rides_202204$ended_at) # create end datetime variable

rides_202204$ride_length <- as.numeric(rides_202204$end_datetime - rides_202204$start_datetime) # create column ride_length (in seconds) as numeric 

rides_202204 %>% 
  filter(ride_length < 60 | ride_length > 86400) # filter out too-short, too-long rides

rides_202204$ride_date <- as.Date(rides_202204$start_datetime) # Date format
rides_202204$ride_hour <- hour(rides_202204$start_datetime) # hour as integer
rides_202204$ride_month <- month(rides_202204$start_datetime, label = TRUE) # month as ord factor w/ 12 levels
rides_202204$ride_weekday <- wday(rides_202204$start_datetime, label = TRUE) # weekday  as ord factor w/ 7 levels

rides_202204_v2 <- rides_202204[!(rides_202204$ride_length < 60 |  # filter out too-short rides
                                    rides_202204$ride_length > 86400 | # filter out too-long rides
                                    is.na(rides_202204$end_lat) |  # filter out rides with no end latitude
                                    is.na(rides_202204$end_lng)), # filter out rides with no end longitude
]
rides_202204_v2 <- rides_202204_v2 %>% 
  select(-c(started_at, ended_at)) # remove started_at, ended_at columns

rm(rides_202204) # remove old data frame


###
# inspect May data frame
str(rides_202205)
summary(rides_202205)

rides_202205 %>% # locate rides with no end coordinates
  filter(is.na(end_lat) | is.na(end_lng))

rides_202205$start_datetime <- ymd_hms(rides_202205$started_at) # create start datetime variable
rides_202205$end_datetime <- ymd_hms(rides_202205$ended_at) # create end datetime variable

rides_202205$ride_length <- as.numeric(rides_202205$end_datetime - rides_202205$start_datetime) # create column ride_length (in seconds) as numeric 

rides_202205 %>% 
  filter(ride_length < 60 | ride_length > 86400) # filter out too-short, too-long rides

rides_202205$ride_date <- as.Date(rides_202205$start_datetime) # Date format
rides_202205$ride_hour <- hour(rides_202205$start_datetime) # hour as integer
rides_202205$ride_month <- month(rides_202205$start_datetime, label = TRUE) # month as ord factor w/ 12 levels
rides_202205$ride_weekday <- wday(rides_202205$start_datetime, label = TRUE) # weekday  as ord factor w/ 7 levels

rides_202205_v2 <- rides_202205[!(rides_202205$ride_length < 60 |  # filter out too-short rides
                                    rides_202205$ride_length > 86400 | # filter out too-long rides
                                    is.na(rides_202205$end_lat) |  # filter out rides with no end latitude
                                    is.na(rides_202205$end_lng)), # filter out rides with no end longitude
]
rides_202205_v2 <- rides_202205_v2 %>% 
  select(-c(started_at, ended_at)) # remove started_at, ended_at columns

rm(rides_202205) # remove old data frame


###
# inspect June data frame
str(rides_202206)
summary(rides_202206)

rides_202206 %>% # locate rides with no end coordinates
  filter(is.na(end_lat) | is.na(end_lng))

rides_202206$start_datetime <- ymd_hms(rides_202206$started_at) # create start datetime variable
rides_202206$end_datetime <- ymd_hms(rides_202206$ended_at) # create end datetime variable

rides_202206$ride_length <- as.numeric(rides_202206$end_datetime - rides_202206$start_datetime) # create column ride_length (in seconds) as numeric 

rides_202206 %>% 
  filter(ride_length < 60 | ride_length > 86400) # filter out too-short, too-long rides

rides_202206$ride_date <- as.Date(rides_202206$start_datetime) # Date format
rides_202206$ride_hour <- hour(rides_202206$start_datetime) # hour as integer
rides_202206$ride_month <- month(rides_202206$start_datetime, label = TRUE) # month as ord factor w/ 12 levels
rides_202206$ride_weekday <- wday(rides_202206$start_datetime, label = TRUE) # weekday  as ord factor w/ 7 levels

rides_202206_v2 <- rides_202206[!(rides_202206$ride_length < 60 |  # filter out too-short rides
                                    rides_202206$ride_length > 86400 | # filter out too-long rides
                                    is.na(rides_202206$end_lat) |  # filter out rides with no end latitude
                                    is.na(rides_202206$end_lng)), # filter out rides with no end longitude
]
rides_202206_v2 <- rides_202206_v2 %>% 
  select(-c(started_at, ended_at)) # remove started_at, ended_at columns

rm(rides_202206) # remove old data frame


###
# inspect July data frame
str(rides_202207)
summary(rides_202207)

rides_202207 %>% # locate rides with no end coordinates
  filter(is.na(end_lat) | is.na(end_lng))

rides_202207$start_datetime <- ymd_hms(rides_202207$started_at) # create start datetime variable
rides_202207$end_datetime <- ymd_hms(rides_202207$ended_at) # create end datetime variable

rides_202207$ride_length <- as.numeric(rides_202207$end_datetime - rides_202207$start_datetime) # create column ride_length (in seconds) as numeric 

rides_202207 %>% 
  filter(ride_length < 60 | ride_length > 86400) # filter out too-short, too-long rides

rides_202207$ride_date <- as.Date(rides_202207$start_datetime) # Date format
rides_202207$ride_hour <- hour(rides_202207$start_datetime) # hour as integer
rides_202207$ride_month <- month(rides_202207$start_datetime, label = TRUE) # month as ord factor w/ 12 levels
rides_202207$ride_weekday <- wday(rides_202207$start_datetime, label = TRUE) # weekday  as ord factor w/ 7 levels

rides_202207_v2 <- rides_202207[!(rides_202207$ride_length < 60 |  # filter out too-short rides
                                    rides_202207$ride_length > 86400 | # filter out too-long rides
                                    is.na(rides_202207$end_lat) |  # filter out rides with no end latitude
                                    is.na(rides_202207$end_lng)), # filter out rides with no end longitude
]
rides_202207_v2 <- rides_202207_v2 %>% 
  select(-c(started_at, ended_at)) # remove started_at, ended_at columns

rm(rides_202207) # remove old data frame


###
# inspect August data frame
str(rides_202208)
summary(rides_202208)

rides_202208 %>% # locate rides with no end coordinates
  filter(is.na(end_lat) | is.na(end_lng))

rides_202208$start_datetime <- ymd_hms(rides_202208$started_at) # create start datetime variable
rides_202208$end_datetime <- ymd_hms(rides_202208$ended_at) # create end datetime variable

rides_202208$ride_length <- as.numeric(rides_202208$end_datetime - rides_202208$start_datetime) # create column ride_length (in seconds) as numeric 

rides_202208 %>% 
  filter(ride_length < 60 | ride_length > 86400) # filter out too-short, too-long rides

rides_202208$ride_date <- as.Date(rides_202208$start_datetime) # Date format
rides_202208$ride_hour <- hour(rides_202208$start_datetime) # hour as integer
rides_202208$ride_month <- month(rides_202208$start_datetime, label = TRUE) # month as ord factor w/ 12 levels
rides_202208$ride_weekday <- wday(rides_202208$start_datetime, label = TRUE) # weekday  as ord factor w/ 7 levels

rides_202208_v2 <- rides_202208[!(rides_202208$ride_length < 60 |  # filter out too-short rides
                                    rides_202208$ride_length > 86400 | # filter out too-long rides
                                    is.na(rides_202208$end_lat) |  # filter out rides with no end latitude
                                    is.na(rides_202208$end_lng)), # filter out rides with no end longitude
]
rides_202208_v2 <- rides_202208_v2 %>% 
  select(-c(started_at, ended_at)) # remove started_at, ended_at columns

rm(rides_202208) # remove old data frame

###
# inspect September data frame
str(rides_202209)
summary(rides_202209)

rides_202209 %>% # locate rides with no end coordinates
  filter(is.na(end_lat) | is.na(end_lng))

rides_202209$start_datetime <- ymd_hms(rides_202209$started_at) # create start datetime variable
rides_202209$end_datetime <- ymd_hms(rides_202209$ended_at) # create end datetime variable

rides_202209$ride_length <- as.numeric(rides_202209$end_datetime - rides_202209$start_datetime) # create column ride_length (in seconds) as numeric 

rides_202209 %>% 
  filter(ride_length < 60 | ride_length > 86400) # filter out too-short, too-long rides

rides_202209$ride_date <- as.Date(rides_202209$start_datetime) # Date format
rides_202209$ride_hour <- hour(rides_202209$start_datetime) # hour as integer
rides_202209$ride_month <- month(rides_202209$start_datetime, label = TRUE) # month as ord factor w/ 12 levels
rides_202209$ride_weekday <- wday(rides_202209$start_datetime, label = TRUE) # weekday  as ord factor w/ 7 levels

rides_202209_v2 <- rides_202209[!(rides_202209$ride_length < 60 |  # filter out too-short rides
                                    rides_202209$ride_length > 86400 | # filter out too-long rides
                                    is.na(rides_202209$end_lat) |  # filter out rides with no end latitude
                                    is.na(rides_202209$end_lng)), # filter out rides with no end longitude
]
rides_202209_v2 <- rides_202209_v2 %>% 
  select(-c(started_at, ended_at)) # remove started_at, ended_at columns

rm(rides_202209) # remove old data frame


###
# inspect October data frame
str(rides_202210)
summary(rides_202210)

rides_202210 %>% # locate rides with no end coordinates
  filter(is.na(end_lat) | is.na(end_lng))

rides_202210$start_datetime <- ymd_hms(rides_202210$started_at) # create start datetime variable
rides_202210$end_datetime <- ymd_hms(rides_202210$ended_at) # create end datetime variable

rides_202210$ride_length <- as.numeric(rides_202210$end_datetime - rides_202210$start_datetime) # create column ride_length (in seconds) as numeric 

rides_202210 %>% 
  filter(ride_length < 60 | ride_length > 86400) # filter out too-short, too-long rides

rides_202210$ride_date <- as.Date(rides_202210$start_datetime) # Date format
rides_202210$ride_hour <- hour(rides_202210$start_datetime) # hour as integer
rides_202210$ride_month <- month(rides_202210$start_datetime, label = TRUE) # month as ord factor w/ 12 levels
rides_202210$ride_weekday <- wday(rides_202210$start_datetime, label = TRUE) # weekday  as ord factor w/ 7 levels

rides_202210_v2 <- rides_202210[!(rides_202210$ride_length < 60 |  # filter out too-short rides
                                    rides_202210$ride_length > 86400 | # filter out too-long rides
                                    is.na(rides_202210$end_lat) |  # filter out rides with no end latitude
                                    is.na(rides_202210$end_lng)), # filter out rides with no end longitude
]
rides_202210_v2 <- rides_202210_v2 %>% 
  select(-c(started_at, ended_at)) # remove started_at, ended_at columns

rm(rides_202210) # remove old data frame


###
# Inspect November data frame
str(rides_202211)
summary(rides_202211)

rides_202211 %>% # locate rides with no end coordinates
  filter(is.na(end_lat) | is.na(end_lng))

rides_202211$start_datetime <- ymd_hms(rides_202211$started_at) # create start datetime variable
rides_202211$end_datetime <- ymd_hms(rides_202211$ended_at) # create end datetime variable

rides_202211$ride_length <- as.numeric(rides_202211$end_datetime - rides_202211$start_datetime) # create column ride_length (in seconds) as numeric 


# find rides that start before 2AM and end after 2AM 11/06/2022
rides_202211 %>% 
  filter(start_datetime < '2022-11-06 02:00:00' & end_datetime > '2022-11-06 02:00:00')

# cleaning cont'd
rides_202211 %>% 
  filter(ride_length < 60 | ride_length > 86400) # filter out too-short, too-long rides

rides_202211$ride_date <- as.Date(rides_202211$start_datetime) # Date format
rides_202211$ride_hour <- hour(rides_202211$start_datetime) # hour as integer
rides_202211$ride_month <- month(rides_202211$start_datetime, label = TRUE) # month as ord factor w/ 12 levels
rides_202211$ride_weekday <- wday(rides_202211$start_datetime, label = TRUE) # weekday  as ord factor w/ 7 levels

rides_202211_v2 <- rides_202211[!(rides_202211$ride_length < 60 |  # filter out too-short rides
                                    rides_202211$ride_length > 86400 | # filter out too-long rides
                                    (rides_202211$start_datetime < '2022-11-06 02:00:00' & rides_202211$end_datetime > '2022-11-06 02:00:00') |  # filter out rides that cross DST
                                    is.na(rides_202211$end_lat) | # filter out rides with no end latitude
                                    is.na(rides_202211$end_lng)),] # filter out rides with no end longitude
rides_202211_v2 <- rides_202211_v2 %>% 
  select(-c(started_at, ended_at))
rm(rides_202211)


###
# inspect December data frame
str(rides_202212)
summary(rides_202212)

rides_202212 %>% # locate rides with no end coordinates
  filter(is.na(end_lat) | is.na(end_lng))

rides_202212$start_datetime <- ymd_hms(rides_202212$started_at) # create start datetime variable
rides_202212$end_datetime <- ymd_hms(rides_202212$ended_at) # create end datetime variable

rides_202212$ride_length <- as.numeric(rides_202212$end_datetime - rides_202212$start_datetime) # create column ride_length (in seconds) as numeric 

rides_202212 %>% 
  filter(ride_length < 60 | ride_length > 86400) # filter out too-short, too-long rides

rides_202212$ride_date <- as.Date(rides_202212$start_datetime) # Date format
rides_202212$ride_hour <- hour(rides_202212$start_datetime) # hour as integer
rides_202212$ride_month <- month(rides_202212$start_datetime, label = TRUE) # month as ord factor w/ 12 levels
rides_202212$ride_weekday <- wday(rides_202212$start_datetime, label = TRUE) # weekday  as ord factor w/ 7 levels

rides_202212_v2 <- rides_202212[!(rides_202212$ride_length < 60 |  # filter out too-short rides
                                     rides_202212$ride_length > 86400 | # filter out too-long rides
                                     is.na(rides_202212$end_lat) |  # filter out rides with no end latitude
                                     is.na(rides_202212$end_lng)), # filter out rides with no end longitude
]
rides_202212_v2 <- rides_202212_v2 %>% 
  select(-c(started_at, ended_at)) # remove started_at, ended_at columns

rm(rides_202212) # remove old data frame


###
# create a data frame combining months into a single year, export as csv
rides_all <- rbind(rides_202201_v2,
                   rides_202202_v2,
                   rides_202203_v2,
                   rides_202204_v2, 
                   rides_202205_v2, 
                   rides_202206_v2,
                   rides_202207_v2, 
                   rides_202208_v2, 
                   rides_202209_v2, 
                   rides_202210_v2, 
                   rides_202211_v2, 
                   rides_202212_v2)
write.csv(rides_all, file = "~/Documents/datasets/rides_all.csv")