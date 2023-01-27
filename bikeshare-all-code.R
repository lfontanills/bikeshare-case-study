## load packages

library(tidyverse) # data processing and analysis
library(lubridate) # wrangle dates and times
#####
## Data Wrangling

# import csv files as data frames
rides_202201 <- read.csv("202201-divvy-tripdata.csv") # January 2022
rides_202202 <- read.csv("202202-divvy-tripdata.csv") # February 2022
rides_202203 <- read.csv("202203-divvy-tripdata.csv") # March 2022
rides_202204 <- read.csv("202204-divvy-tripdata.csv") # April 2022
rides_202205 <- read.csv("202205-divvy-tripdata.csv") # May 2022
rides_202206 <- read.csv("202206-divvy-tripdata.csv") # June 2022
rides_202207 <- read.csv("202207-divvy-tripdata.csv") # July 2022
rides_202208 <- read.csv("202208-divvy-tripdata.csv") # August 2022
rides_202209 <- read.csv("202209-divvy-publictripdata.csv") # September 2022
rides_202210 <- read.csv("202210-divvy-tripdata.csv") # October 2022
rides_202211 <- read.csv("202211-divvy-tripdata.csv") # November 2022
rides_202212 <- read.csv("202212-divvy-tripdata.csv") # December 2022

# create a combined data frame for all rides over 12-month period
rides_all_raw <- rbind(rides_202201,
                       rides_202202,
                       rides_202203,
                       rides_202204, 
                       rides_202205, 
                       rides_202206,
                       rides_202207, 
                       rides_202208, 
                       rides_202209, 
                       rides_202210, 
                       rides_202211, 
                       rides_202212)

# write combined data frame as csv
write.csv(rides_all_raw, "~/Documents/projects/bikeshare-case-study/rides_all_raw.csv") 

#####
## Data cleaning

# load packages
library(tidyverse)
library(lubridate)

# inspect rides_2022 data set
glimpse(rides_2022)
head(rides_2022)
summary(rides_2022)

# Check that all ride ids are unique
n_distinct(rides_2022$ride_id) == nrow(rides_2022)


#########
## Ride times and duration

# Change started_at, ended_at from character to POSIXct 
rides_2022$started_at <- ymd_hms(rides_2022$started_at)
rides_2022$ended_at <- ymd_hms(rides_2022$ended_at)
glimpse(rides_2022)

# Create column ride_length for duration
rides_2022$ride_length <- as.numeric(rides_2022$ended_at - rides_2022$started_at)
summary(rides_2022)

# Identify rides with negative ride length
rides_2022 %>% 
  filter(ride_length < 0)

rides_2022 %>% 
  filter(started_at < '2022-11-06 02:00:00' & ended_at > '2022-11-06 02:00:00') %>% 
  select(started_at, ended_at, ride_length) %>% 
  head()

# Identify rides with 0 ride length

# ride_duration: find any rides that were shorter than 1 minute and longer than 24 hours
rides_2022 %>% 
  filter(ride_length < 60)


rides_2022 %>% 
  filter(ride_length > 60*60*24)

nrow(rides_2022[rides_2022$ride_length == 0, ])
nrow(rides_2022[rides_2022$ride_length == 60, ])
nrow(rides_2022[rides_2022$ride_length > (60*60*24), ])
nrow(rides_2022[rides_2022$ride_length > (60*60*3), ])


# see long and short rides by user type
rides_2022 %>% 
  filter(ride_length < 60) %>% 
  group_by(member_casual) %>% 
  summarize(num_rides=n()) %>% 
  mutate(pct_rides = 100* num_rides/sum(num_rides))

rides_2022 %>% 
  filter(ride_length > 60*60*3) %>% 
  group_by(member_casual) %>% 
  summarize(num_rides=n()) %>% 
  mutate(pct_rides = 100* num_rides/sum(num_rides))

rides_2022 %>% 
  filter(ride_length > 60*60*24) %>% 
  group_by(member_casual) %>% 
  summarize(num_rides=n()) %>% 
  mutate(pct_rides = 100* num_rides/sum(num_rides))

## Bike types and locations

# See bike types for members and casual users 
rides_2022 %>% 
  group_by(member_casual, rideable_type) %>% 
  summarize(num_rides = n()) %>% 
  mutate(pct_rides = 100*(num_rides/sum(num_rides)))

nrow(rides_2022[rides_2022$rideable_type == "docked_bike", ])

# calculate percentage

# Learn more about docked_bike type
rides_2022 %>% 
  filter(rideable_type == "docked_bike") %>% 
  group_by(started_at) %>% 
  # arrange(started_at)
  # arrange(desc(started_at))
  
  # When were docked bikes taken out
  rides_2022 %>% 
  filter(rideable_type == "docked_bike") %>% 
  group_by(rideable_type) %>% 
  summarize(min_start = min(started_at), 
            max_start = max(started_at),
            min_end = min(ended_at),
            max_end = max(ended_at))

# Where were docked bikes taken out
rides_2022 %>% 
  filter(rideable_type == "docked_bike") %>% 
  group_by(start_station_name) %>% 
  summarize(num_rides=n()) %>% 
  # arrange(num_rides)
  arrange(desc(num_rides))

# Find docks with no start or end times
summary(rides_2022$started_at)
summary(rides_2022$ended_at)

# Check out ride starting locations
rides_2022 %>% 
  filter(start_station_name == "")

rides_2022 %>% 
  filter(start_station_id == "")

nrow(rides_2022[rides_2022$start_station_name == "", ]) # count rides w/o start station name
nrow(rides_2022[rides_2022$start_station_id == "", ]) # count rides w/o end_station_name
nrow(rides_2022[rides_2022$start_station_name == "" & rides_2022$start_station_id == "", ])

nrow(rides_2022[is.na(rides_2022$start_lat), ]) # count rides w/o start lat
nrow(rides_2022[is.na(rides_2022$start_lng), ]) # count rides w/o end lat

# Check if which bike types had start location data
rides_2022 %>% 
  filter(rideable_type == "docked_bike" & start_station_name == "")

rides_2022 %>% 
  filter(rideable_type == "classic_bike" & start_station_name == "")

rides_2022 %>% 
  filter(rideable_type == "electric_bike" & start_station_name == "")


# Check out ride ending locations

rides_2022 %>% 
  filter(end_station_name == "")

rides_2022 %>% 
  filter(end_station_id == "")

nrow(rides_2022[rides_2022$end_station_name == "", ]) # count rides w/o start station name
nrow(rides_2022[rides_2022$end_station_id == "", ]) # count rides w/o end_station_name
nrow(rides_2022[rides_2022$end_station_name == "" & rides_2022$end_station_id == "", ])

nrow(rides_2022[is.na(rides_2022$start_lat), ]) # count rides w/o start lat
nrow(rides_2022[is.na(rides_2022$start_lng), ]) # count rides w/o end lat


# Check if which bike types had end location data
rides_2022 %>% 
  filter(rideable_type == "docked_bike" & end_station_name == "")

rides_2022 %>% 
  filter(rideable_type == "classic_bike" & end_station_name == "")

rides_2022 %>% 
  filter(rideable_type == "electric_bike" & end_station_name == "")

# set city limits
city_lat <- c(41, 43)
city_lng <- c(-89, -87)

# find rides outside city area
rides_2022 %>% 
  filter((!between(start_lat,  city_lat[1], city_lat[2])) | 
           (!between(start_lng,  city_lng[1], city_lng[2]))) %>% 
  select(ride_id, start_station_name, start_lat, start_lng)

rides_2022 %>% 
  filter((!between(end_lat,  city_lat[1], city_lat[2])) | 
           (!between(end_lng,  city_lng[1], city_lng[2])) & end_lat != 0 & end_lng != 0) %>% 
  select(ride_id, end_station_name, end_lat, end_lng)

## create clean df

rides_clean <-rides_2022 %>% 
  filter(!is.na(start_lat)) %>% 
  filter(!is.na(end_lat)) %>% 
  filter(!(started_at < '2022-11-06 02:00:00' & ended_at > '2022-11-06 02:00:00')) %>% 
  filter(between(start_lat, city_lat[1], city_lat[2])) %>% 
  filter(between(end_lat, city_lat[1], city_lat[2])) %>% 
  select(ride_id, member_casual, rideable_type, started_at, ended_at, start_station_name, end_station_name, start_lat, start_lng, end_lat, end_lng, ride_length)

# export clean df as csv for analysis
write.csv(rides_clean, "~/Documents/projects/bikeshare-case-study/rides_clean.csv") 