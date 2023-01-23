library(tidyverse)
library(lubridate)

# Inspect raw data frame
str(rides_all_raw)
summary(rides_all_raw)

# ride_id: Check all ride ids are unique, TRUE = all unique, FALSE = duplicates exist
n_distinct(rides_all_raw$ride_id) == nrow(rides_all_raw)

# rideable_type: Find different bike types and % of all rides
rides_all_raw %>% 
  group_by(rideable_type) %>% 
  summarize(count =n()) %>% 
  mutate(percent = 100*(count/sum(count)))

# membership: Find rider types and % of all rides
rides_all_raw %>% 
  group_by(member_casual) %>% 
  summarize(count = n()) %>% 
  mutate(percent = 100*(count/sum))


# started_at and ended_at: Change type from chr to positx
rides_all_raw$started_at <- ymd_hms(rides_all_raw$started_at)
rides_all_raw$ended_at <- ymd_hms(rides_all_raw$ended_at)

# create new columns for ride_month (e.g. January), ride_weekday (e.g. Monday), ride_hour (as integer 0-24)
rides_all_raw$ride_month <- month(rides_all_raw$started_at, label = TRUE)
rides_all_raw$ride_weekday <- wday(rides_all_raw$started_at, label = TRUE)
rides_all_raw$ride_hour <- hour(rides_all_raw$started_at)

# ride_length: create new column for ride duration, add 1 hour to rides that started before 2AM and ended after 3AM on March 13th (DST begins)
rides_all_raw$ride_length_raw <- as.numeric(rides_all_raw$ended_at - rides_all_raw$started_at)
rides_all_raw <- mutate(rides_all_raw, ride_length = ifelse(started_at <= '2022-03-13 02:00:00' & ended_at >= '2022-03-13 03:00:00', ride_length_raw - 360, ride_length_raw))

# check it worked
rides_all_raw %>% 
  filter(started_at <= '2022-03-13 02:00:00' & ended_at >= '2022-03-13 03:00:00')

# ride_duration: find any rides that started before 2AM and ended after 2AM on November 6th (DST ends)
rides_all_raw %>% 
  filter(started_at < '2022-11-06 02:00:00' & ended_at > '2022-11-06 02:00:00')

# ride_duration: find any rides that were shorter than 1 minute and longer than 24 hours
rides_all_raw %>% 
  filter(ride_length < 60 | ride_length > (60*60*24)) # time in seconds

# start_station_name, end_station_name Check for rides with no start or end station name
rides_all_raw %>% 
  group_by(start_station_name) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count)) %>% 
  mutate(percent = 100*(count/sum(count)))

rides_all_raw %>% 
  group_by(end_station_name) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count)) %>% 
  mutate(percent = 100*(count/sum(count)))

# find any rides where station name absent & station id present, or station name present & station id absent
rides_all_raw %>% 
  filter(start_station_name != "" & start_station_id == "")

rides_all_raw %>% 
  filter(start_station_name == "" & start_station_id != "")

rides_all_raw %>% 
  filter(end_station_name != "" & end_station_id == "")

rides_all_raw %>% 
  filter(end_station_name == "" & end_station_id != "")

# start_lat: find any rides starting outside the Chicago area
# coordinates from https://www.google.com/search?q=divvy+chicago+map&oq=divvy+chicago+map&aqs=chrome..69i57.3411j0j4&sourceid=chrome&ie=UTF-8

city_lat <- c(41.6, 42.1)
city_lng <- c(-87.9, -87.5)

rides_all_raw %>% 
  filter((!between(start_lat,  city_lat[1], city_lat[2]) & start_station_name != "") | 
           (!between(start_lng,  city_lng[1], city_lng[2]) & start_station_name != "")) 

rides_all_raw %>% 
  filter((!between(end_lat,  city_lat[1], city_lat[2]) & end_station_name != "") | 
           (!between(end_lng,  city_lng[1], city_lng[2]) & end_station_name != "")) 


# create new data frame - clean
rides_all_clean <-rides_all_raw %>% 
  filter(rideable_type == "classic_bike" | rideable_type == "electric_bike") %>% 
  filter(between(ride_length, 60, (60*60*24))) %>% 
  filter(start_station_name != "") %>% 
  filter(end_station_name != "") %>% 
  filter(!(started_at < '2022-11-06 02:00:00' & ended_at > '2022-11-06 02:00:00')) %>% 
  filter(between(start_lat, city_lat[1], city_lat[2])) %>% 
  filter(between(end_lat, city_lat[1], city_lat[2]))

# export data frame as csv
write.csv(rides_all, '/Users/laurafontanills/Documents/projects/bikeshare-case-study/rides_all.csv', row.names = FALSE)

# see how many rides we have left to analyze  
nrow(rides_all_raw) - nrow(rides_all)
(nrow(rides_all_raw) - nrow(rides_all))/nrow(rides_all) * 100

# inspect clean data frame
summary(rides_all)

# check rideable types ok
rides_all %>% 
  group_by(rideable_type) %>% 
  summarize(count =n()) %>% 
  mutate(percent = 100*(count/sum(count)))

rides_all %>% 
  filter(end_station_name =="")
