# What is the difference in ride duration for members and casual riders?

member_rides <- rides_all[rides_all$member_casual == "member",c(13,18)]
casual_rides <- rides_all[rides_all$member_casual == "casual", c(13, 18)]

wilcox.test(member_rides$ride_length, casual_rides$ride_length) # reject null hypothesis

t.test(member_rides$ride_length, casual_rides$ride_length)

# Just summer months

rides_all %>% 
  mutate(month = month(started_at, label = TRUE, abbr = FALSE)) %>% 
  filter(month == "June" | month == "July" |month == "August") %>% 
    group_by(member_casual) %>% 
  summarize(
    count = n(),
    median = median(ride_length, na.rm = TRUE),
    IQR = IQR(ride_length, na.rm = TRUE)
  ) 


# Ride counts by month, week, hour, bike type

rides_all %>% 
  mutate(month = month(started_at, label = TRUE, abbr = FALSE)) %>% 
  group_by(member_casual, month) %>% 
  summarize(count=n())

rides_all %>% 
  mutate(weekday = wday(started_at, label = TRUE, abbr = FALSE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarize(count=n())

rides_all %>% 
  mutate(hour = hour(started_at)) %>% 
  group_by(member_casual, hour) %>% 
  summarize(count=n())

rides_all %>% 
  group_by(member_casual, rideable_type) %>% 
  summarize(count=n())

# Ride durations by whole year, month, week, hour, bike

# Entire year
aggregate(rides_all$ride_length ~ rides_all$member_casual, FUN = mean)
aggregate(rides_all$ride_length ~ rides_all$member_casual, FUN = median)

rides_all %>% 
  mutate(month = month(started_at, label = TRUE, abbr = FALSE)) %>% 
  group_by(member_casual, month) %>% 
  summarize(median = median(ride_length))

rides_all %>% 
  mutate(weekday = wday(started_at, label = TRUE, abbr = FALSE)) %>% 
  group_by(member_casual, month) %>% 
  summarize(median = median(ride_length))

rides_all %>% 
  mutate(hour = hour(started_at)) %>% 
  group_by(member_casual, month) %>% 
  summarize(median = median(ride_length))

rides_all %>% 
  group_by(member_casual, rideable_type) %>% 
  summarize(median = median(ride_length))

# Ride counts by location
rides_all %>% 
  filter(member_casual == "casual") %>% 
  group_by(member_casual, start_station_name) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count)) %>% 
  mutate(percent = 100*(count/sum(count)))

rides_all %>% 
  filter(member_casual == "member") %>% 
  group_by(member_casual, start_station_name) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count)) %>% 
  mutate(percent = 100*(count/sum(count)))

# Bike types by top location
rides_all %>% 
  group_by(start_station_name, member_casual, rideable_type) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count)) %>% 
  filter(start_station_name == "Streeter Dr & Grand Ave") %>% 
  mutate(percent = 100*(count/sum(count)))
