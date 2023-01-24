###
# Descriptive analysis of NO. OF RIDES by user type (member_casual)

# Summarize number of rides each month (quick visualization)
rides_all_clean %>% 
  mutate(month = month(started_at)) %>% 
  group_by(month, member_casual) %>% 
  summarize(count=n()) %>% 
  ggplot(aes(x=month, y=count, fill=member_casual)) +
  geom_col(position = "dodge")

# Summarize number of rides each weekday (quick visualization)
rides_all_clean %>% 
  group_by(ride_weekday, member_casual) %>% 
  summarize(count=n()) %>% 
  ggplot(aes(x=ride_weekday, y=count, fill=member_casual)) +
  geom_col(position = "dodge")

# Summarize number of rides each hour (quick visualization)
rides_all_clean %>% 
  group_by(ride_hour, member_casual) %>% 
  summarize(total_count=n()) %>% 
  ggplot(aes(x=ride_hour, y=total_count, fill=member_casual)) +
  geom_col(position = "dodge") + geom_smooth()


###
# Descriptive analysis of RIDE DURATION by user type (member_casual)

# Quick look - compare ride duration over the entire year
aggregate(rides_all_clean$ride_length ~ rides_all_clean$member_casual, FUN = mean)
aggregate(rides_all_clean$ride_length ~ rides_all_clean$member_casual, FUN = median)
aggregate(rides_all_clean$ride_length ~ rides_all_clean$member_casual, FUN = max) 
aggregate(rides_all_clean$ride_length ~ rides_all_clean$member_casual, FUN = min) 

# Summarize ride duration each month (quick visualization)
rides_all_clean %>% 
  mutate(month = month(started_at, label = TRUE, abbr = FALSE)) %>% 
  group_by(member_casual, month) %>% 
  summarize(mean = mean(ride_length)) %>% 
  ggplot(aes(x=month, y=mean, fill=member_casual)) +
  geom_col(position = "dodge")

# Summarize ride duration each weekday (quick visualization)
rides_all_clean %>% 
  mutate(weekday = wday(started_at, label = TRUE, abbr = FALSE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarize(mean = mean(ride_length)) %>% 
  ggplot(aes(x=weekday, y=mean, fill=member_casual)) +
  geom_col(position = "dodge")

# Summarize ride duration each hour (quick visualization)
rides_all_clean %>% 
  mutate(hour = hour(started_at)) %>% 
  group_by(member_casual, hour) %>% 
  summarize(mean = mean(ride_length)) %>% 
  ggplot(aes(x=hour, y=mean, fill=member_casual)) +
  geom_col(position = "dodge")


###
# Descriptive analysis of BIKE TYPES by user type (member_casual)

# Summarize number of rides per bike type by user type over the entire year
# Note - small % of rides are docked_bike, all casual users
rides_all_clean %>% 
  group_by(member_casual, rideable_type) %>% 
  summarize(total_count=n()) %>% 
  ggplot(aes(x=rideable_type, y=total_count, fill=member_casual)) +
  geom_col(position = "dodge")

# Summarize ride duration per bike type by user type over the entire year
# Note - docked_bike rides account for very long ride durations
rides_all_clean %>% 
  group_by(member_casual, rideable_type) %>% 
  summarize(median_ride_length = median(ride_length)) %>% 
  ggplot(aes(x=rideable_type, y=median_ride_length, fill=member_casual)) +
  geom_col(position = "dodge")

# Summarize number of rides per bike type by user type over each month
# Note - filtering out docked_bike
# Note - visualizing takes forever, not useful
rides_all_clean %>% 
  mutate(month = month(started_at, label = TRUE, abbr = FALSE)) %>% 
  filter(rideable_type != "docked_bike") %>% 
  group_by(member_casual, rideable_type, month) %>% 
  summarize(total_count=n(), member_casual) %>% 
  arrange(member_casual, rideable_type)

# Summarize number of rides per bike type by user type over each weekday
# Note - filtering out docked_bike
# Note - visualizing takes forever, not useful
rides_all_clean %>% 
  mutate(weekday = wday(started_at, label = TRUE, abbr = FALSE)) %>% 
  filter(rideable_type != "docked_bike") %>% 
  group_by(member_casual, rideable_type, weekday) %>% 
  summarize(total_count=n(), member_casual) %>% 
  arrange(member_casual, rideable_type) 


###
# Find top 10 start, end locations by user type
rides_all_clean %>% 
  filter(member_casual == "casual") %>% 
  group_by(member_casual, start_station_name) %>% 
  summarize(total_count = n()) %>% 
  arrange(desc(total_count))

rides_all_clean %>% 
  filter(member_casual == "member") %>% 
  group_by(member_casual, start_station_name) %>% 
  summarize(total_count = n()) %>% 
  arrange(desc(total_count))

rides_all_clean %>% 
  filter(member_casual == "casual") %>% 
  group_by(member_casual, end_station_name) %>% 
  summarize(total_count = n()) %>% 
  arrange(desc(total_count))

rides_all %>% 
  filter(member_casual == "member") %>% 
  group_by(member_casual, end_station_name) %>% 
  summarize(total_count = n()) %>% 
  arrange(desc(total_count))


###
# Subset ride time, types from rides_all_clean and export as csv
colnames(rides_all_clean)
rides_subset <- rides_all_clean[,c(2, 11:18)]
# write_csv(rides_subset, file = "~/Documents/datasets/rides_subset.csv")

# Subset location data and export as csv
colnames(rides_all_clean)
rides_location <- rides_all_clean[,c(3:12)]
# write_csv(rides_location, file = "~/Documents/datasets/rides_location.csv")

# Number of rides, Just summer months

rides_all_clean %>% 
  mutate(month = month(started_at, label = TRUE, abbr = FALSE)) %>% 
  filter(month == "June" | month == "July" |month == "August") %>% 
  group_by(member_casual) %>% 
  summarize(
    count = n(),
    median = median(ride_length, na.rm = TRUE),
    IQR = IQR(ride_length, na.rm = TRUE)
  ) 


# Ride durations by whole year, month, week, hour, bike

# Entire year
aggregate(rides_all_clean$ride_length ~ rides_all_clean$member_casual, FUN = mean)
aggregate(rides_all_clean$ride_length ~ rides_all_clean$member_casual, FUN = median)

rides_all_clean %>% 
  mutate(month = month(started_at, label = TRUE, abbr = FALSE)) %>% 
  group_by(member_casual, month) %>% 
  summarize(median = median(ride_length))

rides_all_clean %>% 
  mutate(weekday = wday(started_at, label = TRUE, abbr = FALSE)) %>% 
  group_by(member_casual, month) %>% 
  summarize(median = median(ride_length))

rides_all_clean %>% 
  mutate(hour = hour(started_at)) %>% 
  group_by(member_casual, month) %>% 
  summarize(median = median(ride_length))

rides_all_clean %>% 
  group_by(member_casual, rideable_type) %>% 
  summarize(median = median(ride_length))

# Ride counts by location
rides_all_clean %>% 
  filter(member_casual == "casual") %>% 
  group_by(member_casual, start_station_name) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count)) %>% 
  mutate(percent = 100*(count/sum(count)))

rides_all_clean %>% 
  filter(member_casual == "member") %>% 
  group_by(member_casual, start_station_name) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count)) %>% 
  mutate(percent = 100*(count/sum(count)))

# Bike types by top location
rides_all_clean %>% 
  group_by(start_station_name, member_casual, rideable_type) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count)) %>% 
  filter(start_station_name == "Streeter Dr & Grand Ave") %>% 
  mutate(percent = 100*(count/sum(count)))