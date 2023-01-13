# run after bikeshare_cleaning.R
str(rides_all)


###
# Descriptive analysis of NO. OF RIDES by user type (member_casual)

# Summarize number of rides each month (quick visualization)
rides_all %>% 
  group_by(ride_month, member_casual) %>% 
  summarize(total_count=n()) %>% 
  ggplot(aes(x=ride_month, y=total_count, fill=member_casual)) +
  geom_col(position = "dodge")

# Summarize number of rides each weekday (quick visualization)
rides_all %>% 
  group_by(ride_weekday, member_casual) %>% 
  summarize(total_count=n()) %>% 
  ggplot(aes(x=ride_weekday, y=total_count, fill=member_casual)) +
  geom_col(position = "dodge")

# Summarize number of rides each hour (quick visualization)
rides_all %>% 
  group_by(ride_hour, member_casual) %>% 
  summarize(total_count=n()) %>% 
  ggplot(aes(x=ride_hour, y=total_count, fill=member_casual)) +
  geom_col(position = "dodge") + geom_smooth()


###
# Descriptive analysis of RIDE DURATION by user type (member_casual)

# Quick look - compare ride duration over the entire year
aggregate(rides_all$ride_length ~ rides_all$member_casual, FUN = mean)
aggregate(rides_all$ride_length ~ rides_all$member_casual, FUN = median)
aggregate(rides_all$ride_length ~ rides_all$member_casual, FUN = max) 
aggregate(rides_all$ride_length ~ rides_all$member_casual, FUN = min) 

# Summarize ride duration each month (quick visualization)
rides_all %>% 
  group_by(member_casual, ride_month) %>% 
  summarize(median_ride_length = median(ride_length)) %>% 
  ggplot(aes(x=ride_month, y=median_ride_length, fill=member_casual)) +
  geom_col(position = "dodge")

# Summarize ride duration each weekday (quick visualization)
rides_all %>% 
  group_by(member_casual, ride_weekday) %>% 
  summarize(median_ride_length = median(ride_length)) %>% 
  ggplot(aes(x=ride_weekday, y=median_ride_length, fill=member_casual)) +
  geom_col(position = "dodge")

# Summarize ride duration each hour (quick visualization)
rides_all %>% 
  group_by(member_casual, ride_hour) %>% 
  summarize(median_ride_length = median(ride_length)) %>% 
  ggplot(aes(x=ride_hour, y=median_ride_length, fill=member_casual)) +
  geom_col(position = "dodge")


###
# Descriptive analysis of BIKE TYPES by user type (member_casual)

# Summarize number of rides per bike type by user type over the entire year
# Note - small % of rides are docked_bike, all casual users
rides_all %>% 
  group_by(member_casual, rideable_type) %>% 
  summarize(total_count=n()) %>% 
  ggplot(aes(x=rideable_type, y=total_count, fill=member_casual)) +
  geom_col(position = "dodge")

# Summarize ride duration per bike type by user type over the entire year
# Note - docked_bike rides account for very long ride durations
rides_all %>% 
  group_by(member_casual, rideable_type) %>% 
  summarize(median_ride_length = median(ride_length)) %>% 
  ggplot(aes(x=rideable_type, y=median_ride_length, fill=member_casual)) +
  geom_col(position = "dodge")

# Summarize number of rides per bike type by user type over each month
# Note - filtering out docked_bike
# Note - visualizing takes forever, not useful
rides_all %>% 
  filter(rideable_type != "docked_bike") %>% 
  group_by(member_casual, rideable_type, ride_month) %>% 
  summarize(total_count=n(), member_casual) %>% 
  arrange(member_casual, rideable_type) %>% 
  ggplot(aes(x=ride_month, y=total_count, fill=rideable_type)) +
  facet_wrap(~member_casual) +
  geom_col(position = "dodge")

# Summarize number of rides per bike type by user type over each weekday
# Note - filtering out docked_bike
# Note - visualizing takes forever, not useful
rides_all %>% 
  filter(rideable_type != "docked_bike") %>% 
  group_by(member_casual, rideable_type, ride_weekday) %>% 
  summarize(total_count=n(), member_casual) %>% 
  arrange(member_casual, rideable_type) %>% 
  ggplot(aes(x=ride_weekday, y=total_count, fill=rideable_type)) +
  facet_wrap(~member_casual) +
  geom_col(position = "dodge")


###
# Subset ride time, types from rides_all and export as csv
colnames(rides_all)
rides_subset <- rides_all[,c(2, 11:18)]
write_csv(rides_subset, file = "~/Documents/datasets/rides_subset.csv")

# Subset location data and export as csv
colnames(rides_all)
rides_location <- rides_all[,c(3:12)]