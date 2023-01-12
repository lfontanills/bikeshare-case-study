# run after bikeshare_cleaning.R
str(rides_all)


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



# Descriptive analysis of RIDE DURATION by user type (member_casual)

# Quick look - compare ride duration over the entire year
aggregate(rides_all$ride_length ~ rides_all$member_casual, FUN = mean)
aggregate(rides_all$ride_length ~ rides_all$member_casual, FUN = median)
aggregate(rides_all$ride_length ~ rides_all$member_casual, FUN = max) 
aggregate(rides_all$ride_length ~ rides_all$member_casual, FUN = min) 

# Summarize ride duration each month (quick visualization)
rides_all %>% 
  group_by(ride_month, member_casual) %>% 
  summarize(median_ride_length = median(ride_length)) %>% 
  ggplot(aes(x=ride_month, y=median_ride_length, fill=member_casual)) +
  geom_col(position = "dodge")

# Summarize ride duration each weekday (quick visualization)
rides_all %>% 
  group_by(ride_weekday, member_casual) %>% 
  summarize(median_ride_length = median(ride_length)) %>% 
  ggplot(aes(x=ride_weekday, y=median_ride_length, fill=member_casual)) +
  geom_col(position = "dodge")

# Summarize ride duration each hour (quick visualization)
rides_all %>% 
  group_by(ride_hour, member_casual) %>% 
  summarize(median_ride_length = median(ride_length)) %>% 
  ggplot(aes(x=ride_hour, y=median_ride_length, fill=member_casual)) +
  geom_col(position = "dodge")




# compare bike types, ride length, create quick visualization
rides_all %>% 
  group_by(member_casual, rideable_type) %>% 
  summarize(sum(number_of_rides = n()), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, rideable_type) %>% 
  ggplot(aes(x=rideable_type, y=average_duration, fill=member_casual)) +
  geom_col(position = "dodge")

# compare bike types, number of rides,create quick visualization
rides_all %>% 
  group_by(member_casual, rideable_type) %>% 
  summarize(number_of_rides = n()) %>% 
  arrange(member_casual, rideable_type) %>% 
  ggplot(aes(x=rideable_type, y=number_of_rides, fill=member_casual)) +
  geom_col(position = "dodge")

# compare bike type, ride length, weekday
rides_all %>% 
  group_by(member_casual, rideable_type, ride_weekday) %>% 
  summarize(sum(number_of_rides = n()), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, rideable_type)

# compare bike type, number of rides, month of year
  rides_all %>% 
  group_by(member_casual, rideable_type, ride_month) %>% 
  summarize(sum(number_of_rides = n()), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, rideable_type)
  
# create summary data frames
avg_ride_length <- aggregate(rides_all$ride_length ~ rides_all$member_casual + rides_all$ride_weekday + rides_all$ride_month + rides_all$rideable_type, FUN = mean)
median_ride_length <- aggregate(rides_all$ride_length ~ rides_all$member_casual + rides_all$ride_weekday + rides_all$ride_month + rides_all$rideable_type, FUN = median)

# export as csv
write.csv(avg_ride_length, file = '~/Documents/datasets/avg_ride_length.csv')
write.csv(median_ride_length, file = '~/Documents/datasets/median_ride_length.csv')