# run after bikeshare_cleaning.R
# Descriptive analysis of ride length, # rides by membership type

# all rider types, all trips
summary(rides_all$ride_length)

# compare members and casual users
aggregate(rides_all$ride_length ~ rides_all$member_casual, FUN = mean)
aggregate(rides_all$ride_length ~ rides_all$member_casual, FUN = median)
aggregate(rides_all$ride_length ~ rides_all$member_casual, FUN = max)
aggregate(rides_all$ride_length ~ rides_all$member_casual, FUN = min)

# compare average ride time per day of week
aggregate(rides_all$ride_length ~ rides_all$member_casual + rides_all$ride_weekday, FUN = mean)

# compare average ride time by day of month
aggregate(rides_all$ride_length ~ rides_all$member_casual + rides_all$ride_day, FUN = mean)

# compare average ride time by month
aggregate(rides_all$ride_length ~ rides_all$member_casual + rides_all$ride_month, FUN = mean)

# compare ridership by weekday, create visualization
rides_all %>% 
  group_by(member_casual, ride_weekday) %>% 
  summarize(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, ride_weekday) %>% 
  ggplot(aes(x=ride_weekday, y=number_of_rides, fill=member_casual)) +
  geom_col(position = "dodge")

# compare ridership by month, create visualization
rides_all %>% 
  group_by(member_casual, ride_month) %>% 
  summarize(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, ride_month) %>% 
  ggplot(aes(x=ride_month, y=number_of_rides, fill=member_casual)) +
  geom_col(position = "dodge")

# compare ride length by weekday, create visualization
rides_all %>% 
  group_by(member_casual, ride_weekday) %>% 
  summarize(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, ride_weekday) %>% 
  ggplot(aes(x=ride_weekday, y=average_duration, fill=member_casual)) +
  geom_col(position = "dodge")

# compare ride length by month, create visualization
rides_all %>% 
  group_by(member_casual, ride_month) %>% 
  summarize(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, ride_month) %>% 
  ggplot(aes(x=ride_month, y=average_duration, fill=member_casual)) +
  geom_col(position = "dodge")


# compare bike types, ride length
rides_all %>% 
  group_by(member_casual, rideable_type) %>% 
  summarize(sum(number_of_rides = n()), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, rideable_type) %>% 
  ggplot(aes(x=rideable_type, y=average_duration, fill=member_casual)) +
  geom_col(position = "dodge")

# compare bike types, number of rides
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