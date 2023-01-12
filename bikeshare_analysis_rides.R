# Descriptive analysis of ride length by membership type

# all rider types, all trips
summary(rides_all$ride_length)

# compare members and casual users
aggregate(rides_all$ride_length ~ rides_all$member_casual, FUN = mean)
aggregate(rides_all$ride_length ~ rides_all$member_casual, FUN = median)
aggregate(rides_all$ride_length ~ rides_all$member_casual, FUN = max)
aggregate(rides_all$ride_length ~ rides_all$member_casual, FUN = min)

# compare average ride time per day of week
aggregate(rides_all$ride_length ~ rides_all$member_casual + rides_all$weekday, FUN = mean)

# compare average ride time by day of month
aggregate(rides_all$ride_length ~ rides_all$member_casual + rides_all$day, FUN = mean)

# compare average ride time by month
aggregate(rides_all$ride_length ~ rides_all$member_casual + rides_all$month, FUN = mean)

# compare ridership by weekday, create visualization
rides_all %>% 
  group_by(member_casual, weekday) %>% 
  summarize(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x=weekday, y=number_of_rides, fill=member_casual)) +
  geom_col(position = "dodge")

# compare ridership by month, create visualization
rides_all %>% 
  group_by(member_casual, month) %>% 
  summarize(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, month) %>% 
  ggplot(aes(x=month, y=number_of_rides, fill=member_casual)) +
  geom_col(position = "dodge")

# compare ride length by weekday, create visualization
rides_all %>% 
  group_by(member_casual, weekday) %>% 
  summarize(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x=weekday, y=average_duration, fill=member_casual)) +
  geom_col(position = "dodge")

# compare ride length by month, create visualization
rides_all %>% 
  group_by(member_casual, month) %>% 
  summarize(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, month) %>% 
  ggplot(aes(x=month, y=average_duration, fill=member_casual)) +
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
  group_by(member_casual, rideable_type, weekday) %>% 
  summarize(sum(number_of_rides = n()), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, rideable_type)

# compare bike type, number of rides, month of year
  rides_all %>% 
  group_by(member_casual, rideable_type, month) %>% 
  summarize(sum(number_of_rides = n()), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, rideable_type)
  
# create summary files
avg_ride_length <- aggregate(rides_all$ride_length ~ rides_all$member_casual + rides_all$weekday, FUN = mean)
median_ride_length <- aggregate(rides_all$ride_length ~ rides_all$member_casual + rides_all$weekday, FUN = median)

# export as csv
write.csv(avg_ride_length, file = '~/Documents/datasets/avg_ride_length.csv')
write.csv(median_ride_length, file = '~/Documents/datasets/median_ride_length.csv')
write.csv(bike_types, file = '~/Documents/datasets/bike_types.csv')