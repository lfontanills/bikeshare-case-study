

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
bike_types <- rides_all %>% 
  group_by(member_casual, rideable_type, weekday) %>% 
  summarize(sum(number_of_rides = n()), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, rideable_type)