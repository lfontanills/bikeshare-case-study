# Descriptive analysis of rides per rideable type by membership type

# compare bike types, ride length, # rides
rides_all %>% 
  group_by(member_casual, rideable_type) %>% 
  summarize(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, rideable_type) %>% 
  ggplot(aes(x=rideable_type, y=average_duration, fill=member_casual)) +
  geom_col(position = "dodge")

# visualize # rides
rides_all %>% 
  group_by(member_casual, rideable_type) %>% 
  summarize(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, rideable_type) %>% 
  ggplot(aes(x=rideable_type, y=number_of_rides, fill=member_casual)) +
  geom_col(position = "dodge")

# compare bike type, ride length, weekday
rides_all %>% 
  group_by(member_casual, rideable_type,ride_weekday) %>% 
  summarize(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, rideable_type)

# compare bike type, ride length, month of year
rides_all %>% 
  group_by(member_casual, rideable_type, ride_month) %>% 
  summarize(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, rideable_type)

# compare bike type, number of rides, weekday
rides_all %>% 
  group_by(member_casual, rideable_type, ride_weekday) %>% 
  summarize(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, rideable_type)

# compare bike type, number of rides, month of year
rides_all %>% 
  group_by(member_casual, rideable_type, ride_month) %>% 
  summarize(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, rideable_type) 

# create summary files
bike_types <- rides_all %>% 
  group_by(member_casual, rideable_type, ride_weekday, ride_month) %>% 
  summarize(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, rideable_type)

# create csv
write.csv(bike_types, file = '~/Documents/datasets/bike_types.csv')