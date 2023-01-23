# Ride counts by month, week, hour, bike
rides_all %>% 
  group_by(member_casual, ride_month) %>% 
  summarize(count=n()) %>% 
  arrange(ride_month)



# Ride durations by month, week, hour, bike



# Ride counts by location



# Bike types by location