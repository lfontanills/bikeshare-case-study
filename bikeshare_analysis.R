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

## Trying out visualizations

# ggplot(data = <DATA>, mapping = aes(x = <X_VARIABLE>, y = <Y_VARIABLE>)) +
# <GEOM_FUNCTION>()


# Average duration
length_plot <-
  rides_all_clean %>% 
  group_by(member_casual) %>% 
  summarize(number_of_rides = n(), mean_length = mean(ride_length)) %>% 
  arrange(member_casual) %>% 
  ggplot(aes(x=member_casual, y=mean_length, fill=member_casual)) +
  geom_col(position = "dodge") +
  labs(
    title = "Average ride duration",
    subtitle = "For all trips between 1 minute and 24 hours long",
    x = "User type", 
    y = "Average ride duration (seconds)",
    fill = "User type") +
  scale_x_discrete(labels = c("classic bike", "electric bike")) +
  scale_y_continuous(labels = scales::comma) +
  theme_bw() + 
  scale_fill_brewer(palette = "Greens", direction = -1)

# Number of rides by time of week
weekday_plot <-
  rides_all_clean %>% 
  mutate(weekday = wday(started_at, label = TRUE, abbr = FALSE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarize(count=n()) %>% 
  ggplot(aes(x=weekday, y=count, fill=member_casual))+
  geom_col(position="dodge") +
  labs(
    title = "Number of rides by weekday",
    subtitle = "For all trips between 1 minute and 24 hours long",
    x = "User type", 
    y = "Number of rides",
    fill = "User type") +
  theme_bw() + 
  scale_fill_brewer(palette = "Greens", direction = -1)


# Ride duration, all rides
rides_all_clean %>% 
  ggplot(mapping = aes(ride_length, member_casual)) + 
  geom_violin() + 
  xlim(60, 5400) +
  stat_summary(fun=mean, color = "blue", linewidth = 2, size = 0.1) +
  labs(
    title = "Ride duration by user type",
    subtitle = "All trips between 60s and 24h long, x-axis truncated",
    x = "Ride duration (seconds)", 
    y = "User type") +
  theme_bw()

# Number of rides, hour of day
rides_all_clean %>% 
  mutate(hour = hour(started_at)) %>% 
  group_by(member_casual, hour) %>% 
  summarize(count=n()) %>% 
  ggplot(aes(x=hour, y=count, fill=member_casual))+
  geom_col(position="dodge") +
  labs(
    title = "Number of rides by weekday",
    subtitle = "For all trips between 1 minute and 24 hours long",
    x = "User type", 
    y = "Number of rides",
    fill = "User type") +
  theme_bw() + 
  scale_fill_brewer(palette = "Greens", direction = -1)

# Month-to-month
rides_all_clean %>% 
  mutate(month = month(started_at, label = TRUE, abbr = FALSE)) %>% 
  group_by(member_casual, month) %>% 
  summarize(number_of_rides = n()) %>% 
  arrange(member_casual, month) %>% 
  ggplot(aes(x=month, y=number_of_rides, fill=member_casual)) +
  geom_col(position = "dodge") +
  labs(
    title = "Number of rides per month",
    x = "Month", 
    y = "Number of rides",
    fill = "User type") +
  scale_y_continuous(labels = scales::comma) +
  theme_bw() + 
  scale_fill_brewer(palette = "Paired")

# By day of week
rides_all_clean %>% 
  mutate(weekday = wday(started_at, label = TRUE, abbr = FALSE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarize(number_of_rides = n()) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x=weekday, y=number_of_rides, fill=member_casual)) +
  geom_col(position = "dodge") +
  labs(
    title = "Number of rides per weekday",
    x = "Day of week", 
    y = "Number of rides",
    fill = "User type" ) +
  scale_y_continuous(labels = scales::comma) +
  theme_bw() + 
  scale_fill_brewer(palette = "Paired")

# Public holiday: May 29 - Sunday
rides_202205_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE, abbr = FALSE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarize(number_of_rides = n()) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x=weekday, y=number_of_rides, fill=member_casual)) +
  geom_col(position = "dodge")

# Public holiday: July 4 - Monday
rides_202207_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE, abbr = FALSE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarize(number_of_rides = n()) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x=weekday, y=number_of_rides, fill=member_casual)) +
  geom_col(position = "dodge")

# Public holiday: September 4 - Monday
rides_202210_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE, abbr = FALSE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarize(number_of_rides = n()) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x=weekday, y=number_of_rides, fill=member_casual)) +
  geom_col(position = "dodge")

# Visualizations for ride duration
# Month-to-month
rides_all_clean %>% 
  mutate(month = month(started_at, label = TRUE, abbr = FALSE)) %>% 
  group_by(member_casual, month) %>% 
  summarize(number_of_rides = n(), median_duration = median(ride_length)) %>% 
  arrange(member_casual, month) %>% 
  ggplot(aes(x=month, y=median_duration, fill=member_casual)) +
  geom_col(position = "dodge") +
  labs(
    title = "Median ride duration",
    subtitle = "For all trips between 1 minute and 24 hours long",
    x = "Month", 
    y = "Median ride duration (seconds)",
    fill = "User type") +
  scale_y_continuous(labels = scales::comma) +
  theme_bw() + 
  scale_fill_brewer(palette = "Greens")

# By day of week
rides_all_clean %>% 
  mutate(weekday = wday(started_at, label = TRUE, abbr = FALSE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarize(number_of_rides = n(), median_duration = median(ride_length)) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x=weekday, y=median_duration, fill=member_casual)) +
  geom_col(position = "dodge") +
  labs(
    title = "Median ride duration",
    subtitle = "For all trips between 1 minute and 24 hours long",
    x = "Month", 
    y = "Median ride duration (seconds)",
    fill = "User type" ) +
  scale_y_continuous(labels = scales::comma) +
  theme_bw() + 
  scale_fill_brewer(palette = "Greens")

# Visualizations for bike types

# Number of rides
# Visualizations for bike types by month
rides_all_clean %>% 
  mutate(month = month(started_at, label = TRUE, abbr = FALSE)) %>% 
  group_by(member_casual, rideable_type) %>% 
  summarize(number_of_rides = n(), month) %>% 
  arrange(member_casual, rideable_type) %>% 
  ggplot(aes(x=rideable_type, y=number_of_rides, fill=member_casual)) +
  geom_col(position = "dodge") +
  facet_wrap(~month)+
  coord_flip() +
  labs(
    title = "Number of rides by bike type",
    subtitle = "For all trips between 1 minute and 24 hours long",
    x = "Bike type", 
    y = "Number of rides",
    fill = "User type") +
  scale_x_discrete(labels = c("classic bike", "electric bike")) +
  scale_y_continuous(labels = scales::comma) +
  theme_bw() + 
  scale_fill_brewer(palette = "Purples")

# Average duration
rides_all_clean %>% 
  group_by(member_casual) %>% 
  summarize(number_of_rides = n(), mean_length = mean(ride_length)) %>% 
  arrange(member_casual) %>% 
  ggplot(aes(x=member_casual, y=mean_length, fill=member_casual)) +
  geom_col(position = "dodge") +
  labs(
    title = "Average ride duration",
    subtitle = "For all trips between 1 minute and 24 hours long",
    x = "User type", 
    y = "Average ride duration (seconds)",
    fill = "User type") +
  scale_x_discrete(labels = c("classic bike", "electric bike")) +
  scale_y_continuous(labels = scales::comma) +
  theme_bw() + 
  scale_fill_brewer(palette = "Greens", direction = -1)
