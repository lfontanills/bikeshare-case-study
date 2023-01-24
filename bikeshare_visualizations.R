# Visualizations for ride length

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




# APPENDIX: Ride duration, all rides
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
  filter(rideable_type != "docked_bike") %>% 
  group_by(member_casual, ride_month) %>% 
  summarize(number_of_rides = n()) %>% 
  arrange(member_casual, ride_month) %>% 
  ggplot(aes(x=ride_month, y=number_of_rides, fill=member_casual)) +
  geom_col(position = "dodge") +
  labs(
    title = "Number of rides per month",
    x = "Month", 
    y = "Number of rides",
    fill = "User type") +
  scale_y_continuous(labels = scales::comma) +
  theme_bw() + 
  scale_fill_brewer(palette = "Paired")

# Whole year
rides_all_clean$ride_date <- as.Date(rides_all_clean$start_datetime)
rides_all_clean %>% 
  filter(rideable_type != "docked_bike") %>% 
  group_by(member_casual, ride_date) %>% 
  summarize(number_of_rides = n()) %>% 
  arrange(member_casual, ride_date) %>% 
  ggplot(aes(x=ride_date, y=number_of_rides, color=member_casual)) +
  geom_point() + geom_smooth() +
  labs(
    title = "Number of rides per day",
    x = "2022", 
    y = "Number of rides",
    color = "User type") +
  scale_y_continuous(labels = scales::comma) +
  theme_bw() + 
  scale_color_brewer(palette = "Paired")

# By day of week
rides_all_clean %>% 
  filter(rideable_type != "docked_bike") %>% 
  group_by(member_casual, ride_weekday) %>% 
  summarize(number_of_rides = n()) %>% 
  arrange(member_casual, ride_weekday) %>% 
  ggplot(aes(x=ride_weekday, y=number_of_rides, fill=member_casual)) +
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
  filter(rideable_type != "docked_bike") %>% 
  group_by(member_casual, ride_weekday) %>% 
  summarize(number_of_rides = n()) %>% 
  arrange(member_casual, ride_weekday) %>% 
  ggplot(aes(x=ride_weekday, y=number_of_rides, fill=member_casual)) +
  geom_col(position = "dodge")

# Public holiday: July 4 - Monday
rides_202207_v2 %>% 
  filter(rideable_type != "docked_bike") %>% 
  group_by(member_casual, ride_weekday) %>% 
  summarize(number_of_rides = n()) %>% 
  arrange(member_casual, ride_weekday) %>% 
  ggplot(aes(x=ride_weekday, y=number_of_rides, fill=member_casual)) +
  geom_col(position = "dodge")

# Public holiday: September 4 - Monday
rides_202210_v2 %>% 
  filter(rideable_type != "docked_bike") %>% 
  group_by(member_casual, ride_weekday) %>% 
  summarize(number_of_rides = n()) %>% 
  arrange(member_casual, ride_weekday) %>% 
  ggplot(aes(x=ride_weekday, y=number_of_rides, fill=member_casual)) +
  geom_col(position = "dodge")

# Visualizations for ride duration
# Month-to-month
rides_all_clean %>% 
  filter(rideable_type != "docked_bike") %>% 
  group_by(member_casual, ride_month) %>% 
  summarize(number_of_rides = n(), median_duration = median(ride_length)) %>% 
  arrange(member_casual, ride_month) %>% 
  ggplot(aes(x=ride_month, y=median_duration, fill=member_casual)) +
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

# Whole year
rides_all_clean %>% 
  filter(rideable_type != "docked_bike") %>% 
  group_by(member_casual, ride_date) %>% 
  summarize(number_of_rides = n(), median_duration = median(ride_length)) %>% 
  arrange(member_casual, ride_date) %>% 
  ggplot(aes(x=ride_date, y=median_duration, color=member_casual)) +
  geom_point() + geom_smooth() +
  labs(
    title = "Median ride duration",
    subtitle = "For all trips between 1 minute and 24 hours long",
    x = "Month", 
    y = "Median ride duration (seconds)",
    fill = "User type" ) +
  scale_y_continuous(labels = scales::comma) +
  theme_bw() + 
  scale_color_brewer(palette = "Greens")

# By day of week
rides_all_clean %>% 
  filter(rideable_type != "docked_bike") %>% 
  group_by(member_casual, ride_weekday) %>% 
  summarize(number_of_rides = n(), median_duration = median(ride_length)) %>% 
  arrange(member_casual, ride_weekday) %>% 
  ggplot(aes(x=ride_weekday, y=median_duration, fill=member_casual)) +
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
  group_by(member_casual, rideable_type) %>% 
  filter(rideable_type != "docked_bike") %>% 
  summarize(number_of_rides = n(), ride_month) %>% 
  arrange(member_casual, rideable_type) %>% 
  ggplot(aes(x=rideable_type, y=number_of_rides, fill=member_casual)) +
  geom_col(position = "dodge") +
  facet_wrap(~ride_month)+
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


