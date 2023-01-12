# Visualizations for ride length

# ggplot(data = <DATA>, mapping = aes(x = <X_VARIABLE>, y = <Y_VARIABLE>)) +
# <GEOM_FUNCTION>()

# Month-to-month
rides_all %>% 
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
    fill = "User type"
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_bw() + 
  scale_fill_brewer(palette = "Paired")

# Whole year
rides_all$ride_date <- as.Date(rides_all$start_datetime)
rides_all %>% 
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
    color = "User type"
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_bw() + 
  scale_color_brewer(palette = "Paired")

# By day of week
rides_all %>% 
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
    fill = "User type"
  ) +
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
rides_all %>% 
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
    fill = "User type"
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_bw() + 
  scale_fill_brewer(palette = "Greens")

# Whole year
rides_all %>% 
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
    fill = "User type"
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_bw() + 
  scale_color_brewer(palette = "Greens")

# By day of week
rides_all %>% 
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
    fill = "User type"
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_bw() + 
  scale_fill_brewer(palette = "Greens")

# Visualizations for bike types

# Number of rides
# Visualizations for bike types by month
rides_all %>% 
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
    fill = "User type"
  ) +
  scale_x_discrete(labels = c("classic bike", "electric bike")) +
  scale_y_continuous(labels = scales::comma) +
  theme_bw() + 
  scale_fill_brewer(palette = "Purples")

# Median duration
rides_all %>% 
  filter(rideable_type != "docked_bike") %>% 
  group_by(member_casual, rideable_type) %>% 
  summarize(number_of_rides = n(), median_duration = median(ride_length)) %>% 
  arrange(member_casual, rideable_type) %>% 
  ggplot(aes(x=rideable_type, y=median_duration, fill=member_casual)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "Median ride duration by bike type",
    subtitle = "For all trips between 1 minute and 24 hours long",
    x = "Bike type", 
    y = "Median ride duration (seconds)",
    fill = "User type"
  ) +
  scale_x_discrete(labels = c("classic bike", "electric bike")) +
  scale_y_continuous(labels = scales::comma) +
  theme_bw() + 
  scale_fill_brewer(palette = "Purples")
