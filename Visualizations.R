# Visualizations for ride length

# ggplot(data = <DATA>, mapping = aes(x = <X_VARIABLE>, y = <Y_VARIABLE>)) +
# <GEOM_FUNCTION>()

# Month-to-month
rides_all %>% 
  group_by(member_casual, ride_month) %>% 
  summarize(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
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

# Public holiday: May 29 - Sunday
rides_202205_v2 %>% 
  group_by(member_casual, ride_weekday) %>% 
  summarize(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, ride_weekday) %>% 
  ggplot(aes(x=ride_weekday, y=number_of_rides, fill=member_casual)) +
  geom_col(position = "dodge")

# Public holiday: July 4 - Monday
rides_202207_v2 %>% 
  group_by(member_casual, ride_weekday) %>% 
  summarize(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, ride_weekday) %>% 
  ggplot(aes(x=ride_weekday, y=number_of_rides, fill=member_casual)) +
  geom_col(position = "dodge")

# Public holiday: September 4 - Monday
rides_202210_v2 %>% 
  group_by(member_casual, ride_weekday) %>% 
  summarize(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, ride_weekday) %>% 
  ggplot(aes(x=ride_weekday, y=number_of_rides, fill=member_casual)) +
  geom_col(position = "dodge")