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

# Public holiday: Jan 2, 16
rides_202201_v2 %>% 
  group_by(member_casual, ride_day) %>% 
  summarize(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, ride_day) %>% 
  ggplot(aes(x=ride_day, y=number_of_rides, fill=member_casual)) +
  geom_col(position = "dodge")
