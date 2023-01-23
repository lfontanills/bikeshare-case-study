## Observation 1: Casual riders ride more often on weekends and during summer

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
    color = "User type") +
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
    fill = "User type" ) +
  scale_y_continuous(labels = scales::comma) +
  theme_bw() + 
  scale_fill_brewer(palette = "Paired")


## Observation 2: Casual riders use the same few stations

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

## Observation 3: Casual riders use bikes more often on off-peak times, and for longer

# Summarize number of rides each hour (quick visualization)
rides_all %>% 
  group_by(ride_hour, member_casual) %>% 
  summarize(total_count=n()) %>% 
  ggplot(aes(x=ride_hour, y=total_count, fill=member_casual)) +
  geom_col(position = "dodge") + geom_smooth()

# Ride duration, max to 90 minutes
rides_all %>% 
  filter(ride_length < (60*90)) %>% 
  ggplot(mapping = aes(ride_length, member_casual)) + 
  geom_violin() + 
  stat_summary(fun.y=median, geom="point", size=2, color="red") +
  labs(
    title = "Ride duration by user type",
    subtitle = "For all trips between 1 minute and 24 hours long",
    x = "Ride duration (seconds)", 
    y = "User type") +
  theme_bw()

## Observation 4: Casual riders use more classic bikes (iffy, because we got rid of docked)
