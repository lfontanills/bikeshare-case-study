####
library(tidyverse)
library(lubridate)


## Ride durations

rides_clean %>% 
  group_by(member_casual) %>% 
  summarize(num_rides=n()) %>% 
  ggplot(aes(x=member_casual, y=num_rides, fill=member_casual)) +
  geom_col(position="dodge")

rides_clean %>% 
  group_by(member_casual) %>% 
  summary(rides_clean$ride_length)

tapply(rides_clean$ride_length, rides_clean$member_casual, summary)

# check if normal distribution


ggplot(rides_clean, aes(x = ride_length)) +
  geom_histogram() +
  facet_wrap(~ member_casual) +
  labs(
    x = "ride length", 
    y = "number of rides") +
  scale_y_continuous(labels = scales::comma)


users_length <- subset(rides_clean, member_casual = 'member', select=c('member_casual', 'ride_length'))

wilcox.test(users_length$ride_length ~ users_length$member_casual)

# graph ride durations

length_plot <-
  rides_clean %>% 
  group_by(member_casual) %>% 
  summarize(number_of_rides = n(), mean_length = mean(ride_length)) %>% 
  arrange(member_casual) %>% 
  ggplot(aes(x=member_casual, y=mean_length, fill=member_casual)) +
  geom_col(position = "dodge") +
  labs(
    title = "Average ride duration",
    subtitle = "For all trips between 1 minute and 3 hours long",
    x = "User type", 
    y = "Average ride duration (seconds)",
    fill = "User type") +
  scale_x_discrete(labels = c("classic bike", "electric bike")) +
  scale_y_continuous(labels = scales::comma) +
  theme_bw() + 
  scale_fill_brewer(palette = "Greens", direction = -1)

## Ride times

# ride counts by time period

rides_clean %>% 
  group_by(member_casual) %>% 
  summarize(count=n()) %>% 
  ggplot(aes(x=member_casual, y=count, fill=member_casual)) +
  geom_col(position="dodge")

rides_clean %>% 
  mutate(month = month(started_at, label = TRUE, abbr = FALSE)) %>% 
  filter(month == "June" | month == "July" |month == "August") %>% 
  group_by(member_casual) %>% 
  summarize(
    count = n(),
    median = median(ride_length, na.rm = TRUE),
    IQR = IQR(ride_length, na.rm = TRUE)
  ) 

# by month of year
rides_clean %>% 
  mutate(month = month(started_at, label = TRUE, abbr = FALSE)) %>% 
  group_by(member_casual, month) %>% 
  summarize(count=n()) %>% 
  ggplot(aes(x=month, y=count, fill=member_casual))+
  geom_col(position="dodge")

# by day of week
weekday_plot <-
  rides_clean %>% 
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
  scale_y_continuous(labels = scales::comma) +
  theme_bw() + 
  scale_fill_brewer(palette = "Greens", direction = -1)

# by hour of day

rides_clean %>% 
  mutate(hour = hour(started_at)) %>% 
  group_by(member_casual, hour) %>% 
  summarize(count=n()) %>% 
  ggplot(aes(x=hour, y=count, fill=member_casual))+
  geom_col(position="dodge")


## Ride locations

casual_stations<-
  rides_clean %>% 
  filter(member_casual == "casual") %>% 
  group_by(member_casual, start_station_name) %>% 
  summarize(num_rides = n()) %>% 
  arrange(desc(num_rides)) %>% 
  mutate(percent_rides = 100*(num_rides/sum(num_rides))) %>% 
  head(10)

member_stations <-
  rides_clean %>% 
  filter(member_casual == "member") %>% 
  group_by(member_casual, start_station_name) %>% 
  summarize(num_rides = n()) %>% 
  arrange(desc(num_rides)) %>% 
  mutate(percent_rides = 100*(num_rides/sum(num_rides))) %>% 
  head(10)

top_stations <- rbind(casual_stations, member_stations)

locations <-rides_all_clean[c(5, 9:10)]

# create csvs -export for mapping
write_csv(top_stations, "~/Desktop/top_stations.csv")
write_csv(locations, "~/Desktop/locations.csv")

## Bike types

rides_clean %>% 
  group_by(member_casual, rideable_type) %>%
  summarize(num_rides = n()) %>% 
  mutate(pct_rides = 100* num_rides/sum(num_rides))
  

bike_type_plot <- rides_clean %>% 
  group_by(rideable_type, member_casual) %>% 
  filter(rideable_type == "classic_bike" | rideable_type == "electric_bike") %>% 
  summarize(num_rides = n()) %>% 
  arrange(member_casual) %>% 
  ggplot(aes(x=member_casual, y=num_rides, fill=rideable_type)) +
  geom_col(position = "dodge") +
  labs(
    title = "Number of rides with Classic Bikes, Electric Bikes",
    subtitle = "For all trips between 1 minute and 3 hours long",
    x = "Membership type", 
    y = "Number of rides",
    fill = "Bike type", 
    labels = c("classic_bike", "electric_bike")) +
  scale_y_continuous(labels = scales::comma) +
  theme_bw() + 
  scale_fill_brewer(palette = "Blues")

bike_type_plot


