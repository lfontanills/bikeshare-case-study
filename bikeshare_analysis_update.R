#### 
# What is the difference in ride frequency for members and casual riders?

# ride counts by time period

rides_all_clean %>% 
  group_by(member_casual) %>% 
  summarize(count=n()) %>% 
  ggplot(aes(x=member_casual, y=count, fill=member_casual)) +
  geom_col(position="dodge")

rides_all_clean %>% 
  mutate(month = month(started_at, label = TRUE, abbr = FALSE)) %>% 
  filter(month == "June" | month == "July" |month == "August") %>% 
  group_by(member_casual) %>% 
  summarize(
    count = n(),
    median = median(ride_length, na.rm = TRUE),
    IQR = IQR(ride_length, na.rm = TRUE)
  ) 
  
rides_all_clean %>% 
  mutate(month = month(started_at, label = TRUE, abbr = FALSE)) %>% 
  group_by(member_casual, month) %>% 
  summarize(count=n()) %>% 
  ggplot(aes(x=month, y=count, fill=member_casual))+
  geom_col(position="dodge")

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
  scale_y_continuous(labels = scales::comma) +
  theme_bw() + 
  scale_fill_brewer(palette = "Greens", direction = -1)

rides_all_clean %>% 
  mutate(hour = hour(started_at)) %>% 
  group_by(member_casual, hour) %>% 
  summarize(count=n()) %>% 
  ggplot(aes(x=hour, y=count, fill=member_casual))+
  geom_col(position="dodge")


####
# What is the difference in ride duration for members and casual riders?

# summary statistics
rides_all_clean %>% 
  group_by(member_casual) %>% 
  summarize(min = min(ride_length),
            q1 = quantile(ride_length, 0.25),
            median = median(ride_length),
            mean = mean(ride_length),
            q3 = quantile(ride_length, 0.75),
            max = max(ride_length),
            sd = sd(ride_length)
            )

# check if normal distribution - NOPE
member_length_hist <-
hist(subset(rides_all_clean, member_casual == "member")$ride_length,
     main = "Ride lengths for members",
     xlab = "Ride length")

casual_length_hist <-
hist(subset(rides_all_clean, member_casual == "casual")$ride_length,
     main = "Ride lengths for casual users",
     xlab = "Ride length")

# statistical test
users_length <- subset(rides_all_clean, member_casual = 'member', select=c('member_casual', 'ride_length'))

wilcox.test(users_length$ride_length ~ users_length$member_casual) # reject null hypothesis

# visualize difference in ride duration

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

####
# What is the difference in ride locations for members and casual users
casual_stations<-
rides_all_clean %>% 
  filter(member_casual == "casual") %>% 
  group_by(member_casual, start_station_name) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count)) %>% 
  mutate(percent = 100*(count/sum(count))) %>% 
  head(10)

member_stations <-
rides_all_clean %>% 
  filter(member_casual == "member") %>% 
  group_by(member_casual, start_station_name) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count)) %>% 
  mutate(percent = 100*(count/sum(count))) %>% 
  head(10)

top_stations <- rbind(casual_stations, member_stations)

locations <-rides_all_clean[c(5, 9:10)]

# create csvs -export for mapping
write_csv(top_stations, "~/Desktop/top_stations.csv")
write_csv(locations, "~/Desktop/locations.csv")

            