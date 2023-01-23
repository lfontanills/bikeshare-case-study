# Recommendation: Incentivize rides in high-traffic areas. Casual users use the same couple stations.


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