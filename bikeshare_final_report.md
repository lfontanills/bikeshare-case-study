---
title: "Case study: How do cyclists use Divvy differently?"
author: "Laura Fontanills"
date: "28 January 2023"
# !!! You need to provide a logo image here !!! Or just delete the field for no logo
# logo: "logo_gallery.png"
theme: readable
output:
  html_document:
    keep_md: yes
    toc: TRUE
    toc_float: TRUE
    toc_collapsed: TRUE
    number_sections: FALSE
    code_folding: "show"
---



# Introduction

I completed this case study as the capstone project for my Google Data Analytics Certification. 

The case study is about Chicago's Divvy bike-share, and my objective is to evaluate how members and casual users utilize the bike-share differently, and then make recommendations to the company. 

The data is taken from a real bike-share -- Chicago-based Divvy -- but in the original case study my objective was set for a fictional company. Because there were some differences between Divvy and the fictional Cyclistic bike-share, I decided to conduct this analysis as if I were conducting this analysis for Divvy.

## Get the Raw Data

[Divvy’s historical data can be accessed here](https://divvy-tripdata.s3.amazonaws.com/index.html). Each month's data is contained in a single .csv file. I combined 12 months of Divvy data into a single data frame.

I conducted all cleaning, analysis, and visualization in R.

```r
library(tidyverse) # data processing and analysis
library(lubridate) # wrangle dates and times
options(dplyr.summarise.inform = FALSE)
```

The combined data frame has 14 variables:

* `X` is a row number that I'll remove later

* `ride_id` is a unique string of letters and numbers that identifies each trip

* `rideable_type` is the type of bike used on each trip: classic, electric, or unspecified

* `started_at` contains the date and time each trip started

* `ended_at` contains the date and time each trip ended

* `start_station_name` contains the name of the dock where each trip began

* `start_station_id` is a unique string of letters and numbers that identifies each starting dock

* `end_station_name` contains the name of the dock where each trip ended

* `end_station_id` is a unique string of letters and numbers that identifies each ending dock

* `start_lat` is the latitude coordinate where each trip started

* `start_lng` is the longitude coordinate where each trip started

* `end-lat` is the latitude coordinate where each trip ended

* `end_lng` is the longitude coordinate where each trip ended

* `member_casual` describes whether a trip was taken by a Divvy member or a casual user


```r
rides_2022 <- read.csv("rides_2022.csv")
head(rides_2022)
```

```
##   X          ride_id rideable_type          started_at            ended_at
## 1 1 C2F7DD78E82EC875 electric_bike 2022-01-13 11:59:47 2022-01-13 12:02:44
## 2 2 A6CF8980A652D272 electric_bike 2022-01-10 08:41:56 2022-01-10 08:46:17
## 3 3 BD0F91DFF741C66D  classic_bike 2022-01-25 04:53:40 2022-01-25 04:58:01
## 4 4 CBB80ED419105406  classic_bike 2022-01-04 00:18:04 2022-01-04 00:33:00
## 5 5 DDC963BFDDA51EEA  classic_bike 2022-01-20 01:31:10 2022-01-20 01:37:12
## 6 6 A39C6F6CC0586C0B  classic_bike 2022-01-11 18:48:09 2022-01-11 18:51:31
##              start_station_name start_station_id              end_station_name
## 1      Glenwood Ave & Touhy Ave              525          Clark St & Touhy Ave
## 2      Glenwood Ave & Touhy Ave              525          Clark St & Touhy Ave
## 3 Sheffield Ave & Fullerton Ave     TA1306000016 Greenview Ave & Fullerton Ave
## 4      Clark St & Bryn Mawr Ave     KA1504000151     Paulina St & Montrose Ave
## 5   Michigan Ave & Jackson Blvd     TA1309000002        State St & Randolph St
## 6         Wood St & Chicago Ave              637       Honore St & Division St
##   end_station_id start_lat start_lng  end_lat   end_lng member_casual
## 1         RP-007  42.01280 -87.66591 42.01256 -87.67437        casual
## 2         RP-007  42.01276 -87.66597 42.01256 -87.67437        casual
## 3   TA1307000001  41.92560 -87.65371 41.92533 -87.66580        member
## 4   TA1309000021  41.98359 -87.66915 41.96151 -87.67139        casual
## 5   TA1305000029  41.87785 -87.62408 41.88462 -87.62783        member
## 6   TA1305000034  41.89563 -87.67207 41.90312 -87.67394        member
```


# Data Cleaning

## Considerations

Before working in R, I reviewed each month's data independently in Excel. I discovered the following inconsistencies that I would need to address.

* Test rides outside the Chicago area or with no geolocation

* Rides starting before and ending after the DST time changes in March and November

* Many rides shorter than 1 minute, which could be due to bike problems

* Many rides longer than 24 hours, which could be due to docking problems

* Rides with no end location

* A nonspecific "docked" bike type associated with a small percentage of rides by casual users

I confirmed that all rows are unique by checking the number of ride IDs. TRUE indicates that all rows are unique.

```r
n_distinct(rides_2022$ride_id) == nrow(rides_2022)
```

```
## [1] TRUE
```

I also confirmed that all rides were taken by either members or casual users.

```r
rides_2022 %>% 
  group_by(member_casual) %>% 
  summarize(num_rides = n()) %>% 
  mutate(pct_rides = 100*(num_rides/sum(num_rides))) # calculate percentage
```

```
## # A tibble: 2 × 3
##   member_casual num_rides pct_rides
##   <chr>             <int>     <dbl>
## 1 casual          2322032      41.0
## 2 member          3345685      59.0
```

## Ride Time Data

I transformed the `started_at` and `ended_at` fields from character vectors to datatime.

```r
rides_2022$started_at <- ymd_hms(rides_2022$started_at)
rides_2022$ended_at <- ymd_hms(rides_2022$ended_at)
```

I create a new column called `ride_length` for ride duration in minutes. Some rides have negative ride duration, so I'll need to filter these out.


```r
rides_2022$ride_length <- as.numeric(rides_2022$ended_at - rides_2022$started_at)

rides_2022$ride_length <- rides_2022$ride_length / 60


rides_2022 %>% 
  filter(ride_length < 0) %>% 
  select(started_at, ended_at, ride_length) %>% 
  head()
```

```
##            started_at            ended_at  ride_length
## 1 2022-03-05 11:00:57 2022-03-05 10:55:01   -5.9333333
## 2 2022-03-05 11:38:04 2022-03-05 11:37:57   -0.1166667
## 3 2022-05-30 11:06:29 2022-05-30 11:06:17   -0.2000000
## 4 2022-06-07 19:15:39 2022-06-07 17:05:37 -130.0333333
## 5 2022-06-07 19:14:46 2022-06-07 17:07:45 -127.0166667
## 6 2022-06-23 19:22:57 2022-06-23 19:21:46   -1.1833333
```
121089 rides are very short (less than 1 minute), and of these 431 are 0 seconds long.

5360 rides are very long (greater than 24 hours). With a day pass, Divvy lets you ride up to 3 hours on a single trip before incurring a fee, and the pass is good for 24 hours. It's impossible to know whether trips longer than 3 hours were real trips or whether the user had docking issues. 19592 rides are longer than 3 hours. For my analysis I considered typical use to be between 60 seconds and 3 hours.

## Ride Location and Bike Data

There are three bike types in the data frame - classic_bike, electric_bike, and docked_bike. Divvy docks use a different geolocation system than the bikes themselves-- docks have geolocation with 0.00001° sensitivity, while undocked bikes have geolocation with 0.01° sensitivity. Many Divvy docks are within 0.01° of other docks, especially in the city center where most rides take place, so undocked bikes can't reliably be matched with nearby docks. Divvy allows their users to take out and drop off undocked bikes for a fee.


```r
# How many rides were taken by members, casual users on each type
rides_2022 %>% 
  group_by(member_casual, rideable_type) %>% 
  summarize(num_rides = n()) %>% 
  mutate(pct_rides = 100*(num_rides/sum(num_rides))) # calculate percentage
```

```
## # A tibble: 5 × 4
## # Groups:   member_casual [2]
##   member_casual rideable_type num_rides pct_rides
##   <chr>         <chr>             <int>     <dbl>
## 1 casual        classic_bike     891459     38.4 
## 2 casual        docked_bike      177474      7.64
## 3 casual        electric_bike   1253099     54.0 
## 4 member        classic_bike    1709755     51.1 
## 5 member        electric_bike   1635930     48.9
```
177474 rides were taken on the nonspecific docked_bike type. Only casual riders used the docked_bike type, which accounted for 7.6% of all casual rides. These bikes were undocked from 678 unique Divvy stations during all months of the year. 


```r
# When were docked bikes taken out, returned
rides_2022 %>% 
  filter(rideable_type == "docked_bike") %>% 
  group_by(rideable_type) %>% 
  summarize(min_start = min(started_at), 
            max_start = max(started_at),
            min_end = min(ended_at),
            max_end = max(ended_at))
```

```
## # A tibble: 1 × 5
##   rideable_type min_start           max_start           min_end            
##   <chr>         <dttm>              <dttm>              <dttm>             
## 1 docked_bike   2022-01-01 00:24:47 2022-12-31 23:44:47 2022-01-01 00:26:19
## # … with 1 more variable: max_end <dttm>
```

```r
# Where were docked bikes taken out
rides_2022 %>% 
  filter(rideable_type == "docked_bike") %>% 
  group_by(start_station_name) %>% 
  summarize(num_rides=n()) %>% 
  # arrange(num_rides)
  arrange(desc(num_rides))
```

```
## # A tibble: 688 × 2
##    start_station_name                 num_rides
##    <chr>                                  <int>
##  1 Streeter Dr & Grand Ave                11788
##  2 DuSable Lake Shore Dr & Monroe St       6969
##  3 Millennium Park                         5796
##  4 Shedd Aquarium                          4658
##  5 Michigan Ave & Oak St                   4196
##  6 Dusable Harbor                          3163
##  7 DuSable Lake Shore Dr & North Blvd      3111
##  8 Adler Planetarium                       2698
##  9 Theater on the Lake                     2627
## 10 Montrose Harbor                         2363
## # … with 678 more rows
```

All rides had starting geolocations in the start_lat and start_lng fields. 833064 rides had no starting station information. All of these bikes were electric_bike type.
`
Not all rides had starting geolocations in the end_lat and end_lng fields. 892742 rides had no ending station information. These bikes were classic_bike and electric_bike types.

I located 1 ride outside the city limits, which I identified as a test ride, and 8 rides to Green St & Madison Ave, which is not listed as a station (current or former) on Divvy's website. I will remove these from the data set during cleaning.

```r
# set city limits
city_lat <- c(41, 43)
city_lng <- c(-89, -87)

# find rides outside city area
rides_2022 %>% 
  filter((!between(start_lat,  city_lat[1], city_lat[2])) | 
           (!between(start_lng,  city_lng[1], city_lng[2]))) %>% 
  select(ride_id, start_station_name, start_lat, start_lng)
```

```
##            ride_id                           start_station_name start_lat
## 1 3327172413547F64 Pawel Bialowas - Test- PBSC charging station  45.63503
##   start_lng
## 1 -73.79648
```

```r
rides_2022 %>% 
  filter((!between(end_lat,  city_lat[1], city_lat[2])) | 
           (!between(end_lng,  city_lng[1], city_lng[2])) & end_lat != 0 & end_lng != 0) %>% 
  select(ride_id, end_station_name, end_lat, end_lng)
```

```
##            ride_id        end_station_name end_lat end_lng
## 1 3B47B333C0D186F0 Green St & Madison Ave*       0       0
## 2 42AF82C53D831251 Green St & Madison Ave*       0       0
## 3 BB8AA29838266294 Green St & Madison Ave*       0       0
## 4 6AFE1471227BD76F Green St & Madison Ave*       0       0
## 5 E9495F1DC3475D41 Green St & Madison Ave*       0       0
## 6 75DE33501313D0CE Green St & Madison Ave*       0       0
## 7 0A6988FE859F4D54 Green St & Madison Ave*       0       0
## 8 7F49424E860E7094 Green St & Madison Ave*       0       0
```

I saved the cleaned and transformed data to a new data frame. 

```r
# create new data frame - clean
rides_clean <-rides_2022 %>% 
  # keep rides b/w 1 min, 3 hr
  filter(ride_length > 1) %>% 
  filter(ride_length < 60*3) %>%
   # filter out rides w/o location data
  filter(!is.na(start_lat)) %>%
  filter(!is.na(end_lat)) %>% 
  # filter out rides during DST change on 11/6
  filter(!(started_at < '2022-11-06 02:00:00' & ended_at > '2022-11-06 02:00:00')) %>%
  # filter out rides not in Chicago
  filter(between(start_lat, city_lat[1], city_lat[2])) %>% 
  filter(between(end_lat, city_lat[1], city_lat[2])) %>% 
  # select columns for new data frame
  select(ride_id, member_casual, rideable_type, started_at, ended_at, start_station_name, end_station_name, start_lat, start_lng, end_lat, end_lng, ride_length)

head(rides_clean)
```

```
##            ride_id member_casual rideable_type          started_at
## 1 C2F7DD78E82EC875        casual electric_bike 2022-01-13 11:59:47
## 2 A6CF8980A652D272        casual electric_bike 2022-01-10 08:41:56
## 3 BD0F91DFF741C66D        member  classic_bike 2022-01-25 04:53:40
## 4 CBB80ED419105406        casual  classic_bike 2022-01-04 00:18:04
## 5 DDC963BFDDA51EEA        member  classic_bike 2022-01-20 01:31:10
## 6 A39C6F6CC0586C0B        member  classic_bike 2022-01-11 18:48:09
##              ended_at            start_station_name
## 1 2022-01-13 12:02:44      Glenwood Ave & Touhy Ave
## 2 2022-01-10 08:46:17      Glenwood Ave & Touhy Ave
## 3 2022-01-25 04:58:01 Sheffield Ave & Fullerton Ave
## 4 2022-01-04 00:33:00      Clark St & Bryn Mawr Ave
## 5 2022-01-20 01:37:12   Michigan Ave & Jackson Blvd
## 6 2022-01-11 18:51:31         Wood St & Chicago Ave
##                end_station_name start_lat start_lng  end_lat   end_lng
## 1          Clark St & Touhy Ave  42.01280 -87.66591 42.01256 -87.67437
## 2          Clark St & Touhy Ave  42.01276 -87.66597 42.01256 -87.67437
## 3 Greenview Ave & Fullerton Ave  41.92560 -87.65371 41.92533 -87.66580
## 4     Paulina St & Montrose Ave  41.98359 -87.66915 41.96151 -87.67139
## 5        State St & Randolph St  41.87785 -87.62408 41.88462 -87.62783
## 6       Honore St & Division St  41.89563 -87.67207 41.90312 -87.67394
##   ride_length
## 1    2.950000
## 2    4.350000
## 3    4.350000
## 4   14.933333
## 5    6.033333
## 6    3.366667
```
This file has 5525383 lines and 12 columns. It is ready to be analyzed.

# Analysis and Visualizations

## Finding 1: Casual users take longer trips than members

Casual trips are about 8 minutes longer, on average, than member trips. Perhaps they're not as used to bikes or bike-shares, or perhaps they are using the bikes for fun.


```r
tapply(rides_clean$ride_length, rides_clean$member_casual, summary)
```

```
## $casual
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.017   7.600  13.200  20.523  24.117 179.983 
## 
## $member
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.017   5.317   9.017  12.265  15.450 179.900
```


```r
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
    y = "Average ride duration (minutes)",
    fill = "User type") +
  scale_y_continuous(labels = scales::comma) +
  theme_bw() + 
  scale_fill_brewer(palette = "Blues", direction = -1)

length_plot
```

<img src="bikeshare_final_report_files/figure-html/plot durations-1.png" style="display: block; margin: auto;" />

## Finding 2: Casual riders take more weekend trips

Member rides peak during the work week, which could indicate that many members use bikes for commuting. Casual rides peak on Saturday and Sunday, suggesting that casual users use bikes for leisure or tourism.


```r
weekday_plot <-
  rides_clean %>% 
  mutate(weekday = wday(started_at, label = TRUE, abbr = FALSE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarize(count=n()) %>% 
  ggplot(aes(x=weekday, y=count, fill=member_casual))+
  geom_col(position="dodge") +
  labs(
    title = "Number of rides by weekday",
    subtitle = "For all trips between 1 minute and 3 hours long",
    x = "User type", 
    y = "Number of rides",
    fill = "User type") +
  scale_y_continuous(labels = scales::comma) +
  theme_bw() + 
  scale_fill_brewer(palette = "Blues", direction = -1)

weekday_plot
```

<img src="bikeshare_final_report_files/figure-html/plot rides by weekday-1.png" style="display: block; margin: auto;" />
## Finding 3: Casual riders use a few stations relatively frequently

Divvy has 678 stations at the time of writing. Casual riders show a clear preference for 5 stations -- these stations are where 8% of all casual rides begin. The top 5 casual starting stations are located in parks in the city center, close to the waterfront. These are very popular locations for tourists and locals.

Members don't especially favor any stations, and no station accounts for more than 1% of all member rides.


```r
  rides_clean %>% 
  filter(member_casual == "casual") %>% 
  group_by(member_casual, start_station_name) %>% 
  summarize(num_rides = n()) %>% 
  arrange(desc(num_rides)) %>% 
  mutate(percent_rides = 100*(num_rides/sum(num_rides))) %>% 
  head(10)
```

```
## # A tibble: 10 × 4
## # Groups:   member_casual [1]
##    member_casual start_station_name                   num_rides percent_rides
##    <chr>         <chr>                                    <int>         <dbl>
##  1 casual        ""                                      331467        14.7  
##  2 casual        "Streeter Dr & Grand Ave"                56441         2.50 
##  3 casual        "DuSable Lake Shore Dr & Monroe St"      30971         1.37 
##  4 casual        "Millennium Park"                        24709         1.09 
##  5 casual        "Michigan Ave & Oak St"                  24618         1.09 
##  6 casual        "DuSable Lake Shore Dr & North Blvd"     22962         1.02 
##  7 casual        "Shedd Aquarium"                         19708         0.873
##  8 casual        "Theater on the Lake"                    18054         0.800
##  9 casual        "Wells St & Concord Ln"                  15913         0.705
## 10 casual        "Dusable Harbor"                         13689         0.606
```

```r
  rides_clean %>% 
  filter(member_casual == "member") %>% 
  group_by(member_casual, start_station_name) %>% 
  summarize(num_rides = n()) %>% 
  arrange(desc(num_rides)) %>% 
  mutate(percent_rides = 100*(num_rides/sum(num_rides))) %>% 
  head(10)
```

```
## # A tibble: 10 × 4
## # Groups:   member_casual [1]
##    member_casual start_station_name             num_rides percent_rides
##    <chr>         <chr>                              <int>         <dbl>
##  1 member        ""                                469193        14.4  
##  2 member        "Kingsbury St & Kinzie St"         24483         0.749
##  3 member        "Clark St & Elm St"                21579         0.660
##  4 member        "Wells St & Concord Ln"            20937         0.641
##  5 member        "University Ave & 57th St"         19487         0.596
##  6 member        "Clinton St & Washington Blvd"     19351         0.592
##  7 member        "Ellis Ave & 60th St"              19088         0.584
##  8 member        "Wells St & Elm St"                18650         0.571
##  9 member        "Loomis St & Lexington St"         18638         0.570
## 10 member        "Clinton St & Madison St"          18466         0.565
```
## Finding 4: Casual riders use more e-bikes

In 2022, Casual riders used more electric bikes than classic bikes. By contrast, members favored classic bikes.


```r
rides_clean %>% 
  group_by(member_casual, rideable_type) %>%
  summarize(num_rides = n()) %>% 
  mutate(pct_rides = 100* num_rides/sum(num_rides))
```

```
## # A tibble: 5 × 4
## # Groups:   member_casual [2]
##   member_casual rideable_type num_rides pct_rides
##   <chr>         <chr>             <int>     <dbl>
## 1 casual        classic_bike     870917     38.6 
## 2 casual        docked_bike      168568      7.47
## 3 casual        electric_bike   1218180     54.0 
## 4 member        classic_bike    1681261     51.5 
## 5 member        electric_bike   1586457     48.5
```



```r
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
```

<img src="bikeshare_final_report_files/figure-html/bike types plot-1.png" style="display: block; margin: auto;" />
# Recommendations

** 1. Appeal to electric bike users.**
Casual riders take more trips on e-bikes than on classic bikes. Electric bikes are fun, easy to use, and somewhat novel. Communicating the advantages of membership for e-bike users could convert some casual users.

** 2. Target casual cyclists riding for leisure.**
Casual riders take more trips on weekends. Advertising membership as a cost-effective way of exploring the city could resonate with people who don't have time to cycle during the week, and want to use bikes for fun. Weekend or off-peak memberships could be an option to explore.

** 3. Increase advertising at high-traffic stations.**
There are 5 stations that are heavily used by casual riders, all of which are located in the city center. Advertise membership to locals who want to get around Chicago to see parks, museums, and restaurants. 

# Divvy Information

[Data source](https://divvy-tripdata.s3.amazonaws.com/index.html)

[Data license](https://ride.divvybikes.com/data-license-agreement)

[City of Chicago Divvy database](https://data.cityofchicago.org/Transportation/Divvy-Bicycle-Stations-All-Map/bk89-9dk7)

