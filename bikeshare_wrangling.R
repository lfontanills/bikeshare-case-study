## Data Wrangling

# load packages

library(tidyverse) # data processing and analysis
library(lubridate) # wrangle dates and times

# import csv files as data frames
rides_202201 <- read.csv("202201-divvy-tripdata.csv") # January 2022
rides_202202 <- read.csv("202202-divvy-tripdata.csv") # February 2022
rides_202203 <- read.csv("202203-divvy-tripdata.csv") # March 2022
rides_202204 <- read.csv("202204-divvy-tripdata.csv") # April 2022
rides_202205 <- read.csv("202205-divvy-tripdata.csv") # May 2022
rides_202206 <- read.csv("202206-divvy-tripdata.csv") # June 2022
rides_202207 <- read.csv("202207-divvy-tripdata.csv") # July 2022
rides_202208 <- read.csv("202208-divvy-tripdata.csv") # August 2022
rides_202209 <- read.csv("202209-divvy-publictripdata.csv") # September 2022
rides_202210 <- read.csv("202210-divvy-tripdata.csv") # October 2022
rides_202211 <- read.csv("202211-divvy-tripdata.csv") # November 2022
rides_202212 <- read.csv("202212-divvy-tripdata.csv") # December 2022

# create a combined data frame for all rides over 12-month period
rides_2022 <- rbind(rides_202201,
                       rides_202202,
                       rides_202203,
                       rides_202204, 
                       rides_202205, 
                       rides_202206,
                       rides_202207, 
                       rides_202208, 
                       rides_202209, 
                       rides_202210, 
                       rides_202211, 
                       rides_202212)

# write combined data frame as csv
write.csv(rides_2022, "~/Documents/projects/bikeshare-case-study/rides_2022.csv") 
