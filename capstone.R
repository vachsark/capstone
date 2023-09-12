# Capstone Project
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)


# importing data from 2023
data_2023_01= read.csv("202301-divvy-tripdata.csv")
data_2023_02= read.csv("202302-divvy-tripdata.csv")
data_2023_03= read.csv("202303-divvy-tripdata.csv")
data_2023_04= read.csv("202304-divvy-tripdata.csv")
data_2023_05= read.csv("202305-divvy-tripdata.csv")
data_2023_06= read.csv("202306-divvy-tripdata.csv")
data_2023_07= read.csv("202307-divvy-tripdata.csv")

#Checking to see if column names all match
str(data_2023_01)
str(data_2023_02)
str(data_2023_03)
str(data_2023_04)
str(data_2023_05)
str(data_2023_06)
str(data_2023_07)
#combined all the trips
all_trips <- bind_rows(data_2023_01,
                       data_2023_02,
                       data_2023_03,
                       data_2023_04,
                       data_2023_05,
                       data_2023_06,
                       data_2023_07)

glimpse(all_trips)


colnames(all_trips)
dim(all_trips)
head(all_trips)
summary(all_trips$date)

head(all_trips$started_at)

all_trips$started_at <- mdy_hm(all_trips$started_at)
all_trips$ended_at <- mdy_hm(all_trips$ended_at)
all_trips$date <- date(all_trips$started_at)
all_trips$month <- month(all_trips$date)
all_trips$day <- day(all_trips$date)
all_trips$year <- year(all_trips$date)

#removing lat,long
all_trips <- all_trips %>% 
  select(-c(start_lat,start_lng,end_lat,end_lng))

all_trips_safe <- all_trips
head(all_trips_safe)
# good up until here
#justincase
all_trips <- all_trips_safe

glimpse(all_trips)

#another ride length try
all_trips$ride_length2 <- difftime(all_trips$ended_at,all_trips$started_at, units = "mins")

is.factor(all_trips$ride_length2)
all_trips$ride_length2 <- as.numeric(as.character(all_trips$ride_length2))
is.numeric(all_trips$ride_length2)
str(all_trips$ride_length2)



all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0 | all_trips$ride_length2<0),]
glimpse(all_trips_v2)



mean(all_trips_v2$ride_length) #straight average (total ride length / rides)
median(all_trips_v2$ride_length) #midpoint number in the ascending array of ride lengths
max(all_trips_v2$ride_length) #longest ride
min(all_trips_v2$ride_length) #shortest ride

mean(all_trips_v2$ride_length2) #straight average (total ride length / rides)
median(all_trips_v2$ride_length2) #midpoint number in the ascending array of ride lengths
max(all_trips_v2$ride_length2) #longest ride
min(all_trips_v2$ride_length2) #shortest ride

aggregate(all_trips_v2$ride_length2 ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length2 ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length2 ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length2 ~ all_trips_v2$member_casual, FUN = min)


# See the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length2 ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

# days are not in order
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels = c("Sunday", "Monday","Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
#again
aggregate(all_trips_v2$ride_length2 ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)


# rider data by type and weekday
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length2)) %>% 
  arrange(member_casual, weekday)

#Visualize

v1_rider_scientific <- all_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length2)) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill= member_casual)) + geom_col(position = "dodge")

#problems with y-axis values showing up in scientific notation
library(scales)
options(scipen = 999)

v1_rider_type <- v1_rider_scientific + theme_bw() + scale_y_continuous(labels = scales::comma)
v1_rider_type

#VIS for average duration


v2_average_duration <- all_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length2)) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x = weekday, y = average_duration, fill= member_casual)) + geom_col(position = "dodge")
v2_average_duration


#exporting into csv file
counts <- aggregate(all_trips_v2$ride_length2 ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
# I don't really care about the cleaned up data I just want to transfer over
# the new file with all the data
write.csv(counts, file = "C:\\Users\\vache\\OneDrive\\Documents\\Project 1\\avg_ride_length.csv")
write.csv(all_trips_v2, file = "C:\\Users\\vache\\OneDrive\\Documents\\Project 1\\2023-divvy-tripdata.csv")
