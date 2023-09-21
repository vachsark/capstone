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


all_trips$started_at <- mdy_hm(all_trips$started_at)
all_trips$ended_at <- mdy_hm(all_trips$ended_at)
all_trips$date <- date(all_trips$started_at)
all_trips$month <- month(all_trips$date)
all_trips$day <- day(all_trips$date)
all_trips$year <- year(all_trips$date)

#removing lat,long
#dont want to do this anymore | i want to see the distance that was traveled compared to the time it took to get there
#all_trips <- all_trips %>% 
 # select(-c(start_lat,start_lng,end_lat,end_lng))



# good up until here

glimpse(all_trips)

#another ride length try
all_trips$ride_length2 <- difftime(all_trips$ended_at,all_trips$started_at, units = "mins")

is.factor(all_trips$ride_length2)
all_trips$ride_length2 <- as.numeric(as.character(all_trips$ride_length2))
is.numeric(all_trips$ride_length2)
str(all_trips$ride_length2)



all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<=0 | all_trips$ride_length2<=0),]
glimpse(all_trips_v2)



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

counts
# the new file with all the data


#write.csv(counts, file = "C:\\Users\\vache\\OneDrive\\Desktop\\capstone\\avg_ride_length.csv")
#write.csv(all_trips_v2, file = "C:\\Users\\vache\\OneDrive\\Desktop\\capstone\\divvy_long_lat.csv")

#long and lat changes
glimpse(all_trips_v2)

#lat diff in km
#all_trips_v2$lat_diff <- abs(all_trips_v2$end_lat - all_trips_v2$start_lat) * 111.699

#checking data
#max(all_trips_v2$lat_diff, na.rm = TRUE)
#min(all_trips_v2$lat_diff, na.rm = TRUE)
#mean(all_trips_v2$lat_diff, na.rm = TRUE)

#long Diff KM
#all_trips_v2$long_diff <- abs(all_trips_v2$end_lng - all_trips_v2$start_lng) * 110.567

#check
#max(all_trips_v2$long_diff, na.rm = TRUE)
#min(all_trips_v2$long_diff, na.rm = TRUE)
#mean(all_trips_v2$long_diff, na.rm = TRUE)

#in km
all_trips_v2$totaldiff_miles <-((abs(all_trips_v2$end_lat - all_trips_v2$start_lat) * 69.4065408
                           + abs(all_trips_v2$end_lng - all_trips_v2$start_lng) * 68.70314861))
glimpse(all_trips_v2$totaldiff_miles)



#cleaning up some extra columns
all_trips_v2 <- all_trips_v2 %>% 
  select(-c(latmean,lat_diff,long_diff))

v2_average_miles <- all_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(), average_miles = mean(totaldiff_miles, na.rm = TRUE)) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x = weekday, y = average_miles, fill= member_casual)) + geom_col(position = "dodge")
v2_average_miles

v1_electric_classic <- all_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(rideable_type, member_casual) %>% 
  summarise(number_of_rides = n(), average_miles = mean(totaldiff_miles, na.rm = TRUE)) %>% 
  arrange(rideable_type, member_casual) %>% 
  ggplot(aes(x = member_casual, y = number_of_rides, fill= rideable_type)) + geom_col(position = "dodge")
v1_electric_classic

#write.csv(all_trips_v2, file = "C:\\Users\\vache\\OneDrive\\Desktop\\capstone\\divvy_everything.csv")
## maybe check average distance traveled with each type of bike?


## finding the most common start station and end station, some data is missing but it can still
## give a good idea about where the casual group comes from and goes to most often.

#for export
all_trips_safe <- all_trips_v2 %>% 
  select(-c(ride_length,start_lng,end_lat,end_lng, start_lat,start_station_id,end_station_id))
head(all_trips_safe)

write.csv(all_trips_safe, file = "C:\\Users\\vache\\OneDrive\\Desktop\\capstone\\divvy_cleaner.csv")
