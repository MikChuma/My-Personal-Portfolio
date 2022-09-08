#-----------------------COMPLETE ANALYSIS IN R STUDIO-------------------------

#Click Session and choose working directory

#Install requird Packages and Libraries using install.packages() function
#Load the Libraries 
library(tidyverse) #calculations
library(lubridate) #dates 
library(hms) #time
library(data.table) #exporting data frame

#Upload the original data from the divvy-tripdata into R using read_csv function 
        jul21 <- read.csv("202107-divvy-tripdata.csv")
        aug21 <- read.csv("202108-divvy-tripdata.csv")
        sep21 <- read.csv("202109-divvy-tripdata.csv")
        oct21 <- read.csv("202110-divvy-tripdata.csv")
        nov21 <- read.csv("202111-divvy-tripdata.csv")
        dec21 <- read.csv("202112-divvy-tripdata.csv")
        jan22 <- read.csv("202201-divvy-tripdata.csv")
        feb22 <- read.csv("202202-divvy-tripdata.csv")
        mar22 <- read.csv("202203-divvy-tripdata.csv")
        apr22 <- read.csv("202204-divvy-tripdata.csv")
        may22 <- read.csv("202205-divvy-tripdata.csv")
        jun22 <- read.csv("202206-divvy-tripdata.csv")

#Combine all the data into one dataframe using rbind function and parse to a new name
july21_jun22_trips <- rbind(jul21, aug21, sep21, oct21, nov21, dec21, jan22, feb22, mar22, apr22, may22, jun22)

#remove individual month data frames from the environment pane to clear up space
remove(jul21, aug21, sep21, oct21, nov21, dec21, jan22, feb22, mar22, apr22, may22, jun22)

#create new data frames by parsing july21_jun22_trip to one_year_trip to retain original data
one_year_trip <- july21_jun22_trips

#calculate ride length by subtracting ended_at time from started_at time and converted it to minutes
one_year_trip$ride_length <- difftime(one_year_trip$ended_at, one_year_trip$started_at, units = "mins")
one_year_trip$ride_length <- round(one_year_trip$ride_length, digits = 1)

#create columns for day of week, month, day, year
one_year_trip$date <- as.Date(one_year_trip$started_at) #default format is yyyy-mm-dd, use start date
one_year_trip$day_of_week <- wday(one_year_trip$started_at) #calculate the day of the week 
one_year_trip$day_of_week <- format(as.Date(one_year_trip$date), "%A") #create column for day of week
one_year_trip$month <- format(as.Date(one_year_trip$date), "%m") #create column for month
one_year_trip$day <- format(as.Date(one_year_trip$date), "%d") #create column for day
one_year_trip$year <- format(as.Date(one_year_trip$date), "%Y") #create column for year

#create column for different seasons: Spring, Summer, Fall, Winter
one_year_trip <-one_year_trip %>% 
                                mutate(season = case_when(
                                      month == "03" ~ "Spring",
                                      month == "04" ~ "Spring",
                                      month == "05" ~ "Spring",
                                      month == "06"  ~ "Summer",
                                      month == "07"  ~ "Summer",
                                      month == "08"  ~ "Summer",
                                      month == "09" ~ "Fall",
                                      month == "10" ~ "Fall",
                                      month == "11" ~ "Fall",
                                      month == "12" ~ "Winter",
                                      month == "01" ~ "Winter",
                                      month == "02" ~ "Winter"))

#create a column for the month using the full month name
one_year_trip <-one_year_trip %>% 
                                mutate(month = case_when(
                                      month == "01" ~ "January",
                                      month == "02" ~ "February",
                                      month == "03" ~ "March",
                                      month == "04" ~ "April",
                                      month == "05" ~ "May",
                                      month == "06" ~ "June",
                                      month == "07" ~ "July",
                                      month == "08" ~ "August",
                                      month == "09" ~ "September",
                                      month == "10" ~ "October",
                                      month == "11" ~ "November",
                                      month == "12" ~ "December"))

#clean the data
one_year_trip <- na.omit(one_year_trip) #remove rows with NA values
one_year_trip<- distinct(one_year_trip) #remove duplicate rows 
one_year_trip <- one_year_trip[!(one_year_trip$ride_length <=0),] #remove where ride_length is 0 or negative

#remove columns not needed: ride_id, start_station_id, end_station_id, start_lat, start_long, end_lat, end_lng
one_year_trip <- one_year_trip %>% select(-c(start_station_id, end_station_id,start_lat, start_lng,end_lat, end_lng))  

#view the final data
View(one_year_trip)

#Download the new data as a .csv file which we can use for visualization in other platforms
fwrite(one_year_trip,"one_year_trip.csv")

#visualization in R using ggplot2
ggplot(data = one_year_trip) +
  geom_bar(mapping = aes(x = day_of_week, fill = member_casual, ), position = "dodge") +
  labs(title="Total Weekly Ride by both Members and Casual Users")

ggplot(data = one_year_trip) +
  geom_bar(mapping = aes(x = month, fill = member_casual, ), position = "dodge") +
  labs(title="Total Monthly Ride by both Members and Casual Users")

liggplot(data = one_year_trip) +
  geom_bar(mapping = aes(x = season, fill = member_casual, ), position = "dodge") +
  labs(title="Total Seasonal Ride by both Members and Casual Users")

#Other Calculations
count(one_year_trip) #Calculates total number of trips
sum(one_year_trip$member_casual == "member") #Calculates total number of trips for members
sum(one_year_trip$member_casual == "casual") #Calculates total number of trips for casuals
sum(one_year_trip$rideable_type == "electric_bike") #Calculates total number of trips with electric_bike
mean(one_year_trip$ride_length)  #Calculates average ride length of all trips

