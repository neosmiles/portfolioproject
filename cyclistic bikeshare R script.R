## Reading required packages
library(tidyverse)  #helps wrangle data
library(lubridate)  #helps wrangle date attributes
library(ggplot2)    #helps visualize data
library(janitor)    #helps wrangle data
library(hms)        #time

## set working directory
setwd("~/R/cyclistics_bike-share case study")
getwd()

## step 1: Import data
df1 <- read_csv("data/202103-divvy-tripdata.csv")
df2 <- read_csv("data/202104-divvy-tripdata.csv")
df3 <- read_csv("data/202105-divvy-tripdata.csv")
df4 <- read_csv("data/202106-divvy-tripdata.csv")
df5 <- read_csv("data/202107-divvy-tripdata.csv")
df6 <- read_csv("data/202108-divvy-tripdata.csv")
df7 <- read_csv("data/202109-divvy-tripdata.csv")
df8 <- read_csv("data/202110-divvy-tripdata.csv")
df9 <- read_csv("data/202111-divvy-tripdata.csv")
df10 <- read_csv("data/202112-divvy-tripdata.csv")
df11 <- read_csv("data/202201-divvy-tripdata.csv")
df12 <- read_csv("data/202202-divvy-tripdata.csv")


## Inspect the dataframes
glimpse(df1)
glimpse(df2)
glimpse(df3)
glimpse(df4)
glimpse(df5)
glimpse(df6)
glimpse(df7)
glimpse(df8)
glimpse(df9)
glimpse(df10)
glimpse(df11)
glimpse(df12)

## Bind data into single dataframe
df_all <- rbind(df1, df2, df3, df4, df5, df6, df7, df8, df9, df10, df11, df12)

#remove individual month data frames to clear up space in the environment
remove(df1, df2, df3, df4, df5, df6, df7, df8, df9, df10, df11, df12)

## CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS

# Inspect the new table
colnames(df_all)  #List of column names
nrow(df_all)    #How many rows are in data frame?
dim(df_all)     #Dimensions of the data frame?
head(df_all)    #See the first 6 rows of data frame.
tail(df_all)    #See the last 6 rows of data frame.
str(df_all)      #See list of columns and data types (numeric, character, etc)
summary(df_all)  #Statistical summary of data. Mainly for numerics

## Remove empty rows and column
df_all <- janitor::remove_empty(df_all, which = c("rows", "cols"), quiet = FALSE)

## Adding additional columns listing  Day, month, year of each ride. Allow us to aggregate ride data for each month, day, or year
df_all$date <- as.Date(df_all$started_at)
df_all$month <- format(as.Date(df_all$date), "%m")         #create column for month
df_all$day <- format(as.Date(df_all$date), "%d")           #create column for day
df_all$year <- format(as.Date(df_all$date), "%Y")          #create column for year
df_all$day_of_week <- format(as.Date(df_all$date), "%A")   #create column for day of week
df_all$time <- format(as.Date(df_all$date), "%H:%M:%S")    #format time as HH:MM:SS
df_all$time <- as_hms((df_all$started_at))                  #create new column for time
df_all$hour <- hour(df_all$time)                   #create new column for hour

## Checking for negative time
df_all %>% 
  filter(ended_at < started_at) %>% 
  count()

# removing negative time values
df_all <- df_all %>% 
  filter(ended_at > started_at)

glimpse(df_all)

## Add a "ride_length" by subtracting ended_at time from started_at time and converted it to minutes
df_all$ride_length <- difftime(df_all$ended_at,df_all$started_at, units = "mins")
df_all$ride_length <- round(df_all$ride_length, digits = 1)

#create column for different seasons: Spring, Summer, Fall, Winter
df_all <-df_all %>% 
  mutate(season = 
           case_when(month == "03" ~ "Spring",
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
                     month == "02" ~ "Winter")
)

#create column for different time_of_day: Night, Morning, Afternoon, Evening
df_all <-df_all %>%
     mutate(time_of_day = 
                case_when(hour == "0" ~ "Night",
                          hour == "1" ~ "Night",
                          hour == "2" ~ "Night",
                          hour == "3" ~ "Night",
                          hour == "4" ~ "Night",
                          hour == "5" ~ "Night",
                          hour == "6" ~ "Morning",
                          hour == "7" ~ "Morning",
                          hour == "8" ~ "Morning",
                          hour == "9" ~ "Morning",
                          hour == "10" ~ "Morning",
                          hour == "11" ~ "Morning",
                          hour == "12" ~ "Afternoon",
                          hour == "13" ~ "Afternoon",
                          hour == "14" ~ "Afternoon",
                          hour == "15" ~ "Afternoon",
                          hour == "16" ~ "Afternoon",
                          hour == "17" ~ "Afternoon",
                          hour == "18" ~ "Evening",
                          hour == "19" ~ "Evening",
                          hour == "20" ~ "Evening",
                          hour == "21" ~ "Evening",
                          hour == "22" ~ "Evening",
                          hour == "23" ~ "Evening")
)


#create a column for the month using the full month name
df_all <-df_all %>%
  mutate(month = 
           case_when(month == "01" ~ "January",
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
                     month == "12" ~ "December")
)


## Checking for duplicated rides
df_all[duplicated(df_all$ride_id), ]

## Removing null values
df_all <- df_all %>% 
  na.omit() 

## checking table after removing null values
dim(df_all)

## Inspect the structure of the columns
str(df_all)


## Remove "bad" data
df_all_v2 <- filter(df_all , ride_length > 0 | start_station_name != "HQ QR")

df_all_v2 <- df_all %>% 
  select( -c(start_lat, start_lng, end_lat, end_lng))

## renaming some column for 
df_all_v2 <- df_all_v2 %>% 
  rename(Bike_type = rideable_type, # changing rideable_type column name to Bike_type
         user_type = member_casual)   # changing member_casual column name to user_type 

## checking df_all
head(df_all_v2)
glimpse(df_all_v2)

##Saving cleaned df_ll_v2 dataset for analysis on other tools
write.csv(df_all_v2, file = "final_cleaned.csv" )


#  DESCRIPTIVE ANALYSIS

## Descriptive analysis on ride_length (all figures in seconds)

df_all_v2 %>% 
  summarise(mean=mean(ride_length), median=median(ride_length), max=max(ride_length), min=min(ride_length))


## Compare members and casual users
aggregate(df_all_v2$ride_length ~ df_all_v2$user_type, FUN = mean )
aggregate(df_all_v2$ride_length ~ df_all_v2$user_type, FUN = median )
aggregate(df_all_v2$ride_length ~ df_all_v2$user_type, FUN = max )
aggregate(df_all_v2$ride_length ~ df_all_v2$user_type, FUN = min )




## Arranging the days of the week accordingly
df_all_v2$day_of_week <- ordered(df_all_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))



## Average ride time by each day for members vs casual users
aggregate(df_all_v2$ride_length ~ df_all_v2$user_type + df_all_v2$day_of_week, FUN = mean)


## Analyzing ridership data by type and weekday
df_all_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(user_type, weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length) ) %>% 
  arrange(user_type, weekday)

# VISUALISATION

## Number of rides grouped by rider type
df_all_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(user_type, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(user_type, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = user_type, title="Average Number of Rides by Day: Members vs. Casual Riders" )) + geom_col(position = "dodge") + scale_y_continuous(labels = scales::comma) +
  labs(x = "Day of Week", y = "Number of Rides", fill = "Member/Casual",
       title = "Average Number of Rides by Day: Members vs. Casual Riders")







## Average duration
df_all_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(user_type, weekday) %>% 
  summarise(number_of_rides = n()
            ,Average_duration = mean(ride_length)) %>% 
  arrange(user_type, weekday)  %>% 
  ggplot(aes(x = weekday, y = Average_duration, fill = user_type)) +
  geom_col(position = "dodge") +
  labs(x = "Day of Week", y = "Average Duration(mins)", 
       fill = "Member/Casual",
       title = "Average Riding Duration by Day: Members vs. Casual Riders")



### visualizing the membership_type count
ggplot(data=df_all_v2)+ geom_bar(mapping=aes(x=user_type, fill = user_type)) + scale_y_continuous(labels = scales::comma) +
  labs(title = 'Member and Casual User Count',
       x = 'User type',
       y = 'Count')


## Count of Casual and Member based on Bike_type
ggplot(data=df_all_v2)+geom_bar(mapping=aes(x = Bike_type, fill=Bike_type)) +
  facet_wrap(~user_type) +
  labs(title = 'Count of Casual and Member based on Bike_type', 
       x = 'Ride Type',
       y = 'Count') + scale_y_continuous(labels = scales::comma)


## Average Number of Rides by Month: user_type
df_all_v2 %>% 
  mutate(month = month(started_at, label = TRUE)) %>%  #creates month field using month()
  group_by(user_type, month) %>%  #groups by membership_type and month
  summarize(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(user_type, month) %>% 
  ggplot(aes(x = month, y = number_of_rides, fill = user_type)) +
  geom_col(position = "dodge") + scale_y_continuous(labels = scales::comma) +
  labs(title = 'Average Number of Rides by Month: user_type',
       x = 'Month',
       y = 'Number of rides')



# EXPORT SUMMARY FILE FOR FURTHER ANALYSIS
counts <- aggregate(df_all_v2$ride_length ~ df_all_v2$user_type + df_all_v2$day_of_week, FUN = mean)
write.csv(counts, file = 'avg_ride_length.csv')

counts2 <- aggregate(df_all_v2$ride_id ~ df_all_v2$user_type + df_all_v2$day_of_week, FUN = length)
write.csv(counts2, file = 'num_of_rides_by_day.csv')

counts3 <- aggregate(df_all_v2$ride_id ~ df_all_v2$user_type + df_all_v2$month, FUN = length)
write.csv(counts3, file = 'num_of_rides_by_month.csv')

