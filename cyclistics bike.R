install.packages("tidyverse")
install.packages("janitor")
library(tidyverse)
library(janitor)
getwd()
library(lubridate)

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

df_main <- rbind(df1, df2, df3, df4, df5, df6, df7, df8, df9, df10, df11, df12)

View(df_main)
head(df_main)

colnames(df_main)

glimpse(df_main)

df_main <- janitor::remove_empty(df_main, which = c("rows", "cols"), quiet = FALSE)

View(df_main)

df_main$started_at <- lubridate::ymd_hms(df_main$started_at)
df_main$ended_at <- lubridate::ymd_hms(df_main$ended_at)

glimpse(df_main)

str(df_main)

df_main <- mutate(df_main, ride_length = ended_at - started_at)
as.numeric(df_main$ride_length)
df_main$date <- as.Date(df_main$started_at)
df_main$month <- format(as.Date(df_main$date), "%m")
df_main$day <- format(as.Date(df_main$date), "%d")
df_main$year <- format(as.Date(df_main$date), "%Y")
df_main$day_of_week <- format(as.Date(df_main$date), "%A")

write.csv(df_main, file = "data/df_main.csv", row.names = FALSE )

View(df_main)

df_main <- filter(df_main , ride_length > 0 | start_station_name != "HQ QR")
df_main <- df_main %>% 
  select( -c(start_lat, start_lng, end_lat, end_lng))

colnames(df_main)
head(df_main)

# Descriptive analysis on ride_length

mean(df_main$ride_length)

median(df_main$ride_length)

max(df_main$ride_length)

min(df_main$ride_length)

summary(df_main$ride_length)


aggregate(df_main$ride_length ~ df_main$member_casual, FUN = mean )
aggregate(df_main$ride_length ~ df_main$member_casual, FUN = median )
aggregate(df_main$ride_length ~ df_main$member_casual, FUN = max )
aggregate(df_main$ride_length ~ df_main$member_casual, FUN = min )

df_main$day_of_week <- ordered(df_main$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

View(df_main)

aggregate(df_main$ride_length ~ df_main$member_casual + df_main$day_of_week, FUN = mean)


df_main %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length) ) %>% 
  arrange(member_casual, weekday)

df_main %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")


df_main %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")


counts1 <- aggregate(df_main$ride_length ~ df_main$member_casual + df_main$day_of_week, FUN = mean)
write.csv(counts1, file = "data/avg_ride_length.csv")

counts2 <- aggregate(df_main$ride_id ~ df_main$member_casual + df_main$day_of_week, FUN = length)
write.csv(counts2, file = 'data/num_of_rides_by_day.csv')

counts3 <- aggregate(df_main$ride_id ~ df_main$member_casual + df_main$month, FUN = length)
write.csv(counts3, file = 'data/num_of_rides_by_month.csv')

View(counts1)
