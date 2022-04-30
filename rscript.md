# case-study_cyclistic
### Google Data Analytic Case Study
### Author : Dylan

_**Data license.**
The dataset has been made available by Motivate International Inc. under this **[license](https://ride.divvybikes.com/data-license-agreement)**. Used for educational purposes only, not sponsored. As per the license, this data will be used as source material, as applicable, in analyses, reports, or studies published or distributed for non-commercial purposes._

```{r}
#Install packages

install.packages("tidyverse") #data import and visualise
install.packages("lubridate")  #date functions
install.packages("ggplot") #visualise data
install.packages("dplyr")
install.packages("skimr")
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(skimr)

# STEP 1: COLLECT DATA
#======================================================
# Upload Bikeshare datasets (csv files) here
q2_2019 <- read_csv("Bikeshare_Trips_2019_Q2.csv")
q3_2019 <- read_csv("Bikeshare_Trips_2019_Q3.csv")
q4_2019 <- read_csv("Bikeshare_Trips_2019_Q4.csv")
q1_2020 <- read_csv("Bikeshare_Trips_2020_Q1.csv")

# STEP 2: DATA INTEGRITY
#======================================================
colnames(q2_2019)
colnames(q3_2019)
colnames(q4_2019)
colnames(q1_2020)

# Rename columns  to make them consistent with q1_2020 (as this will be the supposed going-forward table design for Bikeshare)

(q2_2019 <- rename(q2_2019, ride_id="01 - Rental Details Rental ID", rideable_type="01 - Rental Details Bike ID", started_at="01 - Rental Details Local Start Time", ended_at="01 - Rental Details Local End Time", start_station_name="03 - Rental Start Station Name", start_station_id="03 - Rental Start Station ID", end_station_name="02 - Rental End Station Name", end_station_id="02 - Rental End Station ID", member_casual="User Type"))

(q3_2019 <- rename(q3_2019, ride_id=trip_id, rideable_type=bikeid, started_at=start_time, ended_at=end_time, start_station_name=from_station_name, start_station_id=from_station_id, end_station_name=to_station_name, end_station_id=to_station_id, member_casual=usertype))

(q4_2019 <- rename(q4_2019, ride_id=trip_id, rideable_type=bikeid, started_at=start_time, ended_at=end_time, start_station_name=from_station_name, start_station_id=from_station_id, end_station_name=to_station_name, end_station_id=to_station_id, member_casual=usertype))


str(q1_2020)
str(q2_2019)
str(q3_2019)
str(q4_2019)

###OBSERVATIONS

#It is also time to decide whether we keep a data variable or remove it. 

#Convert ride_id and rideable_type to character. They can stack correctly when combining the datasets.

q4_2019 <- mutate(q4_2019, ride_id = as.character(ride_id),rideable_type = as.character(rideable_type))
q3_2019 <- mutate(q3_2019, ride_id = as.character(ride_id),rideable_type = as.character(rideable_type))
q2_2019 <- mutate(q2_2019, ride_id = as.character(ride_id),rideable_type = as.character(rideable_type))


all_trips <- bind_rows(q2_2019, q3_2019, q4_2019, q1_2020) #combine in one data frame

#untouched columns : q2_2019 ="05 - Member Details Member Birthday Year", "01 - Rental Details Duration In Seconds Uncapped", "Member Gender"
#untouched columns : q1_2020 ="start_lat"   "start_lng"     "end_lat"     "end_lng"   
#untouched columns : q3 & q4 = "gender", "birthyear" + tripduration

#remove lat, long, birthyear and gender fields as this data was dropped beginning in 2020. 
all_trips <- all_trips %>%
  select(-c(start_lat, start_lng, end_lat, end_lng, gender, birthyear, "05 - Member Details Member Birthday Year", "01 - Rental Details Duration In Seconds Uncapped", "Member Gender"))

all_trips <- all_trips %>%
  select(-c(tripduration))


# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
#======================================================

#inspect the new table

dim(all_trips) #3879822 rows and 9 columns
head(all_trips)
summary(all_trips)
str(all_trips)

skim_without_charts(all_trips) 

#(1). First issue we can find while looking at our variables, member_casual has 4 uniques which should only be 2. We will inspect the detail of the column with a pipe.
#Subscriber and member, Customer and casual

table(all_trips$member_casual) #casual : 48480, Customer : 857474, member : 378407, Subscriber : 2595461

#Rename values to the most recent format
all_trips <- all_trips %>%
  mutate(member_casual = recode(member_casual, "Subscriber" = "member", "Customer" = "casual"))

#Double check reassigned variables
table(all_trips$member_casual) #casual : 905954, member:2973868

#(2) Second issue is the date, we would like to add columns that list the date, month, day and year of each ride. It will allow to aggregate ride date of each month, day, or year. 

all_trips$date <- as.Date(all_trips$started_at)
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

#(3) add a "ride_length" calculation to all trips (in seconds) because some of the data didn't have tripduration for consistency. 

all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)
all_trips$ride_minute <- difftime(all_trips$ended_at, all_trips$started_at)/60

str(all_trips)

#convert "ride_length" from factor to numeric and be able to run calculations on the data.
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)
is.factor(all_trips$ride_minute)
all_trips$ride_minute <- as.numeric(as.character(all_trips$ride_minute))
is.numeric(all_trips$ride_minute)

#(4) Remove bad data
#The dataframe includes a few hundred entries when bikes are sent to the HQ QR for quality check.  We want to delete these rides. 

all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]

str(all_trips_v2)


# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS
#=====================================

#Descriptive analysis on ride_length (in seconds)

mean(all_trips_v2$ride_length) #average (total ride length/rides) R: [1] 1479.139
median(all_trips_v2$ride_length) #midpoint number in the ascending array of ride length  R: [1] 712
max(all_trips_v2$ride_length) #longest ride R : [1] 9387024
min(all_trips_v2$ride_length) #shortest ride R: [1] 1

summary(all_trips_v2$ride_length)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1     412     712        1479    1289   9387024 
summary(all_trips_v2$ride_minute)
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#0.02      6.87     11.87     24.65     21.48  156450.40 


#2. Compare members and casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean) #casual : 3552.72, member : 850.06
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median) #casual : 1546, member : 589
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max) #casual : 9387024, member : 9056634
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min) #casual : 2, member: 1


#3. See the average ride by each for each members vs casual riders
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

#days of the week are out of order

all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)


#4. Visualise the number of rides by rider type

all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% #label = TRUE/FALSE, order factor
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides=n(), average_duration=mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x= weekday, y=number_of_rides, fill = member_casual)) + geom_col(position="dodge") +
  geom_text(
      aes(label = round(number_of_rides, 0)),
      colour = "black", 
      size = 3, 
      vjust = 1.5, 
      position=position_dodge(.9)) +
  
  labs(
    title="Number of rides per rider type and weekday over the last 12 months", 
    subtitle="Includes data from 2019 - Q2, Q3, Q4 and 2020 - Q1", 
    caption ="Data source : The dataset has been made available by Motivate International Inc. for educational purposes only") + 
  
  theme(
    plot.title = element_text(size = 14, face="bold"), 
    plot.subtitle= element_text(colour="grey"), 
    plot.caption = element_text(hjust=1, face="italic")) + 
  
  scale_fill_hue()

# x*e ^y, how to calculate exponential : 1*10^+5 = 100000 

#5. Visualise the average duration 
all_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x= weekday, y=average_duration, fill= member_casual)) + geom_col(position="dodge") +
  geom_text(
  aes(label = round(average_duration, 0)),
  colour = "black", 
  size = 3, 
  vjust = 1.5, 
  position=position_dodge(.9))+ 
  labs(
  title ="Average on ride duration (in sec) per rider type and weekday over last 12 months", subtitle ="Includes data from 2019 - Q2, Q3, Q4 and 2020 - Q1", 
  caption="Data source : The dataset has been made available by Motivate International Inc. for educational purposes only")+
  theme(plot.title = element_text(size = 14, face="bold"), plot.subtitle= element_text(colour="grey"), plot.caption = element_text(hjust=1, face="italic"))+
  scale_fill_hue()

all_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_minute)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x= weekday, y=average_duration, fill= member_casual)) + geom_col(position="dodge") +
  geom_text(
    aes(label = round(average_duration, 0)),
    colour = "black", 
    size = 3, 
    vjust = 1.5, 
    position=position_dodge(.9))+ 
  labs(
    title ="Average on ride duration (in min.) per rider type and weekday over last 12 months", subtitle ="Includes data from 2019 - Q2, Q3, Q4 and 2020 - Q1", 
    caption="Data source : The dataset has been made available by Motivate International Inc. for educational purposes only")+
  theme(plot.title = element_text(size = 14, face="bold"), plot.subtitle= element_text(colour="grey"), plot.caption = element_text(hjust=1, face="italic"))+
  scale_fill_hue()
