#Load libraries
library(tidyverse)
library(ggplot2)
library(lubridate)
library(leaflet)

#Set Working Directory
setwd("~/MSCA/3. Data Engineering Platforms/TheRealEstate")

#Import the dataset
#Above Midway (District=2,Ward=5), Below Midway(District=3,Ward=6)
dataset = read.csv("CrimeHydePark.csv",stringsAsFactors = FALSE,header = TRUE)

#Clean Data
#Convert to datetime
#dataset$date = as_datetime(mdy_hms(dataset$date))
dataset$date = as_datetime(dataset$date)

# dataset = dataset %>%
#   filter(ward == 5 | ward == 42) %>%
#   mutate(Campus = ifelse(ward==42,"Downtown",
#                          ifelse(district==2,"Hyde Park", "Below Midway Plaissance"))) %>%
#   select(-c(caseNumber,updatedOn,location))

#Heatmap for Weekdays vs. Months
dataset %>% filter(year==2017) %>%
  mutate(Campus = ifelse(Campus=="Hyde Park" | Campus=="Below Midway Plaissance", "Hyde Park","Downtown")) %>%
  group_by(Campus,
           month = month(date,label=T), 
           weekday = wday(date,label=TRUE,week_start = 1)) %>%
  summarise(count = n()) %>% 
  ggplot(aes(x=month,y=weekday,fill=count)) +
  geom_tile() +
  facet_grid(~Campus) +
  xlab("Month of the Year") +
  ylab("Day of the Week") +
  ggtitle("Crime Counts Across the Year") +
  scale_fill_gradient(name = "Crime Counts",low = "#F5F5DC",high = "#36648B") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

#Heatmap for Hours vs. Months
dataset %>% filter(year==2017) %>%
  mutate(Campus = ifelse(Campus=="Hyde Park" | Campus=="Below Midway Plaissance", "Hyde Park","Downtown")) %>%
  group_by(Campus,
           month = month(date,label=T), 
           hour = hour(date)) %>%
  summarise(count = n()) %>% 
  ggplot(aes(x=month,y=as.factor(hour),fill=count)) +
  geom_tile() +
  facet_grid(~Campus) +
  xlab("Month of the Year") +
  ylab("Hour of the Day") +
  ggtitle("Crime Counts Across the Year") +
  scale_fill_gradient(name = "Crime Counts",low = "#F5F5DC",high = "#36648B") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

#Create Crime Type Counts
dataset %>% filter(year==2018) %>% 
  mutate(Campus = ifelse(Campus=="Hyde Park" | Campus=="Below Midway Plaissance", "Hyde Park","Downtown")) %>%
  group_by(Campus,primaryType) %>% summarise(Count = n()) %>%
  ggplot(aes(reorder(primaryType,Count),Count)) + 
  geom_bar(stat="identity", fill = "royalblue", alpha = 0.7) + 
  coord_flip() +
  facet_grid(~Campus) + 
  xlab("Crime Type") +
  ylab("Number of Crimes") +
  ggtitle("Crime Type: Count") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

#Create Monthly Crime Count
dataset %>% 
  mutate(Campus = ifelse(Campus=="Hyde Park" | Campus=="Below Midway Plaissance", "Hyde Park","Downtown")) %>%
  filter(year==2017) %>% group_by(Campus,month = month(date,label=T)) %>%
  summarise(Count = n()) %>%
  ggplot(aes(month,Count)) + 
  geom_bar(stat="identity", fill = "royalblue", alpha = 0.7) +
  facet_grid(~Campus) +
  scale_fill_gradient2(high = "tomato") +
  xlab("Months (2018)") +
  ylab("Count of Crimes") +
  ggtitle("Crime Trend") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "None")

#Daily crimes
dataset %>% 
  mutate(Campus = ifelse(Campus=="Hyde Park" | Campus=="Below Midway Plaissance", "Hyde Park","Downtown")) %>%
  filter(year==2018) %>% group_by(Campus,weekday = wday(date,label=TRUE,week_start = 1)) %>%
  summarise(Count = n()) %>%
  ggplot(aes(weekday,Count)) + 
  geom_bar(stat="identity", fill = "royalblue", alpha = 0.7) +
  facet_grid(~Campus) +
  scale_fill_gradient2(high = "tomato") +
  xlab("Months (2018)") +
  ylab("Count of Crimes") +
  ggtitle("Crime Trend") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "None")

#Daily Crimes by crimestype
dataset %>% 
  filter(year==2018,
         primaryType %in% c("THEFT","BATTERY","CRIMINAL DAMAGE","ASSAULT","ROBBERY")) %>% 
  group_by(primaryType,weekday = wday(date,label=TRUE,week_start = 1)) %>%
  summarise(Count = n()) %>%
  ggplot(aes(weekday,Count)) + 
  geom_bar(stat="identity", fill = "royalblue", alpha = 0.7) +
  facet_grid(~primaryType) +
  scale_fill_gradient2(high = "tomato") +
  xlab("Months (2018)") +
  ylab("Count of Crimes") +
  ggtitle("Crime Trend") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "None")

#Create Hourly Crime Count
dataset %>% 
  mutate(Campus = ifelse(Campus=="Hyde Park" | Campus=="Below Midway Plaissance", "Hyde Park","Downtown")) %>%
  filter(year==2018) %>% group_by(Campus,hour = hour(date)) %>% summarise(Count = n()) %>%
  ggplot(aes(hour,Count)) + 
  geom_bar(stat = "identity", fill="royalblue",alpha=0.7) +
  xlab("Hour of Day") +
  ylab("Count of Crimes") +
  ggtitle("Crime Trend") +
  facet_grid(~Campus) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "None")

#Location Based Crimes
dataset %>% 
  mutate(Campus = ifelse(Campus=="Hyde Park" | Campus=="Below Midway Plaissance", "Hyde Park","Downtown")) %>%
  filter(year==2018) %>% 
  group_by(Campus,locationDesc) %>% 
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ggplot(aes(reorder(locationDesc,Count),Count)) + 
  geom_bar(stat = "identity",fill = "royalblue", alpha = 0.7) +
  coord_flip() + 
  xlab("Location") +
  ylab("Count of Crimes") +
  ggtitle("Crimes Based on Location Type") +
  facet_grid(~Campus) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "None")
#Create map
# data = dataset %>% 
#   filter(Campus=="Downtown",year==2018)
# 
# m = leaflet(data = data) %>% addTiles() %>% 
#   addCircles(lng = ~longitude, lat = ~latitude) %>% addTiles()
