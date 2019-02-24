#Import necessary libraries
library(tidyverse)
library(ggplot2)
library(lubridate)

#Import necessary files
dataset = read.csv("Crimes_-_2001_to_present.csv",stringsAsFactors = FALSE, header = TRUE)

dataset = dataset %>%
  filter(Year >= 2010) %>%
  select(-c(Case.Number,Updated.On,Location)) %>%
  mutate(Arrest = ifelse(Arrest=="true",1,0),
         Domestic = ifelse(Domestic=="true",1,0))
#Rename columns 
colnames(dataset) = c("ID","Date","Block","IUCR","PrimaryType",
                      "Description","LocationDesc","Arrest","Domestic",
                      "Beat","District","Ward","CommunityArea","FBICode",
                      "XCoord","YCoord","Year","Latitude","Longitude")
#Convert date into a meaningful date
dataset$Date = as_datetime(mdy_hms(dataset$Date))

#Write CSV with raw data
#write.csv(dataset,"Crime2010Raw.csv", row.names = F)

#Rollup for day level crime data
data = dataset %>% 
  group_by(Date=date(Date)) %>% 
  summarise(Arrest = sum(Arrest, na.rm = T), 
            Domestic = sum(Domestic, na.rm = T),
            TotalCrimes = n()) %>%
  arrange(Date)

write.csv(data, "CrimeDayLevel2010.csv", row.names = F)

#Keep recent one year data (2017)
data = dataset %>% 
  filter(Year>=2016)

#write.csv(data, "Crime20161718.csv", row.names = F)

#Clean weather data
weatherdata = read.csv("Weather2010.csv", header = T)

#Add Weather Data to 3 Year Crime Data
dataset = dataset %>% mutate(dateid = date(Date)) 
data = data %>% mutate(Date = date(Date))
bigdata = inner_join(dataset,data,by = c("dateid" = "Date"))

bigdata = bigdata %>% mutate(Arrest = Arrest.x, Domestic = Domestic.x) %>% select(-c(Arrest.x,Domestic.x))

write.csv(bigdata, "Crime20161718.csv", row.names = F)
