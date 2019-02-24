#Import necessary libraries
library(tidyverse)
library(lubridate)

#Import dataset
dataset = read.csv("Crime_Weather_Cleaned_2017.csv", stringsAsFactors = F, header = T)

#Date Dim table
DateDataset = dataset %>% distinct(Date)
DateDataset$date_key = 1:nrow(DateDataset)
DateDataset = DateDataset %>% mutate(
  datetime = Date,
  date = date(datetime),
  weekend = ifelse(weekdays(date) %in% c("Saturday","Sunday"), "Weekend", "Weekday"),
  day_of_week = weekdays(date),
  month = month(date),
  month_day = day(date),
  year = year(date),
  week_starting_monday = week(date)
) %>% select(-Date)

#Location Dim table
LocationDataset = dataset %>% 
  distinct(Block,LocationDesc,District,Ward,XCoord,YCoord)
LocationDataset$location_key = 1:nrow(LocationDataset)
LocationDataset = LocationDataset %>%
  select(location_key,Block,LocationDesc,District,Ward,XCoord,YCoord)

#Crime Dim table
CrimeDataset = dataset %>% distinct(IUCR,PrimaryType)
CrimeDataset$IUCR_key = 1:nrow(CrimeDataset)
CrimeDataset = CrimeDataset %>% select(IUCR_key, IUCR,PrimaryType)

#Weather Dim table
WeatherDataset = dataset %>% distinct(WindAvg,Precipitation,Snow,SnowDepth,TempMax,Tmin,
                                      IndFog,IndHeavyFog,IndThunder,IndPellets,IndGlaze,IndSmoke,
                                      IndDriftSnow)
WeatherDataset$weather_key = 1:nrow(WeatherDataset)
WeatherDataset = WeatherDataset %>% select(weather_key,WindAvg,Precipitation,Snow,SnowDepth,TempMax,Tmin,
                                           IndFog,IndHeavyFog,IndThunder,IndPellets,IndGlaze,IndSmoke,
                                           IndDriftSnow)

#Fact Table
FactDataset = inner_join(dataset,DateDataset,by = c("Date"="datetime")) %>%
  select(-c(Date,date,weekend,day_of_week,month,month_day,Year,year,week_starting_monday))

FactDataset = inner_join(FactDataset,WeatherDataset,
                          by = c("WindAvg"="WindAvg","Precipitation"="Precipitation",
                                 "Snow"="Snow","SnowDepth"="SnowDepth",
                                 "TempMax"="TempMax","Tmin"="Tmin",
                                 "IndFog"="IndFog","IndHeavyFog"="IndHeavyFog",
                                 "IndThunder"="IndThunder","IndPellets"="IndPellets",
                                 "IndGlaze"="IndGlaze","IndSmoke"="IndSmoke",
                                 "IndDriftSnow"="IndDriftSnow")) %>%
  select(-c(WindAvg,Precipitation,Snow,SnowDepth,TempMax,Tmin,IndFog,IndHeavyFog,
            IndThunder,IndPellets,IndGlaze,IndSmoke,IndDriftSnow))

FactDataset = inner_join(FactDataset,CrimeDataset,
                          by = c("IUCR"="IUCR","PrimaryType"="PrimaryType")) %>%
  select(-c(IUCR,PrimaryType))

FactDataset = inner_join(FactDataset,LocationDataset,
                          by = c("Block"="Block","LocationDesc"="LocationDesc",
                                 "District"="District","Ward"="Ward",
                                 "XCoord"="XCoord","YCoord"="YCoord")) %>%
  select(-c(Block,LocationDesc,District,Ward,XCoord,YCoord))

FactDataset$Crime_Count = 1

FactDataset = FactDataset %>% select(ID, date_key, location_key, weather_key, IUCR_key,
                                     Arrest, Domestic, Crime_Count, Latitude, Longitude,
                                     TempAvg)

#Write CSVs
write.csv(CrimeDataset, "C:/ProgramData/MySQL/MySQL Server 8.0/Uploads/CrimeDimData.csv", row.names = F)
write.csv(DateDataset, "C:/ProgramData/MySQL/MySQL Server 8.0/Uploads/DateDimData.csv", row.names = F)
write.csv(LocationDataset, "C:/ProgramData/MySQL/MySQL Server 8.0/Uploads/LocDimData.csv", row.names = F)
write.csv(WeatherDataset, "C:/ProgramData/MySQL/MySQL Server 8.0/Uploads/WeatherDimData.csv", row.names = F)
write.csv(FactDataset, "C:/ProgramData/MySQL/MySQL Server 8.0/Uploads/FactData.csv", row.names = F)
