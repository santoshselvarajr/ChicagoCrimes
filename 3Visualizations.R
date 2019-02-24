#Load libraries
library(tidyverse)
library(ggplot2)
library(lubridate)
setwd("~/MSCA/3. Data Engineering Platforms/TheRealEstate")
#Read necessary data
dataset = read.csv("Crime20161718.csv", stringsAsFactors = F, header = T)
dataset$Date = as_datetime(dataset$Date)
data = read.csv("CrimeWeather2010.csv", stringsAsFactors = F, header = T)
data$Date = as_datetime(mdy(data$Date))

#Crime Counts across Years
data %>%
  ggplot(aes(Date,TotalCrimes)) + 
  geom_point(col = "tomato", alpha = 0.3) + 
  geom_smooth(se = F, col="tomato") +
  geom_point(aes(Date,Arrest),col = "royalblue", alpha = 0.3) +
  geom_smooth(aes(Date,Arrest),se = F, col="royalblue") +
  xlab("Year") +
  ylab("Total Crimes/Arrests") +
  ggtitle("Crime Trend Across Years") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))

#Crime Counts Monthly
data %>% filter(year(Date)==2017) %>%
  group_by(month = month(Date,label=T)) %>%
  summarise(Count = sum(TotalCrimes)) %>%
  ggplot(aes(month,Count)) + 
  geom_bar(stat="identity", fill = "royalblue", alpha = 0.7) +
  scale_fill_gradient2(high = "tomato") +
  xlab("Months (2018)") +
  ylab("Count of Crimes") +
  ggtitle("Crime Trend") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "None")

#Crime Counts Monthly across Years
data %>% filter(between(year(Date),2010,2017)) %>%
  group_by(Year=year(Date),month = month(Date,label=T)) %>%
  summarise(Count = sum(TotalCrimes)) %>%
  ggplot(aes(month,Count,group=1)) + 
  geom_point(col = "royalblue", alpha = 0.5) + 
  geom_line(col = "royalblue") + 
  facet_wrap(~Year) +
  theme_bw() +
  xlab("Months") +
  ylab("Count of Crimes") +
  ggtitle("Crimes Across Years") +
  theme(plot.title = element_text(hjust = 0.5))
  

#Crime Counts - Day of the Week and Month of the Year
data %>% filter(year(Date)==2017) %>%
  group_by(month = month(Date,label=T), 
           weekday = wday(Date,label=TRUE,week_start = 1)) %>%
  summarise(Count = sum(TotalCrimes)) %>% 
  ggplot(aes(x=month,y=weekday,fill=Count)) +
  geom_tile(color = "black") +
  xlab("Month of the Year") +
  ylab("Day of the Week") +
  ggtitle("Crime Counts Across the Year - Daily") +
  scale_fill_gradient(name = "Crime Counts",low = "#F5F5DC",high = "#36648B") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#Crime Counts - Hour of the day and Month of the Year
dataset %>% filter(year(Date)==2017) %>%
  group_by(month = month(Date,label=T), 
           hour = hour(Date)) %>%
  summarise(Count = n()) %>% 
  ggplot(aes(x=month,y=as.factor(hour),fill=Count)) +
  geom_tile(color = "black") +
  xlab("Month of the Year") +
  ylab("Hour of the Day") +
  ggtitle("Crime Counts Across the Year - Hourly") +
  scale_fill_gradient(name = "Crime Counts",low = "#F5F5DC",high = "#36648B") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#Crime Counts - Hour of the day and Day of the Week
dataset %>% filter(year(Date)==2017) %>%
  group_by(weekday = wday(Date,label=TRUE,week_start = 1), 
           hour = hour(Date)) %>%
  summarise(Count = n()) %>% 
  ggplot(aes(x=weekday,y=as.factor(hour),fill=Count)) +
  geom_tile(color = "black") +
  xlab("Month of the Year") +
  ylab("Hour of the Day") +
  ggtitle("Crime Counts Across the Year - Hourly") +
  scale_fill_gradient(name = "Crime Counts",low = "#F5F5DC",high = "#36648B") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#Create Crime Type Counts
dataset %>% filter(Year==2018) %>% 
  group_by(PrimaryType) %>% 
  summarise(Count = n()) %>%
  ggplot(aes(reorder(PrimaryType,Count),Count)) + 
  geom_bar(stat="identity", fill = "royalblue", alpha = 0.7) + 
  coord_flip() +
  xlab("Crime Type") +
  ylab("Number of Crimes") +
  ggtitle("Crimes Based on Crime Type") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

#Daily crimes - All Crimes
data %>% 
  filter(year(Date)==2018) %>% 
  group_by(weekday = wday(Date,label=TRUE,week_start = 1)) %>%
  summarise(Count = sum(TotalCrimes)) %>%
  ggplot(aes(weekday,Count)) + 
  geom_bar(stat="identity", fill = "royalblue", alpha = 0.7) +
  scale_fill_gradient2(high = "tomato") +
  xlab("Months (2018)") +
  ylab("Count of Crimes") +
  ggtitle("Crime Trend") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "None")

#Daily Crimes - Crime Type
dataset %>% 
  filter(Year==2018,
         PrimaryType %in% c("THEFT","BATTERY","CRIMINAL DAMAGE","ASSAULT","ROBBERY")) %>% 
  group_by(PrimaryType,weekday = wday(Date,label=TRUE,week_start = 1)) %>%
  summarise(Count = n()) %>%
  ggplot(aes(weekday,Count)) + 
  geom_bar(stat="identity", fill = "royalblue", alpha = 0.7) +
  facet_wrap(~PrimaryType) +
  scale_fill_gradient2(high = "tomato") +
  xlab("Months (2018)") +
  ylab("Count of Crimes") +
  ggtitle("Crime Trend") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "None")

#Hourly Crime Data
dataset %>% 
  filter(Year==2018) %>% group_by(hour = hour(Date)) %>% summarise(Count = n()) %>%
  ggplot(aes(hour,Count)) + 
  geom_bar(stat = "identity", fill="royalblue",alpha=0.7) +
  xlab("Hour of Day") +
  ylab("Count of Crimes") +
  ggtitle("Crime Trend - Hourly") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "None")

#Location Based Crimes
dataset %>% 
  filter(Year==2018) %>% 
  group_by(LocationDesc) %>% 
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  top_n(25) %>%
  ggplot(aes(reorder(LocationDesc,Count),Count)) + 
  geom_bar(stat = "identity",fill = "royalblue", alpha = 0.7) +
  coord_flip() + 
  xlab("Location") +
  ylab("Count of Crimes") +
  ggtitle("Crimes Based on Location Type (Top 25)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "None")

#Temperature Heat Map
data %>% filter(year(Date)==2017) %>%
  group_by(month = month(Date,label=T), 
           weekday = wday(Date,label=TRUE,week_start = 1)) %>%
  summarise(Temp = mean(TempAvg)) %>% 
  ggplot(aes(x=month,y=weekday,fill=Temp)) +
  geom_tile(color = "black") +
  xlab("Month of the Year") +
  ylab("Day of the Week") +
  ggtitle("Temperature across the Year") +
  scale_fill_gradient(name = "Temperature(in F)",low = "#F5F5DC",high = "#36648B") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#Basic Models
#Linear Regression: Theft
#Creating Dataset
TheftData = dataset %>% filter(PrimaryType=="THEFT",Year==2018) %>%
  group_by(date = date(Date)) %>%
  summarise(TotalCrimes = n(), TempAvg = mean(TempAvg), 
            WindAvg = mean(WindAvg), Precipitation = mean(Precipitation),
            Snow = mean(Snow), SnowDepth = mean(SnowDepth), 
            IndFog = mean(IndFog), IndHeavyFog = mean(IndHeavyFog),
            IndThunder = mean(IndThunder), IndPellets = mean(IndPellets),
            IndGlaze = mean(IndGlaze), IndSmoke = mean(IndSmoke),
            IndDriftSnow = mean(IndDriftSnow)) %>%
  mutate_at(vars(IndFog,IndHeavyFog,IndThunder,IndPellets,IndGlaze,IndSmoke,IndDriftSnow), factor)
#Building Linear Model
lm_theft = lm(TotalCrimes~TempAvg, data = TheftData)
summary(lm_theft)
#Checking Assumptions
#Homoscedasity
TheftData %>% 
  ggplot(aes(x=lm_theft$fitted.values,y=lm_theft$residuals)) + 
  geom_point(col = "royalblue", alpha = 0.7, size = 4) + 
  geom_smooth(se=F, size=2) +
  xlab("Predicted Crime Counts") +
  ylab("Model Residuals") +
  ggtitle("Assumption Check: Homoscedasity") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

#Homoscedasity
TheftData %>% 
  ggplot(aes(x=lm_theft$residuals)) + 
  geom_density(fill = "royalblue", alpha = 0.5) + 
  geom_line(aes(x=seq(-70,70,length.out = 295),y=dnorm(seq(-70,70,length.out = 295),sd=sd(lm_theft$residuals))), size = 2, color = "seagreen") +
  xlab("Residuals") +
  ylab("Density") +
  ggtitle("Assumption Check: Normality") +
  theme_bw()

#Plotting the linear Regression
TheftData %>%
  ggplot(aes(TempAvg,TotalCrimes)) + 
  geom_point(col = "royalblue", alpha = 0.7, size = 4) +
  geom_line(data=TheftData,aes(x=TempAvg,y=lm_theft$fitted.values), color = "royalblue",size = 2) +
  xlab("Average Temperature (in Farheneit)") +
  ylab("Total Crimes") +
  ggtitle("Effect of Temperature on Crime - THEFT") +
  theme_bw()

#Building multiple linear models
CrimeData = dataset %>% filter(Year==2017 | Year==2016) %>%
  group_by(date = date(Date), PrimaryType) %>%
  summarise(TotalCrimes = n(), TempAvg = mean(TempAvg))
  
#Create Model for all Primary Types
CrimeModel = function(df){
  lm(TotalCrimes ~ TempAvg, data = df)
}
#Run multiple models
AllCrimeModels = CrimeData %>% group_by(PrimaryType) %>% nest() %>%
  mutate(model = map(data,CrimeModel),
         coef = map(model,broom::tidy))
#Get all the coefficients
AllCrimeModels = AllCrimeModels %>% select(PrimaryType,coef) %>% unnest()
  
#Checking the Estimate variance across models 
AllCrimeModels %>%
  mutate(term = fct_recode(term,
                           Intercept = "(Intercept)",
                           Slope = "TempAvg")) %>%
  ggplot(aes(estimate, fill = term)) +
  geom_density(show.legend = FALSE, alpha = 0.5) +
  geom_histogram(aes(y=..density..),col = "black",fill="lightgrey",alpha=0.5) +
  facet_wrap(~term, scales = "free") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  labs(x="Estimates",y=NULL)

#Build all linear models
AllCrimeModels = AllCrimeModels %>%
  select(PrimaryType,term,estimate) %>%
  mutate(Intercept = ifelse(term=="(Intercept)", estimate,0),
         TempEstimate = ifelse(term=="TempAvg", estimate,0)) %>%
  group_by(PrimaryType) %>% summarise(Intercept = sum(Intercept),
                                      TempEstimate = sum(TempEstimate))

#Combine Data with Slopes and Intercepts from Linear Model
CrimeData = inner_join(CrimeData,AllCrimeModels, by = ("PrimaryType"="PrimaryType"))

#Visualize the top crimes
CrimeData %>% 
  filter(PrimaryType %in% c("THEFT","BATTERY","CRIMINAL DAMAGE","ASSAULT",
                            "DECEPTIVE PRACTICE","NARCOTICS","BURGLARY",
                            "MOTOR VEHICLE THEFT","ROBBERY")) %>%
  ggplot(aes(TempAvg,TotalCrimes)) +
  geom_point(col = "gray", alpha = 0.5, size = 2) +
  geom_line(aes(TempAvg, TempAvg*TempEstimate + Intercept), col = "royalblue",size=2) +
  facet_wrap(~PrimaryType) +
  theme_bw() +
  labs(x="Average Temperature (in F)",y="Total Crimes")

CrimeData %>% filter(year(date)==2017) %>%
  filter(PrimaryType %in% c("THEFT","BATTERY","CRIMINAL DAMAGE","ASSAULT")) %>%
  group_by(PrimaryType) %>% summarise(sum(TotalCrimes))

write.csv(CrimeData, "CrimeDataModels.csv", row.names = F)

#x = dataset %>% group_by(PrimaryType) %>% summarise(Count = n())
#y = dataset %>% group_by(LocationDesc) %>% summarise(Count = n())

#write.csv(x, "CrimeTypeCount.csv")
#write.csv(y, "LocationCount.csv")