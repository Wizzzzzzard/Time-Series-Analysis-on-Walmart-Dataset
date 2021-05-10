library(tidyr)
library(dplyr)
library(ggplot2)
library(patchwork)
library(lubridate)
library(PerformanceAnalytics)

# Load in 3 tables

sales <- read.csv('C:/Users/elija/OneDrive - University of Strathclyde/DAIP Project 3/WalmartData/sales.csv')
stores <- read.csv('C:/Users/elija/OneDrive - University of Strathclyde/DAIP Project 3/WalmartData/stores.csv')
features <- read.csv('C:/Users/elija/OneDrive - University of Strathclyde/DAIP Project 3/WalmartData/features.csv')

features['Date'] = pd.to_datetime(features['Date'])
sales['Date'] = pd.to_datetime(sales['Date'])

# Join tables into one

sf <- left_join(sales, features, by = c("Store" = "Store", "Date" = "Date"))

ssf <- left_join(sf, stores, by = "Store")

# Drop unnecessary/duplicate columns and change column to date format

ssf <- subset(ssf, select = -c(IsHoliday.x))
ssf %>% 
  rename(IsHoliday = IsHoliday.y) -> ssf

as.Date(ssf$Date, format = "%d/%m/%Y",optional = FALSE) -> ssf$Date

# Subset out data for graph

Date <- subset(ssf, select = c(Store,Date,Weekly_Sales))

Date_Series = xts(ssf$Date,ssf$Weekly_Sales)
unique(Date_Series)

start(Date_Series)
end(Date_Series)
frequency(Date_Series)

Date %>%
  group_by(Store,Date) %>%
  ggplot()+
  geom_line(aes(Date,Weekly_Sales,colour=as.factor(Store))) +
  facet_wrap("Store",scale='free') +
  scale_x_continuous(breaks=seq(1,143,7), name="Weeks")
            