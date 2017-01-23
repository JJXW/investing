
# Set working directory, and import libraries:
setwd("C:/Users/User/Documents/GitHub/investing")
library(ggplot2)
library(scales)
library(lubridate)
library(dplyr)
source("functions.R")

# The following data source:
sp500<-read.csv("stocks.csv", stringsAsFactors=FALSE) %>%
  # Doing stuff with dates:
  # Reformatting the dates to make it readable by the system.
  mutate(Date = as.Date(Date), year = year(Date))


# S&P 500 was started in 1923; prior history is from Shiller.
sp500_act<-sp500 %>% filter(Date >= '1923-01-01')

#Calculate real returns (Reinvested dividends)

sp500_act <- returnCalc(data = sp500_act, pricevar = "Real.Price", dividendvar = "Real.Dividend") 

#Plot real returns

ggplot(sp500_act,aes(x=Date,y=real_return),na.rm=T)+
  geom_path(color="black",alpha=1)+
  labs(title="Returns After Investing",
       x="Month",
       y="Cash Multiplier (After Inflation and Dividends)")

#Calculate return for perfect market timing one buy one sell

sp500_min <- min(sp500_act$real_return, na.rm=T) 
sp500_max <- max(sp500_act$real_return, na.rm=T) 
sp500_timing_1 <- sp500_max / sp500_min

