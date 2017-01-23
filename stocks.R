
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

#yearly scenarios

sp500_yearly <- sp500_act %>% 
  group_by(year) %>%
  summarize(year_price_high = max(real_return, na.rm=T),
            year_price_low = min(real_return, na.rm=T),
            year_price_avg = mean(real_return, na.rm=T)) %>%
  mutate(join_key = 1)

sp500_max <- sp500_yearly %>%
  filter(year == max(year)) %>%
  rename( max_price_high = year_price_high, 
          max_price_low = year_price_low,
          max_price_avg = year_price_avg,
          max_year = year)

sp500_yearly <- sp500_yearly %>%
  left_join(sp500_max, by = c('join_key')) %>%
  mutate(buy_high_sell_low_return = max_price_low / year_price_high,
         buy_high_sell_low_yearly_return = (max_price_low / year_price_high) ^ (1/(max_year - year)) - 1) %>%
  filter(year < max(year))

ggplot(sp500_yearly,aes(x=year,y=buy_high_sell_low_yearly_return),na.rm=T)+
  geom_path(color="black",alpha=1)+
  labs(title="Returns After Investing",
       x="Year Invested",
       y="Yearly Average Return (After Inflation and Dividends)")

#Calculate return for perfect market timing one buy one sell

sp500_min <- min(sp500_act$real_return, na.rm=T) 
sp500_max <- max(sp500_act$real_return, na.rm=T) 
sp500_timing_1 <- sp500_max / sp500_min

