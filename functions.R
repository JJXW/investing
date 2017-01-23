
returnCalc <- function(data, pricevar, dividendvar = NULL){
  
  data$real_unit_return <- 1 # Start with initial conditions. I invest one dollar at the beginning of the stock market.
  data_now <- data[2:nrow(data),]
  data_lag <- data[1:nrow(data)-1,]
  
  if(!is.null(dividendvar)){
    print("Include Dividend")
    data$real_unit_return[2:nrow(data)] <- 
      # Start with previous value
      data_lag$real_unit_return * 
      # Multiply it by the % change in stock value in the last month
      (((data_now[,pricevar]-data_lag[,pricevar])/
          (data_lag[,pricevar]))+1) +
      # Finally, add last month's dividends to the party; they get reinvested
      (data_lag[,dividendvar]/data_lag[,pricevar])*
      (data_lag$real_unit_return/12)
  } else {
    print("No Dividend")
    data$real_unit_return[2:nrow(data)] <- 
      # Start with previous value
      data_lag$real_unit_return * 
      # Multiply it by the % change in stock value in the last month
      (((data_now[,pricevar]-data_lag[,pricevar])/
          (data_lag[,pricevar]))+1)
  }
  
  data <- data %>%
    mutate(real_return = cumprod(real_unit_return))  
  
}

