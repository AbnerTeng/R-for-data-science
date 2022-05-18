library(tidyquant)
library(ggplot2)
in_code <- 2330
in_url <- paste0 ("https://query1.finance.yahoo.com/v7/finance/download/",in_code,".TW?period1=1594339200&period2=1652400000&interval=1d&events=history&includeAdjustedClose=true")
Ind_stock <- read.csv(in_url)
remove_null <- rowSums(Ind_stock == "null")
Ind_stock <- Ind_stock[remove_null ==0, ]
Ind_stock_data <- data.frame(date = as.Date(Ind_stock$Date), price = as.numeric(as.character(Ind_stock$Adj.Close)))
Ind_stock_data <- Ind_stock_data[Ind_stock_data$date >= as.Date("2010-01-01"), ]
ma_days <- 20
ma <- array(0,nrow(Ind_stock_data)) ## let ma be an array of 0
for(i in ma_days : nrow(Ind_stock_data)) ## 20 to 0
{
  ma[i] <- mean(Ind_stock_data$price[(i-ma_days+1) : (i)])
}
Ind_stock_data$ma <- ma
ma_data <- data.frame(date = as.Date(Ind_stock_data$date), ma = ma)
ma_data <- ma_data[ma_data$ma!=0, ]  ## delete the 0
Ind_stock_data <- Ind_stock_data[Ind_stock_data$ma !=0, ]

ggplot(Ind_stock_data, aes(x = date, y = price)) +
  geom_line(aes(x = date, y = price), color = "black") +
  labs( x = "Date", y = "Ind_stock") +
  scale_x_date(date_breaks = "2 years", date_labels = c("%b %y")) +
  scale_y_continuous(breaks = seq(8000,18000,2000)) +
  geom_line(data = ma_data, aes(x = date, y = ma), color = "red", lwd = 1) +
  ggtitle("MA by hand")

gen_ma <- function(ma_days) ## a function of general ma
{
  ma <- array(0, nrow(Ind_stock_data))
  for (i in ma_days : nrow(Ind_stock_data))
  {
    ma[i] <- mean(Ind_stock_data$price[(i-ma_days+1) : (i)])
  }
  ma_data <- data.frame(date = as.Date(Ind_stock_data$date), ma=ma)
  ma_data <- ma_data[ma_data$ma != 0, ]
  return(ma_data)
}

ggplot(Ind_stock_data, aes(x = date, y = price)) +
  geom_line(aes(x = date, y = price), color = "black") +
  labs( x = "Date", y = "Ind_stock") +
  scale_x_date(date_breaks = "2 year", date_labels = ("%b %y")) +
  geom_line(data = gen_ma(250), aes(x = date, y = ma, color = "blue"), lwd = 1) +
  geom_line(data = gen_ma(60), aes(x = date, y = ma, color = "red"), lwd = 1) +
  geom_line(data = gen_ma(20), aes(x = date, y = ma, color = "green"), lwd = 1) +
  scale_color_identity(name = "",
                       breaks = c("black", "blue", "red", "green"),
                       labels = c("original", "250 days MA", "60 days MA", "20 days MA"),
                       guide = "legend")

## trading strategy using ma
strategy <- array("", nrow(Ind_stock_data))
for(i in 1 : nrow(Ind_stock_data))
{
  if(Ind_stock_data$price[i] > Ind_stock_data$ma[i])
  {
    strategy[i] <- "long"
  }
  else
  {
    strategy[i] <- "short"
  }  
} 

Ind_stock_data$strategy <- strategy

if (Ind_stock_data$strategy[1] == "long")
{
  long_date_start <- Ind_stock_data$date[1]
  long_date_end <- Ind_stock_data$date[(which(Ind_stock_data$strategy == "short")[1]-1)]
  counter <- which(Ind_stock_data$strategy == "short")[1]+1
} else {
  long_date_start <- Ind_stock_data$date[which(Ind_stock_data$strategy == "long")[1]]
  long_date_end <- NULL
  counter <- which(Ind_stock_data$strategy == "long")[1]+1
}

for(i in counter : nrow(Ind_stock_data))
{
  if(Ind_stock_data$strategy[i] == "long" && Ind_stock_data$strategy[(i-1)] == "short")
  {
    long_date_start <- c(long_date_start, Ind_stock_data$date[i])
  }
  if(Ind_stock_data$strategy[i] == "short" && Ind_stock_data$strategy[(i-1)] == "long")
  {
    long_date_end <- c(long_date_end, Ind_stock_data$date[(i-1)])
  }
}

long_date_start <- as.Date(long_date_start)
long_date_end <- as.Date(long_date_end)

shade_data <- data.frame(start = long_date_start,
                         end = long_date_end,
                         ymin = -Inf, ymax = Inf)

ggplot(Ind_stock_data, aes(x = date, y = price)) +
  geom_rect(data = shade_data, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE, alpha = 0.8, fill = c("lightblue")) +
  geom_line(aes(x = date, y = price), color = "black") +
  labs( x = "Date", y = "Ind_stock") +
  scale_x_date(date_breaks = "2 year", date_labels = ("%b %y")) +
  geom_line(data = gen_ma(250), aes(x = date, y = ma, color = "blue"), lwd = 1) +
  geom_line(data = gen_ma(60), aes(x = date, y = ma, color = "red"), lwd = 1) +
  geom_line(data = gen_ma(20), aes(x = date, y = ma, color = "green"), lwd = 1) +
  scale_color_identity(name = "",
                       breaks = c("black", "blue", "red", "green"),
                       labels = c("original", "250 days MA", "60 days MA", "20 days MA"),
                       guide = "legend")
## calculate trade return
trade_return <- (
  log(Ind_stock_data$price[Ind_stock_data$date %in% long_date_end]) -
    log(Ind_stock_data$price[Ind_stock_data$date %in% long_date_start])
)*100
sum(trade_return)

## write a function
return_calc <- function(ma_days)
{
  Ind_stock_data <- data.frame(date = as.Date(Ind_stock$Date), price = as.numeric(as.character(Ind_stock$Adj.Close)))
  Ind_stock_data <- Ind_stock_data[Ind_stock_data$date >= as.Date("2010-01-01"), ]
  ma <- array(0,nrow(Ind_stock_data)) ## let ma be an array of 0
  for(i in ma_days : nrow(Ind_stock_data)) ## 20 to 0
  {
    ma[i] <- mean(Ind_stock_data$price[(i-ma_days+1) : (i)])
  }
  Ind_stock_data$ma <- ma
  ma_data <- data.frame(date = as.Date(Ind_stock_data$date), ma = ma)
  ma_data <- ma_data[ma_data$ma!=0, ]  ## delete the 0
  
  strategy <- array("", nrow(Ind_stock_data))
  for(i in 1 : nrow(Ind_stock_data))
  {
    if(Ind_stock_data$price[i] > Ind_stock_data$ma[i])
    {
      strategy[i] <- "long"
    } else
    {
      strategy[i] <- "short"
    }  
  } 
  
  Ind_stock_data$strategy <- strategy
  
  if (Ind_stock_data$strategy[1] == "long")
  {
    long_date_start <- Ind_stock_data$date[1]
    long_date_end <- Ind_stock_data$date[(which(Ind_stock_data$strategy == "short")[1]-1)]
    counter <- which(Ind_stock_data$strategy == "short")[1]+1
  } else {
    long_date_start <- Ind_stock_data$date[which(Ind_stock_data$strategy == "long")[1]]
    long_date_end <- NULL
    counter <- which(Ind_stock_data$strategy == "long")[1]+1
  }
  
  for(i in counter : nrow(Ind_stock_data))
  {
    if(Ind_stock_data$strategy[i] == "long" && Ind_stock_data$strategy[(i-1)] == "short")
    {
      long_date_start <- c(long_date_start, Ind_stock_data$date[i])
    }
    if(Ind_stock_data$strategy[i] == "short" && Ind_stock_data$strategy[(i-1)] == "long")
    {
      long_date_end <- c(long_date_end, Ind_stock_data$date[(i-1)])
    }
  }
  
  long_date_start <- as.Date(long_date_start)
  long_date_end <- as.Date(long_date_end)
  
  trade_return <- (
    log(Ind_stock_data$price[Ind_stock_data$date %in% long_date_end]) -
      log(Ind_stock_data$price[Ind_stock_data$date %in% long_date_start])
  )*100
  sum(trade_return)
}
