library(tidyquant)
library(ggplot2)
in_code <- 2330
in_url <- paste0 ("https://query1.finance.yahoo.com/v7/finance/download/",in_code,".TW?period1=1594339200&period2=1652400000&interval=1d&events=history&includeAdjustedClose=true")
Ind_stock <- read.csv(in_url)
remove_null <- rowSums(Ind_stock == "null")
Ind_stock <- Ind_stock[remove_null ==0, ]
Ind_stock_data <- data.frame(date = as.Date(Ind_stock$Date), price = as.numeric(as.character(Ind_stock$Adj.Close)))
Ind_stock_data <- Ind_stock_data[Ind_stock_data$date >= as.Date("2010-01-01"), ]
ggplot(Ind_stock_data, aes(x = date, y = price)) +
  geom_line() +
  labs( x = "Date", y = "Stock") +
  scale_x_date(date_breaks = "2 years", date_labels = c("%b %y")) +
  scale_y_continuous(breaks = seq(0,700,50))

ma_days <- 20

gen_bb <- function(ma_days)
{
  bb <- matrix(0, nrow(Ind_stock_data), 3)
  for(i in ma_days : nrow(Ind_stock_data))
  {
    bb[i,1] <- mean(Ind_stock_data$price[(i-ma_days + 1) : (i)]) -1.96*sd(Ind_stock_data$price[(i-ma_days+1):(i)])
    bb[i,2] <- mean(Ind_stock_data$price[(i-ma_days + 1) : (i)])
    bb[i,3] <- mean(Ind_stock_data$price[(i-ma_days + 1) : (i)]) +1.96*sd(Ind_stock_data$price[(i-ma_days+1):(i)])
  }
  bb_data <- data.frame(date = as.Date(Ind_stock_data$date), bbl=bb[,1], bbc = bb[,2], bbu = bb[,3])
  bb_data <- bb_data[bb_data$bbl !=0, ]
  return(bb_data)
}
bb_plot_data <- gen_bb(ma_days)

bb_data <- Ind_stock_data[ma_days:nrow(Ind_stock_data), ]
bb_data$bbl <- bb_plot_data$bbl
bb_data$bbu <- bb_plot_data$bbu
bb_data$bbc <- bb_plot_data$bbc

ggplot(bb_data, aes(x = date, y = price)) +
  geom_line() +
  labs( x = "Date", y = "Ind_stock") +
  scale_x_date(date_breaks = "2 years", date_labels = c("%b %y")) +
  geom_line( data = bb_plot_data, aes(x = date, y = bbc), size = 1, color = "blue") +
  geom_line( data = bb_plot_data, aes(x = date, y = bbl), size = 1, color = "pink") +
  geom_line( data = bb_plot_data, aes(x = date, y = bbu), size = 1, color = "pink") +
  geom_ribbon(aes(ymin = bbl, ymax = bbu, x = date, fill = "band"), alpha = 0.2, color = "yellow", fill = "red")

strategy <- array("stay",nrow(bb_data))
for(i in 2:nrow(bb_data))
{
  if(bb_data$price[i] < bb_data$bbl[i] || bb_data$price[i] > bb_data$bbc[i])
  {
    strategy[i] <- "long"
  } else {
    if(bb_data$price[i] > bb_data$bbu[i] || bb_data$price[i] < bb_data$bbc[i])
    {
      strategy[i] <- "short"
    } else {
      strategy[i] <- strategy[i-1] 
    }
  } 
}

bb_data$strategy <- strategy

if(bb_data$strategy[1]=="long")
{
  long_date_start <- bb_data$date[1]
  long_date_end <- bb_data$date[(which(bb_data$strategy=="short")[1]-1)]
  counter <- which(bb_data$strategy=="short")[1]+1
} else {
  long_date_start <- bb_data$date[which(bb_data$strategy=="long")[1]]
  long_date_end <- NULL
  counter <- which(bb_data$strategy=="long")[1]+1
}

for(i in counter:nrow(bb_data))
{
  if(bb_data$strategy[i]=="long" && bb_data$strategy[(i-1)]=="short")
  {
    long_date_start <- c(long_date_start,bb_data$date[i])
  } 
  if(bb_data$strategy[i]=="short" && bb_data$strategy[(i-1)]=="long")
  {
    if(length(long_date_end)==0)
    {
      long_date_end <- bb_data$date[(i-1)]
    } else {
      long_date_end <- c(long_date_end,bb_data$date[(i-1)])
    }
  }
}


if(length(long_date_start) > length(long_date_end))
{
  long_date_end <- c(long_date_end,Ind_stock_data$date[nrow(Ind_stock_data)])
} 


long_date_start <- as.Date(long_date_start)
long_date_end <- as.Date(long_date_end)

shade_data <- data.frame(start=long_date_start,
                         end=long_date_end,
                         ymin=-Inf,ymax=Inf)

ggplot(bb_data, aes(x=date, y=price)) +
  geom_rect(data = shade_data, aes(xmin = start , xmax = end, ymin = -Inf, ymax = Inf),
            inherit.aes=FALSE, alpha = 0.4, fill = c("lightblue"))+
  geom_line(aes(x=date, y=price,color="black")) +
  labs(x = "Time",y="Stock") +  
  scale_x_date(date_breaks = "4 years",date_labels=c("%b %y"))  +
  geom_line(data = bb_data, aes(x = date, y = bbc,color="blue"), size = 1) +
  geom_ribbon(aes(ymin=bbl, ymax=bbu,x=date, fill = "band"), alpha = 0.1,colour="yellow",fill="red")+
  scale_color_identity(name = "",
                       breaks = c("black", "blue"),
                       labels = c("Original", paste(ma_days," days MA")),
                       guide = "legend")

trade_return <- (
  log(Ind_stock_data$price[Ind_stock_data$date %in% long_date_end])-
    log(Ind_stock_data$price[Ind_stock_data$date %in% long_date_start])
)*100

sum(trade_return)

