## load packages
library(tidyverse)
library(dplyr)
library(ggplot2)
library(tseries)
library(urca) # pp test with specified lag term
library(ARDL)
library(lmtest)
## start cleaning data
gdp.season <- read.table("/Users/abnerteng/Downloads/gdp季資料1.csv"
                         , header = T
                         , sep = ",")
gov.debt <- read.table("/Users/abnerteng/Downloads/政府公債-月資料.csv"
                       , header = T
                       , sep = ",")
season <- c("Q1","Q2","Q3","Q4")
num.year <- c(1997:2021)
trans.season <- c(1:100)
for(i in 1997:2021){
  word.year <- as.character(i)
  for(j in 1:4){
    trans.season[4*(i-1997) + j] <- paste0(word.year,season[j])
  }
} 

#創新的MATRIX把好的資料放進去

colnames(gov.debt) <- c("DATE", "TOTAL", "Central_Bank", "Local_Gov")
clean.data <- matrix(0, nrow = 101, ncol = 3)
rownames(clean.data) <- c(trans.season, "2022S1")
colnames(clean.data) <- c("time","GDP_S_mil", "DEBT_S_mil")
for(i in 1:100){
  clean.data[i,1] <- trans.season[i]
}
clean.data[101,1] <- c("2022Q1")
for(i in 0:100){
  clean.data[(i+1),3] <- (gov.debt$TOTAL[1 + (3*i)] +
                            gov.debt$TOTAL[2 + (3*i)] +
                            gov.debt$TOTAL[3 + (3*i)])
}
for(i in 1:101){
  clean.data[i,2] <- gdp.season[144+i,2]
}

#轉data.frame
Final <- data.frame(clean.data)
## my shortcut
setwd("/Users/abnerteng/RDS-final-project/")

## cci index
cci <- read.csv("datasets/CCI new.csv")[,c(1,2)]
cci <- separate(cci, X.年.月, into = c("year", "month"))
cci <- cci %>% mutate(Quarter = ceiling(as.numeric(cci$month) / 3))
index <- cci$不含.雜項類.之總指數
result <- seq(1, length(index), 3)
cci_index_mean <- sapply(result, function(i){mean(index[i:(i+2)])})[1:37]

quarter <- rep(c("Q1", "Q2", "Q3", "Q4"), 10)[1:37]
year <- rep(seq(2013, 2022, 1), 4)
year <- year[order(year)][1:37]

cci_df <- data.frame(year, quarter, cci_index_mean) %>% 
  unite(time, year, quarter, sep = "")

## TWII index
twii <- read.csv("datasets/^TWII.csv")[,c(1,6)]
twii <- separate(twii, Date, into = c("year", "month", "day"))
twii <- subset(twii, select = c("year", "month", "Adj.Close"))
twii <- twii[-c(1:156), ]
twii <- twii %>% mutate(quarter = ceiling(as.numeric(twii$month) / 3))
stockindex <- twii$Adj.Close
result2 <- seq(1, length(stockindex), 3)
stockindex_mean <- sapply(result, function(i) {mean(stockindex[i:(i+2)])})[1:37]

twii_df <- data.frame(year, quarter, stockindex_mean)
twii_df <- twii_df %>% unite(time, year, quarter, sep = "")
head(twii_df)

## salary
salary <- read.csv("datasets/salary.csv", header = F)
salary <- na.omit(salary)
salary <- arrange(salary, V1)
salary <- salary[-c(1:156), ]
salary <- subset(salary, select = -c(V2:V139))
salary$date <- as.Date(paste(salary$V1, "01", sep = "-"), "%YM%m-%d")
salary <- separate(salary, date, into = c("year", "month"))
salary <- salary %>% mutate(quarter = ceiling(as.numeric(salary$month) / 3))
salary_index <- salary$V140
salary_index <- as.numeric(salary_index)
result4 <- seq(1, length(salary_index), 3)
mean_salary <- sapply(result4, function(i) {mean(salary_index[i:(i+2)])})[1:37]

quarter <- rep(c("Q1", "Q2", "Q3", "Q4"), 10)[1:37]
year <- rep(seq(2013,2022,1), 4)
year <- year[order(year)][1:37]
salary_df <- data.frame(year, quarter, mean_salary)
salary_df <- salary_df %>% unite(time, year, quarter, sep = "")

## cpi
cpi <- read.csv("datasets/消費者物價指數.csv", header = F)
cpi <- arrange(cpi, V1)
cpi <- cpi[-c(1:648), ]
cpi <- separate(cpi, V1, into = c("year", "month"))
cpi <- cpi %>% mutate(quarter = ceiling(as.numeric(cpi$month) / 3))
cpiindex <- cpi$V2
result3 <- seq(1, length(cpiindex), 3)
cpiindex_mean <- sapply(result3, function(i) {mean(cpiindex[i:(i+2)])})[1:37]

quarter <- rep(c("Q1", "Q2", "Q3", "Q4"), 10)[1:37]
year <- rep(seq(2013,2022,1), 4)
year <- year[order(year)][1:37]
cpi_df <- data.frame(year, quarter, cpiindex_mean)
cpi_df <- cpi_df %>% unite(time, year, quarter, sep = "")

## housing price
price <- read.csv("datasets/國泰房價指數.csv", header = F)
price <- price[length(price$V1):1,]
quarter <- rep(c("Q1", "Q2", "Q3", "Q4"), 23)[1:89]
year <- rep(seq(2000, 2022, 1), 4)
year <- year[order(year)][1:89]
price_df <- data.frame(year, quarter, price$V2) %>% 
  unite(time, year, quarter, sep = "")
colnames(price_df) <- c("time", "price")
head(price_df)

## population
population <- read.csv("datasets/population.csv", header = F)
population <- population[2:nrow(population), ]
colnames(population) <- c("date","population")
population$date <- as.Date(paste(population$date, "01", sep = "-"), "%YM%m-%d")
population <- separate(population, date, into = c("year", "month"))
population <- population %>% mutate(quarter = ceiling(as.numeric(month) / 3))
population_index <- population$population
population_index <- as.numeric(population_index)
result5 <- seq(1, length(population_index), 3)
mean_population <- sapply(result5, function(i) {mean(population_index[i:(i+2)])})[1:37]

quarter <- rep(c("Q1", "Q2", "Q3", "Q4"), 10)[1:37]
year <- rep(seq(2013,2022,1), 4)
year <- year[order(year)][1:37]
population_df <- data.frame(year, quarter, mean_population)
population_df <- population_df %>% unite(time, year, quarter, sep = "")

## merge data frames
all_variable_df <- merge(cci_df, population_df, by = "time", all = T) %>%
  merge(cpi_df, by = "time", all = T) %>%
  merge(price_df, by = "time", all = T) %>%
  merge(salary_df, by = "time", all = T) %>%
  merge(twii_df, by = "time", all = T) %>%
  merge(Final, by = "time", all = T)

## switch to time series data
cci_ts <- ts(all_variable_df$cci_index_mean, end = c(2022, 1), frequency = 4)
population_ts <- ts(all_variable_df$mean_population, end = c(2021,4), frequency = 4)
twii_ts <- ts(all_variable_df$stockindex_mean, end = c(2022,1), frequency = 4)
salary_ts <- ts(all_variable_df$mean_salary, end = c(2022,1), frequency = 4)
price_ts <- ts(all_variable_df$price, end = c(2022,1), frequency = 4)
cpi_ts <- ts(all_variable_df$cpiindex_mean, end = c(2022,1), frequency = 4)

## make plots, and we can see that almost every plot has trend
plot(cci_ts)
plot(population_ts)
plot(twii_ts)
plot(salary_ts)
plot(price_ts)
plot(cpi_ts)

## unit root test (ADF test), all data have unit root
cci_adf <- adf.test(na.omit(cci_ts), k = 4)
population_adf <- adf.test(na.omit(population_ts), k = 4)
twii_adf <- adf.test(na.omit(twii_ts), k = 4)
salary_adf <- adf.test(na.omit(salary_ts), k = 4)
price_adf <- adf.test(na.omit(price_ts), k = 4)
cpi_adf <- adf.test(na.omit(cpi_ts), k = 4)

## first order difference
cci_I1 <- diff(cci_ts)
twii_I1 <- diff(twii_ts)
population_I1 <- diff(population_ts)
price_I1 <- diff(price_ts)
cpi_I1 <- diff(cpi_ts)
salary_I1 <- diff(salary_ts)
plot(population_I2)
populationI2_adf <- adf.test(na.omit(population_I2), k = 4)

## log difference
cci_log_diff <- diff(log(cci_ts))
cci_log_diff_pp <- ur.pp(na.omit(cci_log_diff), type = "Z-alpha", use.lag = 4)
summary(cci_log_diff_pp)
## pp test 
cciI1_pp <- ur.pp(na.omit(cci_I1), type = 'Z-alpha', use.lag = 4) # p \to 0 , unstationary
summary(cciI1_pp)

## fit ARDL model
ARDL_model <- ardl(data = all_variable_df, formula = GDP_S_mil ~ DEBT_S_mil + cci_index_mean + cpiindex_mean 
                   + mean_population + price + mean_salary + stockindex_mean , order = c(1,1,1,1,1,1,1,1))
rs <- residuals(ARDL_model)
rs_1 <- 
summary(ARDL_model)
plot(ARDL_model)
plot(residuals(ARDL_model))

## ARDL model 2 <- significant data stay
ARDL_model_2 <- ardl(data = all_variable_df, formula = GDP_S_mil ~ DEBT_S_mil + cci_index_mean  
                     + mean_population + stockindex_mean , order = c(1,1,1,1,1))
summary(ARDL_model_2)
plot(residuals(ARDL_model_2))
## serial correlation
lm()
dwtest(residuals(ARDL_model), alternative = "two.sided")
