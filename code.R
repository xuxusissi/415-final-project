library(tidyverse)
library(httr)
library(jsonlite)
library(magrittr)
library(ggplot2)
library(tseries)
library(timeSeries)
library(dplyr)
library(ggthemes)


#Bitcoin real time
bitcoin <- fromJSON('https://api.coindesk.com/v1/bpi/currentprice.json')
bpi <- bitcoin$bpi
 
code <- bpi %>%
  sapply(function(x) x$code) %>% 
  unlist()
symbol <- bpi %>%
  sapply(function(x) x$symbol) %>% 
  unlist()
rate <- bpi %>%
  sapply(function(x) x$rate) %>% 
  unlist()
description <- bpi %>%
  sapply(function(x) x$description) %>% 
  unlist()
rate_float <- bpi %>%
  sapply(function(x) x$rate_float) %>% 
  unlist()

bitcoin.df <- data.frame(code, symbol, rate, description, rate_float)
bitcoin.df
bitcoin.df$currency <- rownames(bitcoin.df)
write.csv(bitcoin.df, file="bitcoin.csv", row.names = F)

btc.data <- read.csv(file='bitcoin.csv', header=TRUE)
btc.data <- btc.data %>% select(currency, everything())


myvar <- c("currency", "rate")
btc.data <- btc.data[myvar]
btc.data["crypto"] <- "BTC"
btc.data$rate <- as.numeric(gsub(",", "", btc.data$rate))


 
#barplot of real-time bitcoin price
btc.bar <- ggplot(data=btc.data, aes(x=currency, y=rate)) +
  geom_bar(stat="identity", fill="lightblue") +
  geom_text(aes(label=rate), vjust=-0.3, size=3.5) + 
  ggtitle("Bitcoin Price Index")


#bitcoin price 2017
btc1 <- read.csv(file="bitcoin_his.csv", header=TRUE)
btc.data.recent <- btc.data.recent %>% select(date, everything())

#compute mothly average for 2017
btc1$date <- as.Date(btc1$date)
btc1$Month <- months(btc1$date)
btc1$Month <- factor(btc1$Month, levels = month.name)
monthly.df <- aggregate(rate_in_USD ~ Month , btc1 , mean)
monthly.df <- monthly.df %>% mutate(rate_in_USD = round(rate_in_USD, 0))

#plot-bitcoin monthly average price in USD
plot_monthly <- ggplot(data=monthly.df, aes(x=Month, y=rate_in_USD, group=1)) +
  geom_line()+
  geom_point()+
  geom_text(aes(label=rate_in_USD), vjust=-0.3, size=3.5) 
plot_monthly + ggtitle("Bitcoin monthly average price in 2017")
  



###Bitcoin recent month
bitcoin.recent <- fromJSON('https://api.coindesk.com/v1/bpi/historical/close.json?start=2018-03-20&end=2018-04-20')
bpi.recent <- bitcoin.recent$bpi
rate_in_USD <- bpi.recent %>%
  sapply(function(x) x) %>% 
  unlist()

bitcoin.recent.df <- data.frame(rate_in_USD)
bitcoin.recent.df$date <- rownames(bitcoin.recent.df)
write.csv(bitcoin.recent.df, file="bitcoin_recent.csv", row.names = F)

btc.data.recent <- read.csv(file='bitcoin_recent.csv', header=TRUE)
btc.data.recent <- btc.data.recent %>% select(date, everything())
#View(btc.data.recent)

inds.recent <- seq(as.Date("2018-03-24"), as.Date("2018-04-24"), by = "day")
btc.recent <- ts(btc.data.recent$rate_in_USD,     
           start = c(2018, as.numeric(format(inds.recent[1], "%j"))),
           frequency = 365)
plot(btc.recent)




###Etherium real time
etherium <- GET('https://bitcoinaverage-global-ethereum-index-v1.p.mashape.com/indices/global/ticker/all?crypto=ETH&fiat=USD%2CEUR%2CGBP', add_headers('X-Mashape-Key'='4UM5yALrNbmsh7inhlyPUnXQtUcFp14145IjsnLwpMRDsBbA0c', 'Accept'='application/json'))
eth <- content(etherium)

rate <- eth %>%
  sapply(function(x) x$last) %>% 
  unlist()

eth.df <- data.frame(rate)
eth.df$currency <- rownames(eth.df)
eth.df
write.csv(eth.df, file="eth.csv", row.names = F)

eth.data <- read.csv(file='eth.csv', header=TRUE)
eth.data <- eth.data %>% select(currency, everything())
#View(eth.data)

eth.data$currency <- as.character(eth.data$currency)
eth.data$currency[eth.data$currency=="ETHEUR"] <- "EUR"
eth.data$currency[eth.data$currency=="ETHGBP"] <- "GBP"
eth.data$currency[eth.data$currency=="ETHUSD"] <- "USD"
eth.data$currency <- as.factor(eth.data$currency)
eth.data["crypto"] <- "ETH"


#bar plot
eth.bar <- ggplot(data=eth.data, aes(x=currency, y=rate)) +
  geom_bar(stat="identity", fill="pink") +
  geom_text(aes(label=rate), vjust=-0.3, size=3.5)
eth.bar + ggtitle("Etherium Price Index")


##combine btc and eth & barplot
btc_and_eth <- rbind(btc.data, eth.data)

barplot <- ggplot(data=btc_and_eth, aes(x=currency, y=rate, fill=crypto)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_text(aes(label=rate), vjust=-0.3, size=3.5) + 
  ggtitle("Bitcoin vs. Etherium")+ 
  theme(plot.title = element_text(size = 20, face = "bold")) 
barplot


#######################
###2 years historical data
#BTC
bitcoin.his2 <- fromJSON('https://api.coindesk.com/v1/bpi/historical/close.json?start=2016-01-01&end=2017-12-31')
bpi.his2 <- bitcoin.his2$bpi
priceUSD <- bpi.his2 %>%
  sapply(function(x) x) %>% 
  unlist()

bitcoin.his2.df <- data.frame(priceUSD)
bitcoin.his2.df
bitcoin.his2.df$date <- rownames(bitcoin.his2.df)
write.csv(bitcoin.his2.df, file="BTC_USD2.csv", row.names = F)

btc2 <- read.csv(file='BTC_USD2.csv', header=TRUE)
btc2 <- btc2 %>% select(date, everything())

#calculate volatility
btc2["change"] <- NA
for (i in 2:731){
  btc2$change[i]=((btc2$priceUSD[i]-btc2$priceUSD[i-1])/btc2$priceUSD[i-1])*100
  btc2$change[i]=round(btc2$change[i], digits=2)
}
btc2$change[1] <- 0.00

btc2_sd <- sd(btc2$change)
btc2_sd
btc2_vol <- round(btc2_sd*sqrt(731), digits=2)
btc2_vol
#the volatility of bitcoin over past two years is 105.73%



#EUR
eur_his2 <- read.csv(file="EUR_USD2.csv", header=TRUE)
eur_his2<- eur_his2[seq(dim(eur_his2)[1],1),]
eur_his2$Date <- as.Date(eur_his2$Date, format="%d-%B-%y")
eur_his2 <- eur_his2 %>% select(Date, Price, Change)
colnames(eur_his2) <- c("date", "price", "change")
eur_his2$change <- as.character(eur_his2$change)
eur_his2$change <- as.numeric(substr(eur_his2$change,0,nchar(eur_his2$change)-1))


eur_his2_sd <- sd(eur_his2$change)
eur_his2_sd
eur_his2_vol <- round(eur_his2_sd*sqrt(609), digits=2)
eur_his2_vol
#the volatility for Euro over past two years is 11.23%


eur_his2 <- eur_his2 %>% complete(date = seq(as.Date("2016/1/1"), as.Date("2017/12/31"), by="day"))
for(i in 1:731){
  if(is.na(eur_his2$price[i])==TRUE){
    eur_his2$price[i] <- eur_his2$price[i-1]
  }   
}




#GBP
gbp_his2 <- read.csv(file="GBP_USD2.csv", header=TRUE)
gbp_his2<- gbp_his2[seq(dim(gbp_his2)[1],1),]
gbp_his2$Date <- as.Date(gbp_his2$Date, format="%d-%B-%y")
gbp_his2 <- gbp_his2 %>% select(Date, Price, Change)
colnames(gbp_his2) <- c("date", "price", "change")
gbp_his2$change <- as.character(gbp_his2$change)
gbp_his2$change <- as.numeric(substr(gbp_his2$change,0,nchar(gbp_his2$change)-1))


gbp_his2_sd <- sd(gbp_his2$change)
gbp_his2_sd
gbp_his2_vol <- round(gbp_his2_sd*sqrt(521), digits=2)
gbp_his2_vol
#the volatility for British Pounds over past two years is 16.48%


gbp_his2 <- gbp_his2 %>% complete(date = seq(as.Date("2016/1/1"), as.Date("2017/12/31"), by="day"))
for(i in 1:731){
  if(is.na(gbp_his2$price[i])==TRUE){
    gbp_his2$price[i] <- gbp_his2$price[i-1]
  }   
}





#CNY
cny_his2 <- read.csv(file="USD_CNY2.csv", header=TRUE)
cny_his2<- cny_his2[seq(dim(cny_his2)[1],1),]
cny_his2$Date <- as.Date(cny_his2$Date, format="%d-%B-%y")
cny_his2 <- cny_his2 %>% select(Date, Price, Change)
colnames(cny_his2) <- c("date", "price", "change")
cny_his2$price <- 1/(cny_his2$price)
cny_his2$change <- as.character(cny_his2$change)
cny_his2$change <- as.numeric(substr(cny_his2$change,0,nchar(cny_his2$change)-1))


cny_his2_sd <- sd(cny_his2$change)
cny_his2_sd
cny_his2_vol <- round(cny_his2_sd*sqrt(521), digits=2)
cny_his2_vol
#the volatility for Chinese Yuan over past two years is 4.69%



cny_his2 <- cny_his2 %>% complete(date = seq(as.Date("2016/1/1"), as.Date("2017/12/31"), by="day"))
for(i in 1:731){
  if(is.na(cny_his2$price[i])==TRUE){
    cny_his2$price[i] <- cny_his2$price[i-1]
  }   
}
#It's obvious that the volatility of Bitcoin is much higher than all three flat currencies, which means that it is way more risky to invest in Cryptocurrency market than foriegn exchange market, facing large probability of gain and loss at the same time.



###combine four
ccc <- cbind(btc2[2], eur_his2[2], gbp_his2[2], cny_his2[2])
colnames(ccc) <- c("BTC", "EUR", "GBP", "CNY")


#scatter plots
par(mfrow=c(2,2))
par(mar=c(4, 4, 4, 4))
scatter.smooth(x=ccc$BTC, y=ccc$EUR, xlab="BTC", ylab="EUR")
scatter.smooth(x=ccc$BTC, y=ccc$GBP, xlab="BTC", ylab="GBP")
scatter.smooth(x=ccc$BTC, y=ccc$CNY, xlab="BTC", ylab="CNY")
dev.off()
#can't see any linear relationship between bitcoin and forex currencies


###Spearman Correlation
correlation <- round(cor(ccc, method="spearman"), 2)
#round(correlation, 2)
#correlation between bitcoin and any of three currencies are smaller than 0.4, which imply weak realtionship between bitcoin rate and flat currency rate.



#time series
inds <- seq(as.Date("2016-01-01"), as.Date("2017-12-31"), by = "day")
btc.ts <- ts(btc2$priceUSD,     
           start = c(2016, as.numeric(format(inds[1], "%j"))),
           frequency = 365)

eur.ts <- ts(eur_his2$price,     
             start = c(2016, as.numeric(format(inds[1], "%j"))),
             frequency = 365)

gbp.ts <- ts(gbp_his2$price,     
             start = c(2016, as.numeric(format(inds[1], "%j"))),
             frequency = 365)

cny.ts <- ts(cny_his2$price,     
             start = c(2016, as.numeric(format(inds[1], "%j"))),
             frequency = 365)

#time series plot for bitcoin and flat currencies

plot(btc.ts)
ts.plot(eur.ts, gbp.ts, cny.ts, gpars = list(col = c("red", "blue", "yellow")))
legend("right", legend = c("Euro", "British Pounds", "Chinese Yuan"), col = c("red", "blue", "yellow"), lty = 1, cex=0.8)




#extra plots
btc_log_returns <- diff(log(btc2$priceUSD), lag=1)
eur_log_returns <- diff(log(eur_his2$price), lag=1)
gbp_log_returns <- diff(log(gbp_his2$price), lag=1)
cny_log_returns <- diff(log(cny_his2$price), lag=1)

summary_df <- cbind(as.matrix(summary(btc_log_returns)), as.matrix(summary(eur_log_returns)), as.matrix(summary(gbp_log_returns)), as.matrix(summary(cny_log_returns)))
colnames(summary_df) <- c("BTC", "EUR", "GBP", "CNY")
summary_df

three_flat <- cbind(eur_his2[2], gbp_his2[2], cny_his2[2])
colnames(three_flat) <- c("Euro", "British Pounds", "Chinese Yuan")

hist(btc2$priceUSD, main="Bitcoin", xlab="price")

par(mfrow=c(2,2))
par(mar=c(4, 4, 4, 4))
plot(btc_log_returns, type="l", main="BTC")
plot(eur_log_returns, type="l", main="EUR")
plot(gbp_log_returns, type="l", main="GBP")
plot(cny_log_returns, type="l", main="CNY")
dev.off()


