library(shiny)
library(shinydashboard)
library(tidyverse)
library(httr)
library(jsonlite)
library(magrittr)
library(ggplot2)
library(tseries)
library(timeSeries)
library(dplyr)
library(ggthemes)


btc.data <- read.csv(file='bitcoin.csv', header=TRUE)
eth.data <- read.csv(file='eth.csv', header=TRUE)
btc2 <- read.csv(file='BTC_USD2.csv', header=TRUE)
eur_his2 <- read.csv(file="EUR_USD2.csv", header=TRUE)
gbp_his2 <- read.csv(file="GBP_USD2.csv", header=TRUE)
cny_his2 <- read.csv(file="USD_CNY2.csv", header=TRUE)

btc.data <- btc.data %>% select(currency, everything())
myvar <- c("currency", "rate")
btc.data <- btc.data[myvar]
btc.data["crypto"] <- "BTC"
btc.data$rate <- as.numeric(gsub(",", "", btc.data$rate))

eth.data <- eth.data %>% select(currency, everything())
eth.data$currency <- as.character(eth.data$currency)
eth.data$currency[eth.data$currency=="ETHEUR"] <- "EUR"
eth.data$currency[eth.data$currency=="ETHGBP"] <- "GBP"
eth.data$currency[eth.data$currency=="ETHUSD"] <- "USD"
eth.data$currency <- as.factor(eth.data$currency)
eth.data["crypto"] <- "ETH"

btc_and_eth <- rbind(btc.data, eth.data)
###
btc2 <- btc2 %>% select(date, everything())

eur_his2<- eur_his2[seq(dim(eur_his2)[1],1),]
eur_his2$Date <- as.Date(eur_his2$Date, format="%d-%B-%y")
eur_his2 <- eur_his2 %>% select(Date, Price, Change)
colnames(eur_his2) <- c("date", "price", "change")
eur_his2$change <- as.character(eur_his2$change)
eur_his2$change <- as.numeric(substr(eur_his2$change,0,nchar(eur_his2$change)-1))
eur_his2 <- eur_his2 %>% complete(date = seq(as.Date("2016/1/1"), as.Date("2017/12/31"), by="day"))
for(i in 1:731){
  if(is.na(eur_his2$price[i])==TRUE){
    eur_his2$price[i] <- eur_his2$price[i-1]
  }   
}


gbp_his2<- gbp_his2[seq(dim(gbp_his2)[1],1),]
gbp_his2$Date <- as.Date(gbp_his2$Date, format="%d-%B-%y")
gbp_his2 <- gbp_his2 %>% select(Date, Price, Change)
colnames(gbp_his2) <- c("date", "price", "change")
gbp_his2$change <- as.character(gbp_his2$change)
gbp_his2$change <- as.numeric(substr(gbp_his2$change,0,nchar(gbp_his2$change)-1))
gbp_his2 <- gbp_his2 %>% complete(date = seq(as.Date("2016/1/1"), as.Date("2017/12/31"), by="day"))
for(i in 1:731){
  if(is.na(gbp_his2$price[i])==TRUE){
    gbp_his2$price[i] <- gbp_his2$price[i-1]
  }   
}


cny_his2<- cny_his2[seq(dim(cny_his2)[1],1),]
cny_his2$Date <- as.Date(cny_his2$Date, format="%d-%B-%y")
cny_his2 <- cny_his2 %>% select(Date, Price, Change)
colnames(cny_his2) <- c("date", "price", "change")
cny_his2$price <- 1/(cny_his2$price)
cny_his2$change <- as.character(cny_his2$change)
cny_his2$change <- as.numeric(substr(cny_his2$change,0,nchar(cny_his2$change)-1))
cny_his2 <- cny_his2 %>% complete(date = seq(as.Date("2016/1/1"), as.Date("2017/12/31"), by="day"))
for(i in 1:731){
  if(is.na(cny_his2$price[i])==TRUE){
    cny_his2$price[i] <- cny_his2$price[i-1]
  }   
}

three_flat <- cbind(eur_his2[2], gbp_his2[2], cny_his2[2])
colnames(three_flat) <- c("Euro", "British Pounds", "Chinese Yuan")

inds <- seq(as.Date("2016-01-01"), as.Date("2017-12-31"), by = "day")

ccc <- cbind(btc2[2], eur_his2[2], gbp_his2[2], cny_his2[2])
colnames(ccc) <- c("Bitcoin", "Euro", "British Pounds", "Chinese Yuan")



ui <- dashboardPage(skin="purple",
  dashboardHeader(title = "Cryptocurrency"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "intro", icon = icon("file-text-o")),
      menuItem("Real Time Price", tabName = "realtime", icon = icon("bar-chart-o")),
      menuItem("Histogram", tabName = "histogram", icon = icon("bar-chart-o")),
      menuItem("Time Series Plot", tabName = "timeseries", icon = icon("line-chart")),
      menuItem("Scatter Plot", tabName = "scatter", icon = icon("line-chart"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "intro", box(background="purple", solidHeader = TRUE, width=12,
        h3("As Bitcoin's price increases dramatically in the last year, more people all around the world get to know the concept of cryptocurrency recently. Lots of people gain a huge amount of money by investing in cryotocurrency market, while others suffer great losses. Many investors who are currently investing in foreign exchange market move a part of their money to the cryptocurrency market, hoping for higher returns.")
      )
      ),
      tabItem(tabName = "realtime", 
              fluidRow(
                  tabBox(title = "Current Price",
                         tabPanel("Bitcoin", "Bitcoin Price", plotOutput("plot_btc_real")),
                         tabPanel("Etherium", "Etherium Price", plotOutput("plot_etc_real")),
                         tabPanel("Together", "BTC&ETH", plotOutput("plot_both"))
                  ),
                  box(collapsible=TRUE, h4("The value per Bitcoin is around $9000, making it the most valuable cryptocurrency. Being the second valuable cryptocurrency, Etherium is worth approimately 7% of the value of Bitcoin."))
              )
      ),
      tabItem(tabName = "timeseries", 
              h3("Flat currency and Bitcoin price (in USD) in last 2 years"),
              fluidRow(
                  box(title="Flat currency price", background="purple", plotOutput("plot_flat_ts"), height="400px"),
                  box(title="Bitcoin price", plotOutput("plot_ts"), height="400px")
              ),
              fluidRow(
                  box(selectInput("flat_currency", "Choose a flat currency:", 
                      choices=c("Euro", "British Pounds", "Chinese Yuan")), background="purple"),
                  box(collapsible=TRUE, h4("The price in cryptocurrency market fluctuates largely during the two-year period, while the price in foreign exchange market is way more stable and only vary by a small amount."))
              )
      ),
      tabItem(tabName = "histogram", 
              h4("Histogram for Bitcoin's prices (in USD) in last 2 years"),
              sliderInput("bins",
                          "Number of bins:",
                          value = 8,
                          min = 2,
                          max = 16),
              box(plotOutput("plot_btc_hist"))
      ),
      tabItem(tabName = "scatter", 
              h3("Scatter plots between Bitcoin and flat currencies"),
              radioButtons("flat", "Bitcoin against which flat currency: ",
                           c("Euro"="Euro", "British Pounds"="British Pounds", "Chinese Yuan"="Chinese Yuan")),
              fluidRow(
              box(plotOutput("plot_scatter"), height="400px"),
              box(collapsible=TRUE, h4("From the plots, it's hard to see any linear relationship between Bitcoin and any of the forex currencies."))
              )
      )
    )
  )
)

server <- function(input, output) { 
  output$plot_btc_real <- renderPlot({
    ggplot(btc.data, aes(x=currency, y=rate)) +
      geom_bar(stat="identity", fill="lightblue") +
      geom_text(aes(label=rate), vjust=-0.3, size=3.5) + 
      ggtitle("Bitcoin Price Index")
  })
  
  output$plot_etc_real <- renderPlot({
    ggplot(eth.data, aes(x=currency, y=rate)) +
      geom_bar(stat="identity", fill="pink") +
      geom_text(aes(label=rate), vjust=-0.3, size=3.5) + 
      ggtitle("Etherium Price Index")
  })
  
  output$plot_both <- renderPlot({
    ggplot(btc_and_eth, aes(x=currency, y=rate, fill=crypto)) +
      geom_bar(stat="identity", position=position_dodge()) +
      geom_text(aes(label=rate), vjust=-0.3, size=3.5) + 
      ggtitle("Bitcoin vs. Etherium") + 
      theme(plot.title = element_text(size = 20, face = "bold")) 
  })
  
  output$plot_ts <- renderPlot({
    btc.ts <- ts(btc2$priceUSD,     
                 start = c(2016, as.numeric(format(inds[1], "%j"))),
                 frequency = 365)
    plot(btc.ts, main="Bitcoin")
  }, height=300, width=300)
  
  output$plot_flat_ts <- renderPlot({
    flat.ts <- ts(three_flat[,input$flat_currency],    
                 start = c(2016, as.numeric(format(inds[1], "%j"))),
                 frequency = 365)
    plot(flat.ts, main=input$flat_currency)
  }, height=300, width=300)
  
  output$plot_btc_hist <- renderPlot({
    hist(btc2$priceUSD, main=paste(input$bins, "bins"), xlab="price", breaks=input$bins, col="purple")
  })
  
  output$plot_scatter <- renderPlot({
    plot(ccc$Bitcoin, ccc[,input$flat], xlab="BTC", ylab=input$flat)
  }, height=350, width=300)
}

shinyApp(ui, server)


