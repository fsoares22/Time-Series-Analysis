#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(ggplot2)
library(dplyr)
library(prophet)

# Import Data
apple = read.csv('./data/AAPL.csv')
amgen = read.csv('./data/AMGN.csv')
comcast = read.csv('./data/CMCSA.csv')
gilead = read.csv('./data/GILD.csv')
microsoft = read.csv('./data/MSFT.csv')
netflix = read.csv('./data/NFLX.csv')

# Convert to Date

apple$Date <- as.Date(apple$Date, format= "%Y-%m-%d")
amgen$Date <- as.Date(amgen$Date, format= "%Y-%m-%d")
comcast$Date <- as.Date(comcast$Date, format= "%Y-%m-%d")
gilead$Date <- as.Date(gilead$Date, format= "%Y-%m-%d")
microsoft$Date <- as.Date(microsoft$Date, format= "%Y-%m-%d")
netflix$Date <- as.Date(netflix$Date, format= "%Y-%m-%d")


# Drop all dates before January 1 2015

app_drop <- subset(apple, Date>= "2015-01-01")
amg_drop <- subset(amgen, Date>= "2015-01-01")
com_drop <- subset(comcast, Date>= "2015-01-01")
gil_drop <- subset(gilead, Date>= "2015-01-01")
mic_drop <- subset(microsoft, Date>= "2015-01-01")
net_drop <- subset(netflix, Date>= "2015-01-01")

# Plot Current Closing Price
close1 <- ggplot(data=app_drop, aes(x=Date, y=Close)) +
    geom_line(color="blue") + ggtitle("Apple Closing Price")

close2 <- ggplot(data=amg_drop, aes(x=Date, y=Close)) +
    geom_line(color="blue") + ggtitle("Amgen Closing Price")

close3 <- ggplot(data=com_drop, aes(x=Date, y=Close)) +
    geom_line(color="blue") + ggtitle("Comcast Closing Price")

close4 <- ggplot(data=gil_drop, aes(x=Date, y=Close)) +
    geom_line(color="blue") + ggtitle("Gilead Closing Price")

close5 <- ggplot(data=mic_drop, aes(x=Date, y=Close)) +
    geom_line(color="blue") + ggtitle("Microsoft Closing Price")

close6 <- ggplot(data=net_drop, aes(x=Date, y=Close)) +
    geom_line(color="blue") + ggtitle("Netflix Closing Price")

# Drop Columns for Prophet package

app_drop = subset(app_drop, select= -c(Open, High, Low, Adj.Close, Volume))
amg_drop = subset(amg_drop, select= -c(Open, High, Low, Adj.Close, Volume))
com_drop = subset(com_drop, select= -c(Open, High, Low, Adj.Close, Volume))
gil_drop = subset(gil_drop, select= -c(Open, High, Low, Adj.Close, Volume))
mic_drop = subset(mic_drop, select= -c(Open, High, Low, Adj.Close, Volume))
net_drop = subset(net_drop, select= -c(Open, High, Low, Adj.Close, Volume))

# Change Column Names for Prophet to run

names(app_drop)[names(app_drop) == "Date"] <- "ds"
names(app_drop)[names(app_drop) == "Close"] <- "y"

names(amg_drop)[names(amg_drop) == "Date"] <- "ds"
names(amg_drop)[names(amg_drop) == "Close"] <- "y"

names(com_drop)[names(com_drop) == "Date"] <- "ds"
names(com_drop)[names(com_drop) == "Close"] <- "y"

names(gil_drop)[names(gil_drop) == "Date"] <- "ds"
names(gil_drop)[names(gil_drop) == "Close"] <- "y"

names(mic_drop)[names(mic_drop) == "Date"] <- "ds"
names(mic_drop)[names(mic_drop) == "Close"] <- "y"

names(net_drop)[names(net_drop) == "Date"] <- "ds"
names(net_drop)[names(net_drop) == "Close"] <- "y"

# Run Prophet to forecast data
# Apple
m1 <- prophet(app_drop)

future1 <- make_future_dataframe(m1, periods = 365)
tail(future1)

forecast1 <- predict(m1, future1)
tail(forecast1[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

forecast_plot1 <- plot(m1, forecast1, xlabel = 'Date', 
                       ylabel = "Apple Forecasted Closing Price")
forecast_plot1

# Amgen
m2 <- prophet(amg_drop)

future2 <- make_future_dataframe(m2, periods = 365)
tail(future2)

forecast2 <- predict(m2, future2)
tail(forecast2[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

forecast_plot2 <- plot(m2, forecast2, xlabel = 'Date', 
                       ylabel = "Amgen Forecasted Closing Price")
forecast_plot2

# Comcast
m3 <- prophet(com_drop)

future3 <- make_future_dataframe(m3, periods = 365)
tail(future3)

forecast3 <- predict(m3, future3)
tail(forecast3[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

forecast_plot3 <- plot(m3, forecast3, xlabel = 'Date', 
                       ylabel = "Comcast Forecasted Closing Price")
forecast_plot3

# Gilead
m4 <- prophet(gil_drop)

future4 <- make_future_dataframe(m4, periods = 365)
tail(future4)

forecast4 <- predict(m4, future4)
tail(forecast4[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

forecast_plot4 <- plot(m4, forecast4, xlabel = 'Date', 
                       ylabel = "Gilead Forecasted Closing Price")
forecast_plot4

# Microsoft
m5 <- prophet(mic_drop)

future5 <- make_future_dataframe(m5, periods = 365)
tail(future5)

forecast5 <- predict(m5, future5)
tail(forecast5[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

forecast_plot5 <- plot(m5, forecast5, xlabel = 'Date', 
                       ylabel = "Microsoft Forecasted Closing Price")
forecast_plot5

# Netflix
m6 <- prophet(net_drop)

future6 <- make_future_dataframe(m6, periods = 365)
tail(future6)

forecast6 <- predict(m6, future6)
tail(forecast6[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

forecast_plot6 <- plot(m6, forecast6, xlabel = 'Date', 
                       ylabel = "Netflix Forecasted Closing Price")
forecast_plot6


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    output$close_plot <- renderPlot({
        if(input$Closing == 'Apple'){close1}
        else if(input$Closing == 'Amgen'){close2}
        else if(input$Closing == 'Comcast'){close3}
        else if(input$Closing == 'Gilead'){close4}
        else if(input$Closing == 'Microsoft'){close5}
        else if(input$Closing == 'Netflix'){close6}
        
    })

    output$forecast_plot <- renderPlot({
        if(input$Companies == 'Apple'){forecast_plot1}
        else if(input$Companies == 'Amgen'){forecast_plot2}
        else if(input$Companies == 'Comcast'){forecast_plot3}
        else if(input$Companies == 'Gilead'){forecast_plot4}
        else if(input$Companies == 'Microsoft'){forecast_plot5}
        else if(input$Companies == 'Netflix'){forecast_plot6}

    })

})
