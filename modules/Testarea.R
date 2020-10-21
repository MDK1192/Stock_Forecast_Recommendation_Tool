library(ggplot2)
library(plotly)
library(xts)
library(zoo)
library(quantmod)
library(Quandl)
library(forecast)
library(RCrawler)
library(rvest)
library(data.table)
getSymbols('TSLA', auto.assign = T)

getSymbols('TSLA', from = "2000-01-01", to = "2016-06-30", auto.assign = T)

Quandl(c('QQQ','SPY'))

getSymbolLookup()

symbols <- stockSymbols("NASDAQ")[,c(1:2)]
symbols <- na.omit(symbols)


#todo: whole nasdaq there choose what to pick?
for (i in 1:length(symbols$Symbol)){
  try(getSymbols(symbols$Symbol[i], from = "2019-01-01", auto.assign = T))
}


today <- Sys.Date()
tm <- seq(0, 600, by = 10)




tm <- seq(0, 600, by = 10)
x <- today - tm

timerange <- max(index(AEY)) - seq(0, 600, by = 10)

data_temp <- AEY
ggplot(data_temp, aes(x=index(data_temp), y=data$AEY.Adjusted)) +
  geom_line() + 
  xlab("")

data <- data.frame("Date"=index(AEY), "Values" = AEY$AEY.Adjusted)
plot_ly(data, x = ~Date, y = ~AEY.Adjusted, type = 'scatter', mode = 'lines', 
        line = list(color = "rgb(0, 0, 0)")) %>%
        layout(title = "Date",
        xaxis = list(title = "Date",
                      zeroline = FALSE),
        yaxis = list(title = "Price",
                      zeroline = FALSE))


https://plotly.com/r/line-and-scatter/
  https://otexts.com/fpp2/the-forecast-package-in-r.html


train <- window(AEY$AEY.Adjusted, start=min(index(AEY)) , end = (max(index(AEY) - 12)))

n=36

meanf(train,h=n)$mean
naive(train,h=n)$mean
snaive(train,h=n)$mean
rwf(train,h=n)$mean
croston(train,h=n)$mean
stlf(train,h=n)$mean
ses(train,h=n)$mean
holt(train,h=n)$mean
hw(train,h=n)$mean
splinef(train,h=n)$mean
thetaf(train,h=n)$mean
a <- ets(train) %>% forecast(h = n)
b <- tbats(train) %>% forecast(h = n)
c <- auto.arima(train) %>% forecast(h = n)



accuracy(meanf(train,h=n)$mean, AEY$AEY.Adjusted[(length(AEY$AEY.Adjusted) - (n-1)) :length(AEY$AEY.Adjusted)])
accuracy(naive(train,h=n)$mean, AEY$AEY.Adjusted[(length(AEY$AEY.Adjusted) - (n-1)) :length(AEY$AEY.Adjusted)])
accuracy(snaive(train,h=n)$mean, AEY$AEY.Adjusted[(length(AEY$AEY.Adjusted) - (n-1)) :length(AEY$AEY.Adjusted)])
accuracy(rwf(train,h=n)$mean, AEY$AEY.Adjusted[(length(AEY$AEY.Adjusted) - (n-1)) :length(AEY$AEY.Adjusted)])
accuracy(croston(train,h=n)$mean, AEY$AEY.Adjusted[(length(AEY$AEY.Adjusted) - (n-1)) :length(AEY$AEY.Adjusted)])
accuracy(stlf(train,h=n)$mean, AEY$AEY.Adjusted[(length(AEY$AEY.Adjusted) - (n-1)) :length(AEY$AEY.Adjusted)])
accuracy(ses(train,h=n)$mean, AEY$AEY.Adjusted[(length(AEY$AEY.Adjusted) - (n-1)) :length(AEY$AEY.Adjusted)])
accuracy(holt(train,h=n)$mean, AEY$AEY.Adjusted[(length(AEY$AEY.Adjusted) - (n-1)) :length(AEY$AEY.Adjusted)])
accuracy(hw(train,h=n)$mean, AEY$AEY.Adjusted[(length(AEY$AEY.Adjusted) - (n-1)) :length(AEY$AEY.Adjusted)])
accuracy(splinef(train,h=n)$mean, AEY$AEY.Adjusted[(length(AEY$AEY.Adjusted) - (n-1)) :length(AEY$AEY.Adjusted)])
accuracy(thetaf(train,h=n)$mean, AEY$AEY.Adjusted[(length(AEY$AEY.Adjusted) - (n-1)) :length(AEY$AEY.Adjusted)])
accuracy(a$mean, AEY$AEY.Adjusted[(length(AEY$AEY.Adjusted) - (n-1)) :length(AEY$AEY.Adjusted)])
accuracy(b$mean, AEY$AEY.Adjusted[(length(AEY$AEY.Adjusted) - (n-1)) :length(AEY$AEY.Adjusted)])
accuracy(c$mean, AEY$AEY.Adjusted[(length(AEY$AEY.Adjusted) - (n-1)) :length(AEY$AEY.Adjusted)])

https://rdrr.io/cran/forecast/man/tsCV.html
https://robjhyndman.com/hyndsight/tscv/
e <- tsCV(train, ses, h=365)

sqrt(mean(e^2, na.rm=TRUE))

colMeans(e^2, na.rm=T)


plot(auto.arima(data$AEY.Adjusted), h=n)


data <- data.frame(x, trace_0, trace_1, trace_2)

fig <- plot_ly(data, x = ~x)
fig <- fig %>% add_trace(y = ~trace_0, name = 'trace 0',mode = 'lines')
fig <- fig %>% add_trace(y = ~trace_1, name = 'trace 1', mode = 'lines+markers')
fig <- fig %>% add_trace(y = ~trace_2, name = 'trace 2', mode = 'markers')

fig


test <- read_html("https://www.finanzen.net/news/bayer-news") %>%
  html_nodes("p") %>%
  html_text()

urls <- read_html("https://www.finanzen.net/news/bayer-news") %>%
  html_nodes("td a") %>%
  html_attr("href")
  
test_df <- data.frame(url = urls)
test_df <- data.frame(url = urls[urls %like% "/nachricht/aktien/"])

test_df$text <- "Placeholder"

for(i in 1:nrow(test_df)){
  test_df$text[i] <- read_html("https://www.finanzen.net/nachricht/aktien/bayer-truemmer-unter-dem-bayer-kreuz-9400279") %>%
    html_node("#news-container") %>%
    html_text() %>%
    
}

