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
library(imputeTS)
library(tsibble)
library(TTR)

getSymbols(auto.assign = T)

getSymbols('TSLA', from = "2000-01-01", to = "2016-06-30", auto.assign = T)

Quandl(c('QQQ','SPY'))

getSymbolLookup()

symbols <- stockSymbols("NASDAQ")[,c(1:2)]
symbols <- na.omit(symbols)
symbols <- symbols$Symbol[1:10]


#todo: whole nasdaq there choose what to pick?
for (i in 1:length(symbols$Symbol)){
  try(getSymbols(symbols$Symbol[i], from = "2017-01-01", auto.assign = T))
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


train <- window(AEY$AEY.Adjusted, start=min(index(AEY)) , end = (max(index(AEY) - 36)))

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


h_vec <- seq(10, 360, 10) 
h_vec
df_fc_acc <- data.frame("h_vec" = h_vec, "meanf" = NA, "ses"=NA)




dates <- seq.Date(from=min(index(ALTM)), to=max(index(ALTM)), by="days")

xts <- xts(x=rep(NA, length(dates)), order.by=dates)
xts_imp <- na.locf(merge(xts, ALTM, join = "left"))

train <- window(xts_imp$ALTM.Adjusted, start=min(index(xts_imp)), end = (max(index(xts_imp)) - 360))
test <- window(xts_imp$ALTM.Adjusted, start=max(index(xts_imp)) - 359)

for(i in h_vec){
  
  
  
  
  
  
  
  
  a <- meanf(train, h = i)$mean
  b <- ses(train, h = i)$mean
  c <- holt(train, h = i)$mean
  d <- ets(train) %>% forecast(h = i)
  e <- tbats(as.numeric(coredata(train))) %>% forecast(h = i)
  f <- auto.arima(train, D=1) %>% forecast(h = 365)
  g <- nnetar(train, lambda=0) %>% forecast(h = 365)
  h <- rwf(train,h=i)$mean
  df_fc_acc$meanf[df_fc_acc$h_vec == i] <- sqrt((test[i]-max(a))^2)
  df_fc_acc$ses[df_fc_acc$h_vec == i] <- sqrt((test[i]-max(b))^2)
  df_fc_acc$holt[df_fc_acc$h_vec == i] <- sqrt((test[i]-max(c))^2)
  df_fc_acc$ets[df_fc_acc$h_vec == i] <- sqrt((test[i]-max(d$mean))^2)
  df_fc_acc$tbats[df_fc_acc$h_vec == i] <- sqrt((test[i]-max(e$mean))^2)
  df_fc_acc$auto.arima[df_fc_acc$h_vec == i] <- sqrt((test[i]-max(f$mean))^2)
  df_fc_acc$nnetar[df_fc_acc$h_vec == i] <- sqrt((test[i]-max(g$mean))^2)
  df_fc_acc$nnetar[df_fc_acc$h_vec == i] <- sqrt((test[i]-max(h))^2)
}



dates <- seq.Date(from=min(index(test)), to=max(index(test)), by="days")
a_xts <- xts(x=a, order.by=dates)
b_xts <- xts(x=b, order.by=dates)
c_xts <- xts(x=c, order.by=dates)
d_xts <- xts(x=d$mean, order.by=dates)
e_xts <- xts(x=e$mean, order.by=dates)
f_xts <- xts(x=f$mean, order.by=dates)
g_xts <- xts(x=g$mean, order.by=dates)
h_xts <- xts(x=h, order.by=dates)

trace_0 <- data.frame(date=index(train), coredata(train))
trace_1 <- data.frame(date=index(test), coredata(test))
trace_2 <- data.frame(date=index(a_xts), coredata(a_xts))
trace_3 <- data.frame(date=index(b_xts), coredata(b_xts))
trace_4 <- data.frame(date=index(c_xts), coredata(c_xts))
trace_5 <- data.frame(date=index(d_xts), coredata(d_xts))
trace_6 <- data.frame(date=index(e_xts), coredata(e_xts))
trace_7 <- data.frame(date=index(f_xts), coredata(f_xts))
trace_8 <- data.frame(date=index(g_xts), coredata(g_xts))
trace_9 <- data.frame(date=index(h_xts), coredata(h_xts))

x <- seq.Date(from=min(index(train)), to=max(index(test)) + 360, by="days")

data_fc_merged <- data.frame("date"=x)
data_fc_merged <- merge(data, trace_0, by="date", all.x = TRUE)
data_fc_merged <- merge(data, trace_1, by="date", all.x = TRUE)
data <- merge(data, trace_2, by="date", all.x = TRUE)
data <- merge(data, trace_3, by="date", all.x = TRUE)
data <- merge(data, trace_4, by="date", all.x = TRUE)
data <- merge(data, trace_5, by="date", all.x = TRUE)
data <- merge(data, trace_6, by="date", all.x = TRUE)
data <- merge(data, trace_7, by="date", all.x = TRUE)
data <- merge(data, trace_8, by="date", all.x = TRUE)
data <- merge(data, trace_9, by="date", all.x = TRUE)

names(data) <- c("date", "train", "test", "meanf", "ses", "holt","ets","tbats", "autoarima", "nnetar", "rwf")

fig <- plot_ly(data, x=~date, y = ~train, name = 'train', type = 'scatter', mode = 'lines') 
fig <- fig %>% add_trace(y = ~test, name = 'test', mode = 'lines') 
fig <- fig %>% add_trace(y = ~meanf, name = 'meanf', mode = 'lines')
fig <- fig %>% add_trace(y = ~ses, name = 'ses', mode = 'lines')
fig <- fig %>% add_trace(y = ~holt, name = 'holt', mode = 'lines')
fig <- fig %>% add_trace(y = ~ets, name = 'ets', mode = 'lines')
fig <- fig %>% add_trace(y = ~tbats, name = 'tbats', mode = 'lines')
fig <- fig %>% add_trace(y = ~autoarima, name = 'autoarima', mode = 'lines')
fig <- fig %>% add_trace(y = ~nnetar, name = 'nnetar', mode = 'lines')
fig <- fig %>% add_trace(y = ~rwf, name = 'rwf', mode = 'lines')
fig



e <- tsCV(train, ses, h=36)

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
    html_text() 
    
}



rsi <- RSI(AACG$AACG.Adjusted)
macd <- MACD(AACG$AACG.Adjusted)

