library(ggplot2)
library(plotly)
library(xts)
library(zoo)
library(quantmod)
library(Quandl)
library(forecast)

getSymbols(a, from = "2000-01-01", to = "2016-06-30", auto.assign = T)

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




meanf(data$AEY.Adjusted,h=6)
naive(data$AEY.Adjusted,h=6)
snaive(data$AEY.Adjusted,h=6)
rwf(data$AEY.Adjusted,h=6)
croston(data$AEY.Adjusted,h=6)
stlf(data$AEY.Adjusted,h=6)
ses(data$AEY.Adjusted,h=6)
holt(data$AEY.Adjusted,h=6) 
hw(data$AEY.Adjusted,h=6)
splinef(data$AEY.Adjusted,h=6)
thetaf(data$AEY.Adjusted,h=6) 
auto.arima(data$AEY.Adjusted) %>% forecast(h = 6)


plot(auto.arima(data$AEY.Adjusted), h=6)


data <- data.frame(x, trace_0, trace_1, trace_2)

fig <- plot_ly(data, x = ~x)
fig <- fig %>% add_trace(y = ~trace_0, name = 'trace 0',mode = 'lines')
fig <- fig %>% add_trace(y = ~trace_1, name = 'trace 1', mode = 'lines+markers')
fig <- fig %>% add_trace(y = ~trace_2, name = 'trace 2', mode = 'markers')

fig

