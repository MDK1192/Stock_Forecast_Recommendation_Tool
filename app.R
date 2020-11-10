#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(xts)
library(zoo)
library(quantmod)
library(dplyr)
# library(Quandl)
library(forecast)
#library(RCrawler)
library(DT)
library(rvest)

#hallotest
#faulty package disables functions -> investigate!
# library(data.table)
# library(imputeTS)
# library(tsibble)
# library(TTR)

source("modules/import_data.R")
source("modules/module2.R")
source("modules/module3.R")


ui <- dashboardPage(
    #Header Content
    dashboardHeader(title = "Stock Recommendation Dashboard"),
    #Sidebar Content
    dashboardSidebar(
        sidebarMenu(
            menuItem("Aktien", tabName = "aktien", icon = icon("th")),
            menuItem("Forecast", tabName = "forecast", icon = icon("th")),
            menuItem("Blogs", tabName = "blogs", icon = icon("th")),
            menuItem("Trades", tabName = "trades", icon = icon("th")),
            menuItem("Kursindikator", tabName = "kursindikator", icon = icon("th")),
            menuItem("Recommendation", tabName = "recommendation", icon = icon("th"))
        )
    ),
    #Body Content
    dashboardBody(
        tabItems(
            tabItem(tabName = "aktien",
                    h2("Aktien Uebersicht"),
                    box(width = 12,
                        box(width = 3, dateRangeInput("dates", label = h3("Date range"))),
                        box(width = 6,radioButtons("branchen", label = h3("Branchen"),
                                     choices = list("Anlagenbau" = 1, "Autoteile" = 2, "Billig-Gemischtwarenladen" = 3,
                                                    "Biotechnologie" = 4, "Computer-Videospiele" = 5, "Computer-Einzelhandel" = 6,
                                                    "Computerhardware" = 7, "Computerspiele" = 8, "Datenanalyse" = 9,
                                                    "Drogerieprodukte" = 10, "E-Commerce" = 11, "Einzelhandel" = 12), 
                                     selected = 1)),
                        box(width = 3, actionButton("loadButton", label = "Load Stockdata", width = '100%'))
                    ),
                    box(width = 12,
                        box(width = 8,plotlyOutput("plotStock"),title = "Kursuebersicht grafisch"),
                        box(width = 4,DTOutput("stockOverview"),title = "Aktienuebersicht")
                    ),
                    box(width = 12, DTOutput("stockData"),title = "Kursuebersicht tabellarisch")
            ),
            tabItem(tabName = "forecast",
                    h2("Forecast"),
                    box(width = 12,
                        box(width = 8,
                            sliderInput("horizonslider", label = h3("Forecast Horizon"), min = 31, max = 365, value = 50),
                            sliderInput("trainslider", label = h3("Forecast Validation window"), min = 0, max = 30, value = 0),
                        ),
                        box(width = 4,DTOutput("stockOverviewFC"),title = "Aktienuebersicht")),
                    box(width = 12, plotlyOutput("plotforecast"),title = "Forecasts grafisch"),
                    box(width = 12,DTOutput("accuracyOverview"),title = "Fehlerausgabe")
            ),
            tabItem(tabName = "blogs",
                    h2("Recommendation"),
                    box(width = 12,
                        box(width = 8,
                            DTOutput("performanceTable"),
                            DTOutput("analystTable"),
                            title = "Performance  Empfehlungen"),
                        box(width = 4,DTOutput("stockOverviewBlog"),title = "Aktienuebersicht")),
                    box(width = 12,DTOutput("placeholder7"),title = "Boersenblog3 tabellarisch")
            ),
            tabItem(tabName = "trades",
                    h2("Trades"),
                    box(width = 12,DTOutput("placeholder8"),title = "Fehlerausgabe")
            ),
            tabItem(tabName = "kursindikator",
                    h2("Kursindikatoren"),
                    box(width = 12,DTOutput("stockOverviewInd"),title = "Aktienuebersicht"),
                    box(width = 12, plotlyOutput("plotIndCore", height = 250)),
                    box(width = 12, 
                        plotlyOutput("plotIndMACD", height = 200),
                        plotlyOutput("plotIndRSI", height = 200)),

            ),
            tabItem(tabName = "recommendation",
                    h2("Recommandation"),
                    box(width = 12,
                        box(width = 6,actionButton("recommendButton", label = "Recommend Stock", width = '100%')),
                        box(width = 6,actionButton("saveButton", label = "Save Recommendation", width = '100%'))
                    ),
                    box(width = 12,DTOutput("recommendationOverview"),title = "Recommendation Overview")
            )

        )
    )
)

#server/logic content
server <- function(input, output, session) {
  

  


  observeEvent(input$loadButton, {
    #adding symbold from different areas
    symbols_fields <<- list(
      Anlagenbau <- c("AMAT","KLAC","LRCX"),
      Autoteile <- c("ORLY"),
      Billig_Gemischtwarenladen <- c("DLTR"),
      Biotechnologie <- c("AMGN","BIIB","BMRN","GILD","ILMN","SGEN"),
      Computer_Videospiele <- c("ATVI","EA"),
      Computer_Einzelhandel <- c("CDW"),
      Computerhardware <- c("WDC"),
      Computerspiele <- c("TTWO"),
      Datenanalyse <- c("VRSK"),
      Drogerieprodukte <- c("WBA"),
      E_Commerce <- c("JD", "MELI"),
      Einzelhandel <- c("FAST", "ROST")
    )
    symbols_Nasdaq <- stockSymbols("NASDAQ")[,c(1:2)]
    symbols_Nasdaq <- na.omit(symbols_Nasdaq)
    symbols_choice <- symbols_fields[[as.numeric(input$branchen)]]
    symbols <- symbols_Nasdaq[symbols_Nasdaq$Symbol %in% symbols_choice,]
    
    if(input$dates[1] == input$dates[2]){
      for (i in 1:length(symbols$Symbol)){
        try(getSymbols(symbols$Symbol[i], from = "2017-12-31", to=as.character(Sys.Date()) ,auto.assign = T))}
    }
    else{
      for (i in 1:length(symbols$Symbol)){
        try(getSymbols(symbols$Symbol[i], from = as.character(input$dates[1]), to=as.character(input$dates[2]) ,auto.assign = T))}
    }
    data_stock <- get(objects()[objects() %in% symbols_choice[1]])
    stocks_picked <<- symbols_Nasdaq[symbols_Nasdaq$Symbol %in% objects(),]

    output$stockOverview <- renderDataTable(stocks_picked,selection=list(mode="single"), options= list(scrollY = TRUE,pageLength = 5))
    output$stockOverviewFC <- renderDataTable(stocks_picked,selection=list(mode="single"), options= list(scrollY = TRUE,pageLength = 5))
    output$stockOverviewInd <- renderDataTable(stocks_picked,selection=list(mode="single"), options= list(scrollY = TRUE,pageLength = 5))
    output$stockOverviewBlog <- renderDataTable(stocks_picked,selection=list(mode="single"), options= list(scrollY = TRUE,pageLength = 5))
    data_stock_overview <- as.data.frame(data_stock)
    output$stockData <- renderDataTable(data_stock_overview,options= list(scrollY = TRUE,pageLength = 5))
    output$plotStock <- renderPlotly({
      data_plot <- data.frame("Date"= index(data_stock), "Adjusted" = select(as.data.frame(data_stock),contains("Adjusted")))
      names(data_plot) <- c("Date", "Adjusted")
      plot_ly(data_plot, x = ~Date, y = ~Adjusted, type = 'scatter', mode = 'lines', 
              line = list(color = "rgb(0, 0, 0)")) %>%
        layout(xaxis = list(title = "Date",
                            zeroline = FALSE),
               yaxis = list(title = "Price",
                            zeroline = FALSE))
    })
    #export everything to global env.
    allglobal <- function() {
      lss <- ls(envir = parent.frame())
      for (i in lss) {
        assign(i, get(i, envir = parent.frame()), envir = .GlobalEnv)
      }
    }
    allglobal()
  })
  observeEvent(input$stockOverview_rows_selected, {
    if (stocks_picked$Symbol[input$stockOverview_rows_selected] %in% ls(envir = .GlobalEnv)) {
      data_stock <<- get(stocks_picked$Symbol[input$stockOverview_rows_selected], envir = .GlobalEnv)
      output$stockData <- renderDataTable(data_stock,options= list(scrollY = TRUE,pageLength = 5))
      output$plotStock <- renderPlotly({
        data_plot <- data.frame("Date"= index(data_stock), "Adjusted" = select(as.data.frame(data_stock),contains("Adjusted")))
        names(data_plot) <- c("Date", "Adjusted")
        plot_ly(data_plot, x = ~Date, y = ~Adjusted, type = 'scatter', mode = 'lines', 
                line = list(color = "rgb(0, 0, 0)")) %>%
          layout(xaxis = list(title = "Date",
                              zeroline = FALSE),
                 yaxis = list(title = "Price",
                              zeroline = FALSE))
      })
      } 
  })
  observeEvent(input$stockOverviewFC_rows_selected, {
    if (stocks_picked$Symbol[input$stockOverviewFC_rows_selected] %in% ls(envir = .GlobalEnv)) {
      data_stock <<- get(stocks_picked$Symbol[input$stockOverviewFC_rows_selected], envir = .GlobalEnv)
      output$plotforecast <- renderPlotly({
        data_fc  <- xts(x=data_stock)
        
        dates <- seq.Date(from=min(index(data_fc)), to=max(index(data_fc)) - input$trainslider, by="days")
        xts <- xts(x=rep(NA, length(dates)), order.by=dates)
        data_fc_train <- na.locf(merge(xts, data_fc, join = "left"))
        
        dates <- seq.Date(from=min(index(data_fc)), to=max(index(data_fc)), by="days")
        xts <- xts(x=rep(NA, length(dates)), order.by=dates)
        data_fc <- na.locf(merge(xts, data_fc, join = "left"))
        
        
        fc_meanf <- meanf(data_fc_train[,7],h=input$horizonslider)$mean
        fc_naive <- naive(data_fc_train[,7],h=input$horizonslider)$mean
        fc_snaive <- snaive(data_fc_train[,7],h=input$horizonslider)$mean
        fc_rwf <- rwf(data_fc_train[,7],h=input$horizonslider)$mean
        # #fc_croston <- croston(data_fc_train[,7],h=input$horizonslider)$mean
        # only for seas.
        # #fc_stlf <- stlf(data_fc_train[,7],h=input$horizonslider)$mean
        fc_ses <- ses(data_fc_train[,7],h=input$horizonslider)$mean
        fc_holt <- holt(data_fc_train[,7],h=input$horizonslider)$mean
        # #fc_hw <- hw(data_fc_train[,7],h=input$horizonslider)$mean
        fc_splinef <- splinef(data_fc_train[,7],h=input$horizonslider)$mean
        fc_thetaf <- thetaf(data_fc_train[,7],h=input$horizonslider)$mean
        fc_ets <- ets(data_fc_train[,7]) %>% forecast(h = input$horizonslider) 
        fc_ets <- fc_ets$mean
        fc_tbats <- tbats(as.numeric(data_fc_train[,7])) %>% forecast(h = input$horizonslider)
        fc_tbats <- fc_tbats$mean
        fc_arima <- auto.arima(data_fc_train[,7]) %>% forecast(h = input$horizonslider)
        fc_arima <- fc_arima$mean
        fc_nnetar <- nnetar(data_fc_train[,7], lambda=0) %>% forecast(h = input$horizonslider)
        fc_nnetar <- fc_nnetar$mean
        
        if(input$trainslider != 0){
          train_ts <- anti_join(as.data.frame(data_fc), as.data.frame(data_fc_train))
          acc_fc_meanf <- accuracy(fc_meanf[1:length(train_ts[,7])], train_ts[,7])
          acc_fc_naive <- accuracy(fc_naive[1:length(train_ts[,7])], train_ts[,7])
          acc_fc_snaive <- accuracy(fc_snaive[1:length(train_ts[,7])], train_ts[,7])
          acc_fc_rwf <- accuracy(fc_rwf[1:length(train_ts[,7])], train_ts[,7])
          acc_fc_ses <- accuracy(fc_ses[1:length(train_ts[,7])], train_ts[,7])
          acc_fc_holt <- accuracy(fc_holt[1:length(train_ts[,7])], train_ts[,7])
          acc_fc_splinef <- accuracy(fc_splinef[1:length(train_ts[,7])], train_ts[,7])
          acc_fc_thetaf <- accuracy(fc_thetaf[1:length(train_ts[,7])], train_ts[,7])
          acc_fc_ets <- accuracy(fc_ets[1:length(train_ts[,7])], train_ts[,7])
          acc_fc_tbats <- accuracy(fc_tbats[1:length(train_ts[,7])], train_ts[,7])
          acc_fc_arima <- accuracy(fc_arima[1:length(train_ts[,7])], train_ts[,7])
          acc_fc_nnetar <- accuracy(fc_nnetar[1:length(train_ts[,7])], train_ts[,7])
          acc_df <- as.data.frame(rbind(acc_fc_meanf, acc_fc_naive, acc_fc_snaive, acc_fc_rwf, acc_fc_ses, 
                          acc_fc_holt, acc_fc_splinef, acc_fc_thetaf, acc_fc_ets, acc_fc_tbats,
                          acc_fc_arima, acc_fc_nnetar))
          row.names(acc_df) <- c("meanf", "naive", "snaive", "rwf","ses", "holt","splinef","thetaf", "ets", "tbats", "arima", "nnetar")
          output$accuracyOverview <- renderDataTable(acc_df, options= list(scrollY = TRUE,pageLength = 5))
         #Build FC_Ensemble
          top_scores_RMSE <- data.frame(acc_df[order(acc_df$RMSE),])
          models <- row.names(top_scores_RMSE[1:3,])
          multiplier <- c(0.5, 0.3, 0.2)
          ensemble_fc <- rep(0,1,length(fc_meanf))
          for(i in 1: length(multiplier)){
            model_fc <- as.numeric(get(paste0("fc_",models[i])))
            model_fc_weighted <- model_fc * multiplier[i]
            ensemble_fc <- ensemble_fc + model_fc_weighted
          }
        }

        dates <- seq.Date(from=max(index(data_fc_train)) + 1, to=max(index(data_fc_train)) + input$horizonslider, by="days")
        fc_meanf_xts <- xts(x=fc_meanf, order.by=dates)
        fc_naive_xts <- xts(x=fc_naive, order.by=dates)
        fc_snaive_xts <- xts(x=fc_snaive, order.by=dates)
        fc_rwf_xts <- xts(x=fc_rwf, order.by=dates)
        #fc_croston_xts <- xts(x=fc_croston, order.by=dates)
        #fc_stlf_xts <- xts(x=fc_stlf, order.by=dates)
        fc_ses_xts <- xts(x=fc_ses, order.by=dates)
        fc_holt_xts <- xts(x=fc_holt, order.by=dates)
        # #fc_hw_xts <- xts(x=fc_hw, order.by=dates)
        fc_splinef_xts <- xts(x=fc_splinef, order.by=dates)
        fc_thetaf_xts <- xts(x=fc_thetaf, order.by=dates)
        fc_ets_xts <- xts(x=fc_ets, order.by=dates)
        fc_tbats_xts <- xts(x=fc_tbats, order.by=dates)
        fc_arima_xts <- xts(x=fc_arima, order.by=dates)
        fc_nnetar_xts <- xts(x=fc_nnetar, order.by=dates)
        
        trace_0 <- data.frame(date=index(data_fc_train), coredata(data_fc_train[,7]))
        if(input$trainslider != 0){
          trace_train <- data.frame(date=index(data_fc), coredata(data_fc[,7]))
          trace_train <- anti_join(trace_train, trace_0)
          trace_ensemble <- data.frame(date=index(fc_meanf_xts), ensemble_fc)
          }
        trace_1 <- data.frame(date=index(fc_meanf_xts), coredata(fc_meanf_xts))
        trace_2 <- data.frame(date=index(fc_naive_xts), coredata(fc_naive_xts))
        trace_3 <- data.frame(date=index(fc_snaive_xts), coredata(fc_snaive_xts))
        trace_4 <- data.frame(date=index(fc_rwf_xts), coredata(fc_rwf_xts))
        #trace_5 <- data.frame(date=index(fc_croston_xts), coredata(fc_croston_xts))
        #trace_6 <- data.frame(date=index(fc_stlf_xts), coredata(fc_stlf_xts))
        trace_7 <- data.frame(date=index(fc_ses_xts), coredata(fc_ses_xts))
        trace_8 <- data.frame(date=index(fc_holt_xts), coredata(fc_holt_xts))
        # #trace_9 <- data.frame(date=index(fc_hw_xts), coredata(fc_hw_xts))
        trace_10 <- data.frame(date=index(fc_splinef_xts), coredata(fc_splinef_xts))
        trace_11 <- data.frame(date=index(fc_thetaf_xts), coredata(fc_thetaf_xts))
        trace_12 <- data.frame(date=index(fc_ets_xts), coredata(fc_ets_xts))
        trace_13 <- data.frame(date=index(fc_tbats_xts), coredata(fc_tbats_xts))
        trace_14 <- data.frame(date=index(fc_arima_xts), coredata(fc_arima_xts))
        trace_15 <- data.frame(date=index(fc_nnetar_xts), coredata(fc_nnetar_xts))
        
        x <- seq.Date(from=min(index(data_fc)), to=max(index(fc_meanf_xts)), by="days")
        data_fc_merged <- data.frame("date"=x)
        data_fc_merged <- merge(data_fc_merged, trace_0, by="date", all.x = TRUE)
        if(input$trainslider != 0){
          data_fc_merged <- merge(data_fc_merged, trace_train, by="date", all.x = TRUE)
          data_fc_merged <- merge(data_fc_merged, trace_ensemble, by="date", all.x = TRUE)
        }
        data_fc_merged <- merge(data_fc_merged, trace_1, by="date", all.x = TRUE)
        data_fc_merged <- merge(data_fc_merged, trace_2, by="date", all.x = TRUE)
        data_fc_merged <- merge(data_fc_merged, trace_3, by="date", all.x = TRUE)
        data_fc_merged <- merge(data_fc_merged, trace_4, by="date", all.x = TRUE)
        #data_fc_merged <- merge(data_fc_merged, trace_5, by="date", all.x = TRUE)
        #data_fc_merged <- merge(data_fc_merged, trace_6, by="date", all.x = TRUE)
        data_fc_merged <- merge(data_fc_merged, trace_7, by="date", all.x = TRUE)
        data_fc_merged <- merge(data_fc_merged, trace_8, by="date", all.x = TRUE)
        # #data_fc_merged <- merge(data_fc_merged, trace_9, by="date", all.x = TRUE)
        data_fc_merged <- merge(data_fc_merged, trace_10, by="date", all.x = TRUE)
        data_fc_merged <- merge(data_fc_merged, trace_11, by="date", all.x = TRUE)
        data_fc_merged <- merge(data_fc_merged, trace_12, by="date", all.x = TRUE)
        data_fc_merged <- merge(data_fc_merged, trace_13, by="date", all.x = TRUE)
        data_fc_merged <- merge(data_fc_merged, trace_14, by="date", all.x = TRUE)
        data_fc_merged <- merge(data_fc_merged, trace_15, by="date", all.x = TRUE)
        
        #"stlf",
        #"croston","hw","ses", "holt", 
        #"splinef","thetaf","ets", "tbats", "autoarima"
        if(input$trainslider != 0){
          names(data_fc_merged) <- c("date", "Price", "Price_Validation", "Ensemble_FC", "meanf", "naive", "snaive", 
        "rwf","ses", "holt","splinef","thetaf", "ets", "tbats", "arima", "nnetar")}
        else{
          names(data_fc_merged) <- c("date", "Price", "meanf", "naive", "snaive", 
                                     "rwf","ses", "holt","splinef","thetaf", "ets", "tbats", "arima", "nnetar")
        }
        
        fig <- plot_ly(data_fc_merged, x=~date, y = ~Price, name = 'Price', type = 'scatter', mode = 'lines')
        if(input$trainslider != 0){
        fig <- fig %>% add_trace(y = ~Price_Validation, name = 'Price_Validation', mode = 'lines')
        fig <- fig %>% add_trace(y = ~Ensemble_FC, name = 'Ensemble_FC', mode = 'lines')
        }
        fig <- fig %>% add_trace(y = ~meanf, name = 'meanf', mode = 'lines')
        fig <- fig %>% add_trace(y = ~naive, name = 'naive', mode = 'lines')
        fig <- fig %>% add_trace(y = ~snaive, name = 'snaive', mode = 'lines')
        fig <- fig %>% add_trace(y = ~rwf, name = 'rwf', mode = 'lines')
        #fig <- fig %>% add_trace(y = ~croston, name = 'croston', mode = 'lines')
        #fig <- fig %>% add_trace(y = ~stlf, name = 'stlf', mode = 'lines')
        fig <- fig %>% add_trace(y = ~ses, name = 'ses', mode = 'lines')
        fig <- fig %>% add_trace(y = ~holt, name = 'holt', mode = 'lines')
        # fig <- fig %>% add_trace(y = ~hw, name = 'hw', mode = 'lines')
        fig <- fig %>% add_trace(y = ~splinef, name = 'splinef', mode = 'lines')
        fig <- fig %>% add_trace(y = ~thetaf, name = 'thetaf', mode = 'lines')
        fig <- fig %>% add_trace(y = ~ets, name = 'ets', mode = 'lines')
        fig <- fig %>% add_trace(y = ~tbats, name = 'tbats', mode = 'lines')
        fig <- fig %>% add_trace(y = ~arima, name = 'arima', mode = 'lines')
        fig <- fig %>% add_trace(y = ~nnetar, name = 'nnetar', mode = 'lines')
        fig
      })
    } 
    

    
    
    
    
    
    
    
    
    
    

  })
  observeEvent(input$stockOverviewInd_rows_selected, {
    if (stocks_picked$Symbol[input$stockOverviewInd_rows_selected] %in% ls(envir = .GlobalEnv)) {
      data_stock <<- get(stocks_picked$Symbol[input$stockOverviewInd_rows_selected], envir = .GlobalEnv)
      data_plot <- data.frame("Date"= index(data_stock), "Adjusted" = select(as.data.frame(data_stock),contains("Adjusted")))
      names(data_plot) <- c("Date", "Adjusted")
      data_plot$RSI <- RSI(data_plot$Adjusted)
      data_plot$MACD1 <- MACD(data_plot$Adjusted)[,1]
      data_plot$MACD2 <- MACD(data_plot$Adjusted)[,2]
      names(data_plot) <- c("Date", "Adjusted", "RSI", "MACD1", "MACD2")

      output$plotIndCore <- renderPlotly({plot_ly(data_plot, x = ~Date, y = ~Adjusted, type = 'scatter', mode = 'lines', 
                line = list(color = "rgb(0, 0, 0)")) %>% layout(title = "Date", xaxis = list(title = "Date", zeroline = FALSE), yaxis = list(title = "Price", zeroline = FALSE))})
      output$plotIndMACD <- renderPlotly({
        fig <- plot_ly(data_plot, x=~Date, y = ~MACD1, name = 'MACD', type = 'scatter', mode = 'lines')
        fig <- fig %>% add_trace(y = ~MACD2, name = 'MACD_Signal', mode = 'lines')
        fig
        })
        
        
        
       # plot_ly(data_plot, x = ~Date, y = ~MACD, type = 'scatter', mode = 'lines', 
      #                                              line = list(color = "rgb(0, 0, 0)")) %>% layout(title = "Date", xaxis = list(title = "Date", zeroline = FALSE), yaxis = list(title = "Price", zeroline = FALSE))})
      output$plotIndRSI <- renderPlotly({plot_ly(data_plot, x = ~Date, y = ~RSI, type = 'scatter', mode = 'lines', 
                                                  line = list(color = "rgb(0, 0, 0)")) %>% layout(title = "Date", xaxis = list(title = "Date", zeroline = FALSE), yaxis = list(title = "Price", zeroline = FALSE))})
        
        
    } 
  })
  observeEvent(input$stockOverviewBlog_rows_selected,{
    if (stocks_picked$Symbol[input$stockOverviewBlog_rows_selected] %in% ls(envir = .GlobalEnv)) {
      data_stock <<- get(stocks_picked$Symbol[input$stockOverviewBlog_rows_selected], envir = .GlobalEnv)
      address_performance <- paste0("https://www.marketwatch.com/investing/stock/",tolower(stocks_picked$Symbol[input$stockOverviewBlog_rows_selected]))
      address_analyst <- paste0("https://www.marketwatch.com/investing/stock/", tolower(stocks_picked$Symbol[input$stockOverviewBlog_rows_selected]), "/analystestimates?mod=mw_quote_analyst")
       performance <- read_html(address_performance) %>%
        html_nodes(".right, .value") %>%
        html_text()
      analyst_opinions <- read_html(address_analyst) %>%
        html_nodes(".analyst-ratings, .value") %>%
        html_text()

      analyst_opinions <- na.omit(as.numeric(analyst_opinions))
      df_performance <- data.frame("5 Day" = performance[9], "1 Month" = performance[10], "3 Month" = performance[11], "YTD" = performance[12], "1 Year" =performance[13])
      names(df_performance) <- c("5 Day", "1 Month", "3 Month", "YTD", "1 Year")
      if(length(analyst_opinions) >= 5){
        df_analyst <- data.frame(analyst_opinions[length(analyst_opinions) - 4],
                                 analyst_opinions[length(analyst_opinions) - 3],
                                 analyst_opinions[length(analyst_opinions) - 2],
                                 analyst_opinions[length(analyst_opinions) - 1],
                                 analyst_opinions[length(analyst_opinions)])
      }
      else(df_analyst <- data.frame("Buy"="no data", "Overweight"="no data", "Hold"="no data", "Underweight"="no data", "Sell"="no data"))

      names(df_analyst) <- c("Buy", "Overweight", "Hold", "Underweight", "Sell")
      output$performanceTable <- renderDataTable(df_performance,options= list(scrollY = TRUE,pageLength = 5))
      output$analystTable <- renderDataTable(df_analyst,options= list(scrollY = TRUE,pageLength = 5))
    } 
  })
  observeEvent(input$recommendButton, {
    for(i in 1:length(symbols_fields)){
      #load stocks after symbols
      symbols_Nasdaq <- stockSymbols("NASDAQ")[,c(1:2)]
      symbols_Nasdaq <- na.omit(symbols_Nasdaq)
      symbols_choice <- symbols_fields[[i]]
      symbols <- symbols_Nasdaq[symbols_Nasdaq$Symbol %in% symbols_choice,]
      if(input$dates[1] == input$dates[2]){
        for (j in 1:length(symbols$Symbol)){
          try(getSymbols(symbols$Symbol[j], from = "2017-12-31", to=as.character(Sys.Date()) ,auto.assign = T))}
      }
      else{
        for (j in 1:length(symbols$Symbol)){
          try(getSymbols(symbols$Symbol[j], from = as.character(input$dates[1]), to=as.character(input$dates[2]) ,auto.assign = T))}
      }
      for(k in 1:nrow(symbols)){
        data_stock_recommend <- get(objects()[objects() %in% symbols_choice[k]])
        print(head(data_stock_recommend))
        #todo: very important DS stuff
      }

      stocks_picked <<- symbols_Nasdaq[symbols_Nasdaq$Symbol %in% objects(),]

    }
    
  })
  
}

# performance <- read_html("https://www.marketwatch.com/investing/stock/aacg") %>%
#   html_nodes(".right, .value") %>%
#   html_text()


# 
# 
# html_table(x, header = NA, trim = TRUE, fill = FALSE, dec = ".")
# 
# div:nth-child(3)
# 
# https://www.marketwatch.com/investing/stock/aacg/analystestimates?mod=mw_quote_analyst


#https://www.marketwatch.com/investing/stock/aacg
#https://www.cnbc.com/quotes/?symbol=aacg&qsearchterm=aacg

shinyApp(ui, server)