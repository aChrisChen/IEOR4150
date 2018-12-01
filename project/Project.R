

# ================= Part1: Initialization ====================
packages.Needed <- c("shiny", "quantmod", "ggplot2", "dplyr", "shinythemes", "httr", "forecast", "randtests")
packages.Uninstalled <- packages.Needed[!(packages.Needed %in% installed.packages()[, "Package"])]
if (length(packages.Uninstalled) > 0) {
  install.packages(packages.Uninstalled)
}
lapply(packages.Needed, library, character.only = TRUE)


stocks = c("AAPL", "AMZN", "FB", "GOOG", "GOOGL", "MSFT", "MU", "NFLX", "NOW","NVDA", "other")
start_date = "2015-01-01"
end_date = "2018-01-01"
stocks_env = new.env()
stocks_env2 = new.env()
stocks_env3 = new.env()

# =========== Part2: User Interface =============
# ===========** first part =========== 
firstPart <- sidebarLayout(
    sidebarPanel( width = 3,
      # h4("One Symbol Analysis"),
      wellPanel(
        dateInput('startDate1',
                  label = 'Start date:',
                  value = start_date
        ),
        dateInput('endDate1',
                  label = 'End date:',
                  value = end_date
        ),
        
        selectInput(
          inputId = "select11",
          label = "Stock Ticker",
          choices = stocks,
          selected = "AAPL"
        ),
        conditionalPanel(
          condition = "input.select11 == 'other'",
          textInput(
            inputId = "otherStock11",
            label = "Please enter the Ticker"
          )
        )
      )
    ),
    mainPanel( width = 9,
      tabsetPanel(
        tabPanel(
          title = "Hist of Log Return",
          plotOutput("histLog1")
        ),
        tabPanel(
          title = "QQ Plot",
          plotOutput("qqPlot1"),
          br(),
          h3("QQ Plot Details Summary"),
          verbatimTextOutput("qqPlotSummary1")
        ),
        tabPanel(
          title = "Confidence Interval",
          sliderInput(inputId = "alpha", label = "Significance (alpha)", value=0.95, min=0, max=1),
          p("Confidence Interval of Mean"),
          verbatimTextOutput('meanCI'),
          p("Confidence Interval of Variance"),
          verbatimTextOutput('varCI')
        ),
        tabPanel(
          title = "Regression",
          plotOutput("regressionPlot1"),
          br(),
          h3("Linear Regression Details Summary"),
          verbatimTextOutput('regressionSummary11'),
          verbatimTextOutput('regressionSummary12')
        ),
        tabPanel(
          title = "TbatsForecast",
          plotOutput("tbatsForecast"),
          br(),
          h3("Tbats Forecast Details Summary"),
          verbatimTextOutput('tbatsForecastSummary')
        )
      )
    )
  )
# ===========** second part =========== 
secondPart <- sidebarLayout(
  sidebarPanel( width = 3,
    # h4("Two Symbols Analysis"),
    wellPanel(
      dateInput('startDate2',
                label = 'Start date:',
                value = start_date
      ),
      dateInput('endDate2',
                label = 'End date:',
                value = end_date
      ),
      
      selectInput(
        inputId = "select21",
        label = "Stock Ticker 1",
        choices = stocks,
        selected = "AAPL"
      ),
      conditionalPanel(
        condition = "input.select21 == 'other'",
        textInput(
          inputId = "otherStock21",
          label = "Please enter the Ticker"
        )
      ),
      selectInput(
        inputId = "select22",
        label = "Stock Ticker 2",
        choices = stocks,
        selected = "AMZN"
      ),
      conditionalPanel(
        condition = "input.select22 == 'other'",
        textInput(
          inputId = "otherStock22",
          label = "Please enter the Ticker"
        )
      )
      
    )
  ),
  mainPanel(width = 9,
    tabsetPanel(
      tabPanel(
        title = "Regression",
        plotOutput("twoStockRegression"),
        br(),
        h3("Two Stock Regression Details Summary"),
        verbatimTextOutput('twoStockRegressionSummary')
      ),
      tabPanel(
        title = "tTest",
        verbatimTextOutput("tTestForTwoStocks")
      ),
      tabPanel(
        title = "Cov and Corr",
        br(),
        h3("Covariance for 10 selected Symbols"),
        verbatimTextOutput("cov"),
        br(),
        h3("Correlation Matrix for 10 selected Symbols"),
        verbatimTextOutput("cor")
      )
    )
  )
)

ui <- fluidPage(
  theme = shinytheme('darkly'),
  
  htmlTemplate(filename = "Template.html",
               singleAnalysis = firstPart,
               doubleAnalysis = secondPart
  )
  
  
  
)
# =========== Part3: Support Functions for Server =============
# =========== ** Single =============
# Arguments needed: df
# df: (data.frame with col names ["open", "high", "low", "close", "volume", "adjusted", "log_returns"]
# Arguments needed: dfWeek
# dfWeek: (data.frame with col names ["year.week", "log_returns"])
# Arguments needed: stock
# stock: the name of the stock
# Arguments needed: alpha 
# alpha: significance to test confidence interval of mean and variance to daily log-return for the specified stock

# For Q1 : How is the log-return of each candidate stock distributed?
CalBreak <- function(df) {
  # Get the number of breaks of the histogram of the log_returns of specific stock.
  #
  # Args:
  #   df: data.frame with col names ["open", "high", "low", "close", "volume", "adjusted", "log_returns", "date])
  # 
  # Returns: proper breaks to build the histogram of the log_returns of specific stock.
  row = dim(df)[1]
  cal_break = round(sqrt(row)) - 1
  return(cal_break)
}

HistForLogReturns <- function(df, stock) {
  # Get the histogram of the log_returns of specific stock.
  #
  # Args:
  #   df: data.frame with col names ["open", "high", "low", "close", "volume", "adjusted", "log_returns"])
  #   stock: the name the stock
  # 
  # Returns:
  #   null (Histogram)
  startDate = rownames(df)[1]
  endDate = rownames(df)[dim(df)[1]]
  hist(df$log_returns, breaks = CalBreak(df), Freq = FALSE, xlab = "log-return", ylab = "Probabilities (%)",
       main = paste0("Histogram of ", stock, " log-return\n from ", startDate, " to ", endDate))
}

#For Q2: sample quantile- normal distribution quantile
QqPlotForLogReturns <- function(df, stock) {
  # Get the Q–Q (quantile-quantile) plot and its distribution plot of the log_returns of specific stock.
  #
  # Args:
  #   df: data.frame with col names ["open", "high", "low", "close", "volume", "adjusted", "log_returns"])
  #   stock: the name the stock
  # 
  # Returns:
  #   null (Q–Q plot and Distribution plot)
  startDate = rownames(df)[1]
  endDate = rownames(df)[dim(df)[1]]
  title = paste0("Normal Q-Q Plot of ", stock, " log-return\n from ", startDate, " to ", endDate)
  qqnorm(df$log_returns, main = title)
  qqline(df$log_returns) 
}

QqPlotSummary <- function(df, stock) {
  # Get the P-value of the log_returns of specific stock.
  #
  # Args:
  #   df: data.frame with col names ["open", "high", "low", "close", "volume", "adjusted", "log_returns"])
  #   stock: the name the stock
  # 
  # Returns:
  #   P_value to the log_returns of specific stock .
  
  runs.test(df$log_returns)
}

# QqPlotForLogReturns(df, "AAPL")

#For Q3: We build
ConfidenceInterval <- function(df, stock, alpha) {
  # Given the significance, the function will find the confindence interval of mean and variance 
  # of the daily log_returns of specific stock.
  #
  # Args:
  #   df: data.frame with col names ["open", "high", "low", "close", "volume", "adjusted", "log_returns"])
  #   stock: the name the stock
  #   alpha: significance for confidence interval test
  # 
  # Returns:
  #   a vector like (meanCILeft, meanCIRight, varCILeft, varCIRight)
  
  # CI for mean
  n = dim(df)[1]
  S = var(df$log_returns)
  # t_(alpha/2) with degree n-1
  t = qt(1-(alpha/2) , n-1)
  X_mean = mean(df$log_returns)
  length = 2 * S * t / sqrt(n)
  meanLeft = round(X_mean - length/2, 8)
  meanRight = round(X_mean + length/2, 8)
  
  # CI for variance
  chi_0.025_n_1 = qchisq(1 - (alpha/2), n-1)
  chi_0.975_n_1 = qchisq(1 - (1-(alpha/2)), n-1)
  
  varLeft = round((n-1)*S/chi_0.025_n_1, 8)
  varRight = round((n-1)*S/chi_0.975_n_1, 8)
  
  result = c(meanLeft, meanRight, varLeft, varRight)
  return(result)
}

# ConfidenceInterval(df, "AAPL", 0.1)



#For Q4.1: We build
RegressionPlot <- function(df, stock) {
  # Get the scatter plot of daily log-return and redsidual log-return as time go, and build linear regression line
  #
  # Args:
  #   df: data.frame with col names ["open", "high", "low", "close", "volume", "adjusted", "log_returns"])
  #   stock: the name the stock
  # 
  # Returns:
  #   scatter plot and fit line for log-return and log-return residual
  
  #first scatter plot for log-return
  doublePar <- par(mfrow=c(1, 2)) # divide graph area in 2 columns
  plot(df$date, df$log_return, main = paste0("Scatterplot for stock: ", stock), xlab = "Date", 
       ylab = "log-return", pch = 16)
  
  #Fit line (Y ~ X) to scatter plot for log-return
  abline(lm(df$log_returns ~ df$date), col = "red") 
  
  #Second scatter plot for log-return residual
  residualLogReturn <- resid(lm(df$log_returns ~ df$date))
  plot(df$date, residualLogReturn, main = paste0("Residual plot for stock: ",stock), xlab = "Date", 
       ylab = "residuals", pch = 16)
  
  #Fit line (Y~ X) to scatter plot for redsidual log-return                            
  abline(lm(residualLogReturn ~ df$date), col = "red") 
  par(doublePar)
  
  
}

RegressionSummary <- function(df, stock) {
  # Get the summary of linear regression line for daily log-return 
  #
  # Args:
  #   df: data.frame with col names ["open", "high", "low", "close", "volume", "adjusted", "log_returns"])
  #   stock: the name the stock
  # 
  # Returns:
  #   the summary of linear regression line for daily log-return 
  
  fitInformation <- lm(df$log_returns ~ df$date)
  return(summary(fitInformation))
  
}

#For Q4.3: We create
AicBicSummary <- function(df, stock) {
  # Get the AIC and BIC information of linear regression line for daily log-return 
  #
  # Args:
  #   df: data.frame with col names ["open", "high", "low", "close", "volume", "adjusted", "log_returns"])
  #   stock: the name the stock
  # 
  # Returns:
  #   a string, AIC and BIC value of linear regression line for daily log-return 
  
  fitInformation <- lm(df$log_returns ~ df$date)
  
  return(c(AIC(fitInformation), BIC(fitInformation)))
  
}


# AicBicSummary(df,"AAPL")


#"Forecasting Daily Data with Multiple Seasonality in R"
#David Benson(n.d). 
#Retreived from: http://www.dbenson.co.uk/Rparts/subpages/forecastR/
#For Q5.1: We create
TbatsForecast <- function(df, stock){
  # Get the seasonality of the data of daily log-return and forecast future log-return 
  #
  # Args:
  #   df: data.frame with col names ["open", "high", "low", "close", "volume", "adjusted", "log_returns"])
  #   stock: the name the stock
  # 
  # Returns:
  #  graph of the seasonality of the data of daily log-return 
  #  and TBATS forecast future log-return value
  
  #transfer date data format from "Character" to "Date"
  strDate <- rownames(df)
  df$date <- as.Date(strDate, "%Y-%m-%d")
  
  
  doublePar2 <- par(mfrow=c(1, 2)) # divide into two column to show graphs
  
  #create a msts object (multiple season time series object)
  msts <- msts(df$log_returns, seasonal.periods = c(5,269.25))
  
  #plot almost 3 years daily log-return data based on season value 5
  plot(msts, main="Daily Log_Return", xlab="Year", ylab="Log_return")
  
  #Use TBATS to extract multiple seasonal components
  tbats <- tbats(msts)
  
  ###plot(tbats, main="Multiple Season Decomposition")
  
  #Use TBATS to predict future log-return trend
  sp<- predict(tbats,h=14)
  plot(sp, include=14)
}
# TbatsForecast(df,"AAPL")


#For Q5.2: We create
TbatsSummary <- function(df, stock){
  # Summarize TBATS forecasting details for the seasonality of the data of daily log-return
  #
  # Args:
  #   df: data.frame with col names ["open", "high", "low", "close", "volume", "adjusted", "log_returns"])
  #   stock: the name the stock
  # 
  # Returns:
  #  Summary of TBATS forecasting details
  
  #transfer date data format from "Character" to "Date"
  strDate <- rownames(df)
  df$date <- as.Date(strDate, "%Y-%m-%d")
  
  #create a msts object (multiple season time series object)
  msts <- msts(df$log_returns, seasonal.periods = c(5,269.25))
  
  #Use TBATS to extract multiple seasonal components
  tbats <- tbats(msts)
  
  ###plot(tbats, main="Multiple Season Decomposition")
  
  #Use TBATS to predict future log-return trend
  sp<- predict(tbats,h=14)
  return(summary(sp))
}

# TbatsSummary(df,"AAPL")

# =========== ** Double =============

# df21, df22, stock21, stock22
# df21: First Selected Stock
# df22: Second Selected Stock
# stock21: name of the first selected stock
# stock22: name of the second selected stock

#For Q6.1: We build
TwoStockRegressionPlot <- function(df21, df22, stock21, stock22) {
  # Get the scatter plot of daily log-return and redsidual log-return as time go for two stocks
  # and build linear regression line
  #
  # Args:
  #   df1, df2: data.frame with col names ["open", "high", "low", "close", "volume", "adjusted", "log_returns"])
  #   stock21, stock22: the name of the stock
  # 
  # Returns:
  #   scatter plot and fit line for two stocks' log-return and log-return residual
  
  #first scatter plot for stock1's log-return and stock2's log-return 
  doublePar <- par(mfrow=c(1, 2)) # divide graph area in 2 columns
  plot(df21$log_returns, df22$log_returns, main = paste0("Scatterplot for stock: ", stock21, " and ", stock22), 
       xlab = paste0("log-return of ", stock21), 
       ylab = paste0("log-return of ", stock22), pch = 16)
  
  #Fit line (Y ~ X) to scatter plot for two stocks' log-return
  abline(lm(df22$log_returns ~ df21$log_returns), col = "red") 
  
  #Second scatter plot for two stocks' log-return residual
  residualLogReturn <- resid(lm(df22$log_returns ~ df21$log_returns))
  plot(df21$log_returns, residualLogReturn, main = paste0("Residual plot for stock: ", stock21, " and ", stock22),
       xlab = paste0("log-return of ", stock21), 
       ylab = "residuals for two stocks", pch = 16)
  
  #Fit line (Y~ X) to scatter plot for redsidual log-return                            
  abline(lm(residualLogReturn ~ df21$log_returns), col = "red") 
  par(doublePar)
}

#For Q6.2: We create
TwoStockRegressionSummary <- function(df21, df22, stock21, stock22){
  # Get the summary of linear regression line for daily log-return 
  #
  # Args:
  #   df: data.frame with col names ["open", "high", "low", "close", "volume", "adjusted", "log_returns"])
  #   stock: the name the stock
  # 
  # Returns:
  #   the summary of linear regression line for daily log-return 
  
  strDate <- rownames(df)
  df$date <- as.Date(strDate, "%Y-%m-%d")
  
  fitInformation <- lm(df22$log_returns ~ df21$log_returns)
  print(paste0("Stock => ",stock21," and ", stock22, " linear regression details"))
  return(summary(fitInformation))
  
}
# TwoStockRegressionSummary(df21, df22, stock21, stock22)



#For Q6.3: We build
TTestForTwoStocks <- function(df21, df22, stock21, stock22){
  # Get the t-test details for daily log-return for two stocks
  # 
  #
  # Args:
  #   df1, df2: data.frame with col names ["open", "high", "low", "close", "volume", "adjusted", "log_returns"])
  #   stock21, stock22: the name of the stock
  # 
  # Returns:
  #   t-test summary for two stocks
  
  t.test(df21$log_returns, df22$log_returns)
  
}

#For 7.1: We create
CovarianceMatrix <- function(logReturnsAll){
  # Get covariance matrix from 10 default stocks
  #
  # Args:
  #   logreturnAll: data.frame with col names [(stock names....)]) n * 10
  #  
  # 
  # Returns:
  #  covariance matrix for 10 stocks
  
  #covariance matrix 10x10
  # rownames(logReturnsAll) <- {}
  return(signif(cov(logReturnsAll), digits = 1))
}


#Correlation matrix : A quick start guide to analyze, format and visualize a correlation matrix using R software. (n.d.). 
#Retrieved from http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software
#For 7.2: We create
CorrelationMatrix <- function(logReturnsAll){
  # Get correlation matrix from 10 default stocks
  #
  # Args:
  #   logreturnAll: data.frame with col names [(stock names....)]) n * 10
  #  
  # 
  # Returns:
  #  correlation matrix for 10 stocks

  #covariance matrix 10x10
  return (signif(cor(logReturnsAll), digits = 3))
}



# ============== Part4: Server ======================
server <- function(input, output) {
  stock <- reactive({
    stock.name <- input$select11
    if (stock.name == "other") {
      stock.name <- input$otherStock11
    }
    stock.name
  })
  
  df <- reactive({
    # if (!stock() %in% names(stocks_env) && stock() != ".getSymbols"){
    #   getSymbols(Symbols = stock(), from = input$startDate, to = input$endDate, env = stocks_env)
    # }
    getSymbols(Symbols = stock(), from = input$startDate1, to = input$endDate1, env = stocks_env)
    
    df = data.frame(stocks_env[[stock()]])
    colnames(df) <- c("open", "high", "low", "close", "volume", "adjusted")
    df$log_returns = log(df$close / df$open)
    df$date <- as.Date(rownames(df), "%Y-%m-%d")
    df
  })
  
  dfWeek <- reactive({
    df <- df()
    df$year.week <- strftime(row.names(df), format = "%Y.%V")
    dfWeek <- aggregate(df$log_returns, by = list(df$year.week), FUN = sum)
    rownames(dfWeek) <- dfWeek[, 1]
    colnames(dfWeek) <- c("year.week", "log_returns")
    dfWeek
  })
  # =============== For Part 1 ================
  
  # =============== ** 1-1 ===============
  output$histLog1 <- renderPlot({
    HistForLogReturns(df(), stock())
  })
  
  # =============== ** 1-2 ===============
  output$qqPlot1 <- renderPlot({
    QqPlotForLogReturns(df(), stock())
  })
  
  output$qqPlotSummary1 <- renderPrint({
    QqPlotSummary(df(), stock())
  })
  
  # =============== ** 1-3 ===============
  output$meanCI <- renderText({
    result <- ConfidenceInterval(df(), stock(), input$alpha)
    paste0("[", result[1], ", ", result[2], "]")
  })
  
  output$varCI <- renderText({
    result <- ConfidenceInterval(df(), stock(), input$alpha)
    paste0("[", result[3], ", ", result[4], "]")
  })
  
  # =============== ** 1-4 ===============
  output$regressionPlot1 <- renderPlot({
    RegressionPlot(df(), stock())
  })
  
  output$regressionSummary11 <- renderPrint({
    RegressionSummary(df(), stock())
  })
  
  output$regressionSummary12 <- renderText({
    result <- AicBicSummary(df(), stock())
    paste(
      paste0("AIC Measure: ", round(result[1], 3)),
      paste0("BIC Measure: ", round(result[2], 3)),
      sep = ", "
    )
  })
  
  # =============== ** 1-5===============
  output$tbatsForecast <- renderPlot({
    TbatsForecast(df(), stock())
  })
  
  output$tbatsForecastSummary <- renderPrint({
    TbatsSummary(df(), stock())
  })
  
  # =============== For Part 2 ================
  stock21 <- reactive({
    stock.name <- input$select21
    if (stock.name == "other") {
      stock.name <- input$otherStock21
    }
    stock.name
  })
  
  stock22 <- reactive({
    stock.name <- input$select22
    if (stock.name == "other") {
      stock.name <- input$otherStock22
    }
    stock.name
  })
  
  df21 <- reactive({
    # if (!stock() %in% names(stocks_env) && stock() != ".getSymbols"){
    #   getSymbols(Symbols = stock(), from = input$startDate, to = input$endDate, env = stocks_env)
    # }
    getSymbols(Symbols = stock21(), from = input$startDate2, to = input$endDate2, env = stocks_env2)
    
    df = data.frame(stocks_env2[[stock21()]])
    colnames(df) <- c("open", "high", "low", "close", "volume", "adjusted")
    df$log_returns = log(df$close / df$open)
    df$date <- as.Date(rownames(df), "%Y-%m-%d")
    df
  })
  
  df22 <- reactive({
    # if (!stock() %in% names(stocks_env) && stock() != ".getSymbols"){
    #   getSymbols(Symbols = stock(), from = input$startDate, to = input$endDate, env = stocks_env)
    # }
    getSymbols(Symbols = stock22(), from = input$startDate2, to = input$endDate2, env = stocks_env2)
    
    df = data.frame(stocks_env2[[stock22()]])
    colnames(df) <- c("open", "high", "low", "close", "volume", "adjusted")
    df$log_returns = log(df$close / df$open)
    df$date <- as.Date(rownames(df), "%Y-%m-%d")
    df
  })
  
  logReturnsAll <- reactive({
    stock.names <- stocks[1:10]
    getSymbols(Symbols = stock.names, from = input$startDate2, to = input$endDate2, env = stocks_env3)
    i <- 1
    for (stock in stock.names){
      df = data.frame(stocks_env3[[stock]])
      colnames(df) <- c("open", "high", "low", "close", "volume", "adjusted")
      df$log_returns = log(df$close / df$open)
      
      if (i == 1){
        result <- data.frame(df$log_returns, row.names = rownames(df))
        colnames(result) <- stock.names[1]
      } else {
        result[, stock] <- df$log_returns
      }
      i <- i + 1;
    }
    result
  })
  
  # =============== ** 2-1 ===============
  output$twoStockRegression <- renderPlot({
    TwoStockRegressionPlot(df21(), df22(), stock21(), stock22())
  })
  
  output$twoStockRegressionSummary <- renderPrint({
    TwoStockRegressionSummary(df21(), df22(), stock21(), stock22())
  })
  
  # =============== ** 2-2 ===============
  output$tTestForTwoStocks <- renderPrint({
    TTestForTwoStocks(df21(), df22(), stock21(), stock22())
  })
  
  # =============== ** 2-3 ===============
  output$cov <- renderPrint({
    CovarianceMatrix(logReturnsAll())
  })
  
  output$cor <- renderPrint({
    CorrelationMatrix(logReturnsAll())
  })
}


shinyApp(ui = ui, server = server)