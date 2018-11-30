library("shiny")
library("quantmod")

stocks = c("AAPL", "AMZN", "FB", "GOOG", "GOOGL", "MSFT", "MU", "NFLX", "NOW","NVDA")
# stocks = c("AAPL", "AMZN", "FB", "GOOG")
start_date = "2015-01-01"
end_date = "2018-01-01"

stocks_env = new.env()
getSymbols(Symbols = stocks, from = start_date, to = end_date, env = stocks_env)

cal_break <- function(df) {
  row = dim(df)[1]
  cal_break = round(sqrt(row)) - 1
  return(cal_break)
}

ui <- fluidPage(
  # Input() functions,
  selectInput(inputId = "stock", label = "Choose a Stock", choices = stocks),
  # Output() functions
  
  plotOutput(outputId = "hist_daily"),
  plotOutput(outputId = "hist_weekly")
  
)

server <- function(input, output) {

  # render*() functions that creates the things you want
  # * ------ type of the object to build
  # {...} is the code block
  output$hist_daily <- renderPlot({
    df = data.frame(stocks_env[[input$stock]])
    colnames(df) <- c("open", "high", "low", "close", "volume", "adjusted")
    df$log_returns = log(df$close / df$open)
    df$year.week <- strftime(row.names(df), format = "%Y.%V")
    
    df1 <- df[1:(dim(df)[1]/2), ]
    df1$aim = sqrt(5) * (df1$log_returns - mean(df1$log_returns))
    
    title = paste0("sqrt(5) * (log_returns - mean) by day")
    hist(df1$aim, breaks = cal_break(df1), main = title)
    })
  
  output$hist_weekly <- renderPlot({
    df = data.frame(stocks_env[[input$stock]])
    colnames(df) <- c("open", "high", "low", "close", "volume", "adjusted")
    df$log_returns = log(df$close / df$open)
    df$year.week <- strftime(row.names(df), format = "%Y.%V")
  
    df2 <- df[(dim(df)[1]/2):(dim(df)[1]), ]
    df2.week <- aggregate(df2$log_returns, by = list(df2$year.week), FUN = sum)
    rownames(df2.week) <- df2.week[, 1]
    colnames(df2.week) <- c("year.week", "log_returns")
    df2.week$aim = df2.week$log_returns - mean(df2.week$log_returns)

    title = paste0("(log_returns - mean) by week")
    hist(df2.week$aim, breaks = cal_break(df), main = title)
  })

}

shinyApp(ui = ui, server = server)
