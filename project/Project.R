# ============================
# Part1: Initialization
# ============================

packages.Needed <- c("shiny", "quantmod", "ggplot2", "dplyr", "shinythemes")
packages.Uninstalled <- packages.Needed[!(packages.Needed %in% installed.packages()[, "Package"])]
if (length(packages.Uninstalled) > 0) {
  install.packages(packages.Uninstalled)
}
lapply(packages.Needed, library, character.only = TRUE)


stocks = c("AAPL", "AMZN", "FB", "GOOG", "GOOGL", "MSFT", "MU", "NFLX", "NOW","NVDA", "other")
start_date = "2015-01-01"
end_date = "2018-01-01"
stocks_env = new.env()

# ============================
# Part2: User Interface
# ============================
ui <- fluidPage(
  titlePanel(title = "Statistical Analysis of Stocks", windowTitle = "Stock Stat Analysis"),
  sidebarLayout(
    sidebarPanel(
      h4("One Symbol Analysis"),
      wellPanel(
        dateInput('startDate',
                  label = 'Start date: yyyy-mm-dd',
                  value = start_date
        ),
        dateInput('endDate',
                  label = 'End date: yyyy-mm-dd',
                  value = end_date
        ),
        
        selectInput(
          inputId = "select1",
          label = "Stock Symbol",
          choices = stocks,
          selected = "AAPL"
        ),
        conditionalPanel(
          condition = "input.select1 == 'other'",
          textInput(
            inputId = "otherStock",
            label = "Please enter the Symbol"
            )
        )
      )
    ),
      
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = "Hist of Log Return",
          plotOutput("histLog")
        ),
        tabPanel(
          title = "QQ Plot",
          plotOutput("qqPlot")
        )
      )
    )
  )
)
# ============================
# Part3: Support Functions for Server
# ============================

# 可以用的数据结构有 df
# df: (data.frame with col names ["open", "high", "low", "close", "volume", "adjusted", "log_returns"])
# 可以用的数据结构有 dfWeek
# dfWeek: (data.frame with col names ["year.week", "log_returns"])
# 可以用的数据结构有 stock
# stock: the name the stock

# For Q1:.............
HistForLogReturns <- function(df, stock) {
  # Get the histogram of the log_returns of specific stock.
  #
  # Args:
  #   df: (data.frame with col names ["open", "high", "low", "close", "volume", "adjusted", "log_returns"])
  #   stock: the name the stock
  # 
  # Returns:
  #   null
  hist(df$log_returns, main = stock)
}
  




# ============================
# Part4: Server
# ============================
server <- function(input, output) {
  stock <- reactive({
    stock.name <- input$select1
    if (stock.name == "other") {
      stock.name <- input$otherStock
    }
    stock.name
  })
  
  df <- reactive({
    # if (!stock() %in% names(stocks_env) && stock() != ".getSymbols"){
    #   getSymbols(Symbols = stock(), from = input$startDate, to = input$endDate, env = stocks_env)
    # }
    getSymbols(Symbols = stock(), from = input$startDate, to = input$endDate, env = stocks_env)
    
    df = data.frame(stocks_env[[stock()]])
    colnames(df) <- c("open", "high", "low", "close", "volume", "adjusted")
    df$log_returns = log(df$close / df$open)
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
  
  output$histLog <- renderPlot({
    HistForLogReturns(df(), stock())
  })
  
}


# getSymbols(Symbols = "tttt", from = start_date, to = end_date, env = stocks_env)
# try(getSymbols(Symbols = "tttt", from = start_date, to = end_date, env = stocks_env), TRUE)

shinyApp(ui = ui, server = server)