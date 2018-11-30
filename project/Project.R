# ============================
# Part1: Initialization
# ============================

packages.Needed <- c("shiny", "quantmod", "ggplot2", "dplyr", "shinythemes", "httr")
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

firstPart <- sidebarLayout(
    sidebarPanel(
      # h4("One Symbol Analysis"),
      wellPanel(
        dateInput('startDate1',
                  label = 'Start date: yyyy-mm-dd',
                  value = start_date
        ),
        dateInput('endDate1',
                  label = 'End date: yyyy-mm-dd',
                  value = end_date
        ),
        
        selectInput(
          inputId = "select11",
          label = "Stock Symbol",
          choices = stocks,
          selected = "AAPL"
        ),
        conditionalPanel(
          condition = "input.select11 == 'other'",
          textInput(
            inputId = "otherStock11",
            label = "Please enter the Symbol"
          )
        )
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = "Hist of Log Return",
          plotOutput("histLog1")
        ),
        tabPanel(
          title = "QQ Plot",
          plotOutput("qqPlot1")
        )
      )
    )
  )

secondPart <- sidebarLayout(
  sidebarPanel(
    # h4("Two Symbols Analysis"),
    wellPanel(
      dateInput('startDate2',
                label = 'Start date: yyyy-mm-dd',
                value = start_date
      ),
      dateInput('endDate2',
                label = 'End date: yyyy-mm-dd',
                value = end_date
      ),
      
      selectInput(
        inputId = "select21",
        label = "Stock Symbol 1",
        choices = stocks,
        selected = "AAPL"
      ),
      conditionalPanel(
        condition = "input.select21 == 'other'",
        textInput(
          inputId = "otherStock21",
          label = "Please enter the Symbol"
        )
      ),
      selectInput(
        inputId = "select22",
        label = "Stock Symbol 2",
        choices = stocks,
        selected = "AAPL"
      ),
      conditionalPanel(
        condition = "input.select22 == 'other'",
        textInput(
          inputId = "otherStock22",
          label = "Please enter the Symbol"
        )
      )
      
    )
  ),
  mainPanel(
    tabsetPanel(
      tabPanel(
        title = "Hist of Log Return",
        plotOutput("histLog2")
      ),
      tabPanel(
        title = "QQ Plot",
        plotOutput("qqPlot2")
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
  
  output$histLog1 <- renderPlot({
    HistForLogReturns(df(), stock())
  })
  
}


# getSymbols(Symbols = "tttt", from = start_date, to = end_date, env = stocks_env)
# try(getSymbols(Symbols = "tttt", from = start_date, to = end_date, env = stocks_env), TRUE)

shinyApp(ui = ui, server = server)