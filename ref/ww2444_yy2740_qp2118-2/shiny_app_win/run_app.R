list.of.packages <- c("ggplot2", "shinythemes",'forecast','randtests','plotly','httr','jsonlite')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(ggplot2)
library(shinythemes)
library(forecast)
library(randtests)
library(plotly)
library(httr)
library(jsonlite)
# here are some elements I want to include as a block
read_to_mydata <- function(symbol_name){
  file <- file.path('data',symbol_name)
  my.data <- read.csv(file)
  return(my.data)
}
getCIofMean <- function(alpha,my.data){
  
  x <- my.data
  df <- length(x) - 1
  me <- -1 * qt(alpha/2,df)*sd(x)/sqrt(length(x))
  lo <- round((mean(x) - me),digits = 8)
  up <- round((mean(x) + me),digits = 8)
  object <- c (lo,up)
  
  return(object)
}
getCIofVar <- function(alpha,my.data){
  
  df <- length(my.data)-1
  conf.level <- 1-alpha
  
  chilower <- qchisq((1 - conf.level)/2, df)
  chiupper <- qchisq((1 - conf.level)/2, df, lower.tail = FALSE)
  
  v <- var(my.data)
  object <- c(round(df * v/chiupper,digits = 8), round(df * v/chilower,digits=8))
  
  return(object)
}
two_stocks_regression <- function(x, y){
  # Estimated Simple Regression Equation
  # !!! Need to Print Out the Simple Regression Equation
  fit <- lm(y$log_returns ~ x$log_returns)
  res <- resid(fit)
  old.par <-par(mfrow = c(1,2))
  qqline(x$log_returns,y$log_returns)
  # attributes(fit)
  #plot(fit)
  # Simple Scatterplot
  plot(x$log_returns, y$log_return, main="Scatterplot Example", 
       xlab="log_return X ", ylab="log_return Y ", pch=19)
  # Add fit lines
  abline(lm(y$log_return~x$log_returns), col="red") # regression line (y~x) 
  plot(x$log_returns, res, ylab="Residuals",xlab="log_return X ", main = "Residual Plot")
  abline(0,0,col="red")
  par(old.par)
}
two_stocks_regression_sum <- function(x, y){
  # Estimated Simple Regression Equation
  # !!! Need to Print Out the Simple Regression Equation
  fit <- lm(y$log_returns ~ x$log_returns)
  return(summary(fit))
}
tb.data <- read_to_mydata('Tbond_log_return.csv')
sp.data <- read_to_mydata('11.S&P.500.(^GSPC)_log_return.csv')
ttest <- function(my.data1,my.data2) {
  
  t.test(my.data1$log_returns,my.data2$log_returns)
  
}
getBeta <- function(my.data){

  x<-sp.data$log_returns-tb.data$log_returns
  y<-my.data$log_returns-tb.data$log_returns
  fit <- lm(y~x)
  res = resid(fit)
  # Simple Scatterplot
  old.par<-par(mfrow=c(1,2))
  plot(x, y, main="Scatterplot Example", 
       xlab="Market premium ", ylab="Stock Excess Return ", pch=19)
  # Add fit lines
  abline(lm(y~x), col="red") # regression line (y~x) 
  plot(x, res, ylab="Residuals",xlab="Market premium ", main = "Residual Plot")
  abline(0,0,col="red")
  par(old.par)
}
getBeta_sum<- function(my.data){
  
  x<-sp.data$log_returns-tb.data$log_returns
  y<-my.data$log_returns-tb.data$log_returns
  fit <- lm(y~x)
  print(paste('Intercept is',round(fit$coefficients[1],digits=6)))
  print(paste('Beta is',round(fit$coefficients[2],digits = 6)))
  return(summary(fit))
}

datamatrix1 <- function(){
  # Covariance Matrix
  matrix= data.frame( AAPL = read_to_mydata("1.AAPL_log_return.csv")$log_returns,
                      GOOG = read_to_mydata("2.Alphabet.Inc_log_return.csv")$log_returns,
                      GS = read_to_mydata("3.GS_log_return.csv")$log_returns,
                      GSK = read_to_mydata("4.GSK_log_return.csv")$log_returns,
                      JNJ = read_to_mydata("5.Johnson.&.Johnson.(JNJ)_log_return.csv")$log_returns,
                      MS = read_to_mydata("6. MS_log_return.csv")$log_returns,
                      TSLA = read_to_mydata("7. Tesla Motors, Inc. (TSLA)_log_return.csv")$log_returns,
                      TM = read_to_mydata("8. Toyota Motor Corporation (TM)_log_return.csv")$log_returns,
                      WMT = read_to_mydata("9. Wal-Mart Stores Inc. (WMT)_log_return.csv")$log_returns,
                      EMR = read_to_mydata("10. Emerson Electric Co. (EMR)_log_return.csv")$log_returns)
  # variance-covariance matrix
  cov_matrix = cov(matrix, y = NULL, use = "everything",
                   method = c("pearson", "kendall", "spearman"))
  cov_matrix = round(cov_matrix, digits=6)
  print("Variance-Covariance Matrix")
  print(cov_matrix)
}
datamatrix2 <- function(){
  # Covariance Matrix
  matrix= data.frame( AAPL = read_to_mydata("1.AAPL_log_return.csv")$log_returns,
                      GOOG = read_to_mydata("2.Alphabet.Inc_log_return.csv")$log_returns,
                      GS = read_to_mydata("3.GS_log_return.csv")$log_returns,
                      GSK = read_to_mydata("4.GSK_log_return.csv")$log_returns,
                      JNJ = read_to_mydata("5.Johnson.&.Johnson.(JNJ)_log_return.csv")$log_returns,
                      MS = read_to_mydata("6. MS_log_return.csv")$log_returns,
                      TSLA = read_to_mydata("7. Tesla Motors, Inc. (TSLA)_log_return.csv")$log_returns,
                      TM = read_to_mydata("8. Toyota Motor Corporation (TM)_log_return.csv")$log_returns,
                      WMT = read_to_mydata("9. Wal-Mart Stores Inc. (WMT)_log_return.csv")$log_returns,
                      EMR = read_to_mydata("10. Emerson Electric Co. (EMR)_log_return.csv")$log_returns)
  # correlation coefficient matrix
  corr_coeff = cor(matrix, y = NULL, use = "everything",
                   method = c("pearson", "kendall", "spearman"))
  corr_coeff = round(corr_coeff, digits=4)
  print("Correlation Coefficient matrix")
  print(corr_coeff)
}
regression_over_time <- function(my.data){
  x = (1:1257)
  fit <- lm(my.data$log_returns ~ x)
  res = resid(fit)
  #plot(fit)
  # Simple Scatterplot
  old.par<-par(mfrow=c(1,2))
  plot(my.data$Date, my.data$log_return, main="Scatterplot Example", 
       xlab="Date ", ylab="log_return ", pch=19)
  # Add fit lines
  abline(lm(my.data$log_return~x), col="red") # regression line (y~x) 
  plot(my.data$Date, res, ylab="Residuals",xlab="Date ", main = "Residual Plot")
  abline(0,0,col="red")
  par(old.par)
}
regression_over_time_sum <- function(my.data){
  x = (1:1257)
  fit <- lm(my.data$log_returns ~ x)
  return(summary(fit))
}

arima_plot <- function(data){
#gtemp <- ts(data)
#par(mfrow = c(1,2))
#fit1 = Arima(gtemp, order = c(4,1,1), 
#             include.drift = T)
#future = forecast(fit1, h = 100)
#plot(future)
  y <- msts(data, seasonal.periods=c(7,365.25))
  fit <- tbats(y)
  fc <- forecast(fit)
  plot(fc)
}
arima_sum <- function(data){
  #gtemp <- ts(data)
  #par(mfrow = c(1,2))
  #fit1 = Arima(gtemp, order = c(4,1,1), 
  #             include.drift = T)
  #future = forecast(fit1, h = 100)
  #plot(future)
  y <- msts(data, seasonal.periods=c(7,365.25))
  fit <- tbats(y)
  fc <- forecast(fit)
  return(summary(fc))
}
#my.data = read_to_mydata("6. MS_log_return.csv")

two_CI_interval <- list(
  titlePanel("Statistics Anaylsis of two symbols"),
  
  sidebarLayout(
    
    sidebarPanel(
      h4("Select two symbols in the box and click update to see it's analysis."),
      selectInput( "my_select1", label = "Select a company",    choices = list("Apple Inc." = '1.AAPL_log_return.csv', 
                                                                               "Alphabet Inc" = '2.Alphabet.Inc_log_return.csv', 
                                                                               "Goldman Sachs" = '3.GS_log_return.csv',
                                                                               "GlaxoSmithKline plc" = '4.GSK_log_return.csv',
                                                                               "Johnson.&.Johnson" = '5.Johnson.&.Johnson.(JNJ)_log_return.csv',
                                                                               "Morgan Stanley" = '6. MS_log_return.csv',
                                                                               "Tesla Motors, Inc." = '7. Tesla Motors, Inc. (TSLA)_log_return.csv',
                                                                               "Toyota Motor Corporation" = '8. Toyota Motor Corporation (TM)_log_return.csv',
                                                                               "Wal-Mart Stores Inc." = '9. Wal-Mart Stores Inc. (WMT)_log_return.csv',
                                                                               "Emerson Electric Co." = '10. Emerson Electric Co. (EMR)_log_return.csv'), 
                   selected = "Apple Inc."),
      selectInput("my_select2", "Select the other company",  choices = list("Apple Inc." = '1.AAPL_log_return.csv', 
                                                                            "Alphabet Inc" = '2.Alphabet.Inc_log_return.csv', 
                                                                            "Goldman Sachs" = '3.GS_log_return.csv',
                                                                            "GlaxoSmithKline plc" = '4.GSK_log_return.csv',
                                                                            "Johnson.&.Johnson" = '5.Johnson.&.Johnson.(JNJ)_log_return.csv',
                                                                            "Morgan Stanley" = '6. MS_log_return.csv',
                                                                            "Tesla Motors, Inc." = '7. Tesla Motors, Inc. (TSLA)_log_return.csv',
                                                                            "Toyota Motor Corporation" = '8. Toyota Motor Corporation (TM)_log_return.csv',
                                                                            "Wal-Mart Stores Inc." = '9. Wal-Mart Stores Inc. (WMT)_log_return.csv',
                                                                            "Emerson Electric Co." = '10. Emerson Electric Co. (EMR)_log_return.csv'), 
                  selected = "Alphabet Inc"),
      actionButton(inputId = "update_button_2", "update")),
    
    mainPanel(
      tabsetPanel(
        tabPanel(title="T Test", verbatimTextOutput("ci")), 
        tabPanel("Two Stock Regression",plotOutput('two_stocks_regression'),verbatimTextOutput("two_stock_reg_sum"))
      ))
  ))

  

shinysidebarlayout <-   list(
  titlePanel("Statistics Anaylsis of One Symbol"),
  
  sidebarLayout(
    
    sidebarPanel(
      h4("Select a symbol in the box and click update to see it's analysis.",colors='b'),
      selectInput("select_1", label = h3("Select box"), 
                  choices = list("Apple Inc." = '1.AAPL_log_return.csv', 
                                 "Alphabet Inc" = '2.Alphabet.Inc_log_return.csv', 
                                 "Goldman Sachs" = '3.GS_log_return.csv',
                                 "GlaxoSmithKline plc" = '4.GSK_log_return.csv',
                                 "Johnson.&.Johnson" = '5.Johnson.&.Johnson.(JNJ)_log_return.csv',
                                 "Morgan Stanley" = '6. MS_log_return.csv',
                                 "Tesla Motors, Inc." = '7. Tesla Motors, Inc. (TSLA)_log_return.csv',
                                 "Toyota Motor Corporation" = '8. Toyota Motor Corporation (TM)_log_return.csv',
                                 "Wal-Mart Stores Inc." = '9. Wal-Mart Stores Inc. (WMT)_log_return.csv',
                                 "Emerson Electric Co." = '10. Emerson Electric Co. (EMR)_log_return.csv'), 
                  selected = "Apple Inc."),
      actionButton(inputId = "update_button", "update")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(title="Hist of Log Return",plotOutput("hist_log")),
        tabPanel(title="QQ_Plot",plotOutput("qq_fig"),verbatimTextOutput('randomness')), 
        tabPanel("Confidence Interval",  
                 sliderInput(inputId = "alpha",label = "Significance (alpha)",
                                                     value = 0.95,min=0,max=1),
                 textOutput('label_CI_1'),
                 verbatimTextOutput('mean_CI'),
                 textOutput('label_CI_2'),
                 verbatimTextOutput('var_CI')), 
        tabPanel(title="Regression on Time",
                 plotOutput("regression_on_time"),
                 verbatimTextOutput('summary_of_reg')),
        tabPanel(title="ARIMA",plotOutput('arima_fig'),verbatimTextOutput('arima_sum')),
        tabPanel("CAPM",plotOutput('capm'),verbatimTextOutput('getBeta_sum'))
        ))
))
multi <- list(
  titlePanel("Statistics Anaylsis of multiple symbols"),
  tabsetPanel(
    tabPanel(title='Covariance Matrix',verbatimTextOutput('coverance_matrix')),
    tabPanel(title='Correlation Coefficient Matrix',verbatimTextOutput('coeff_matrix'))
  )
  
)
# user interface
ui <- fluidPage(
  theme = shinytheme('lumen'),
  
  htmlTemplate("..\\shiny_app\\index.html",
               shinycontrols =  two_CI_interval,
               shinysidebar = shinysidebarlayout,
               shinymulti = multi
  )
  
  
)
server <- function(input, output, session) {
  my.data <- reactiveValues(data = read_to_mydata('1.AAPL_log_return.csv'))
  my.data1 <- reactiveValues(data = read_to_mydata('1.AAPL_log_return.csv'))
  my.data2 <- reactiveValues(data = read_to_mydata('2.Alphabet.Inc_log_return.csv'))
  #reg_fit <-  reactiveValues(data = regression_over_time({read_to_mydata('1.AAPL_log_return.csv')}))
  observeEvent(input$update_button,{my.data$data <- read_to_mydata(input$select_1)})
  #observeEvent(input$update_button,{reg_fit$data <- regression_over_time({read_to_mydata(input$select_1)})})
  observeEvent(input$update_button_2,{my.data1$data <- read_to_mydata(input$my_select1)})
  observeEvent(input$update_button_2,{my.data2$data <- read_to_mydata(input$my_select2)})
  output$hist_log <- renderPlot({
    hist(my.data$data$log_returns, freq = FALSE, main = "Histogram of log-return\n during 10/010/2011-10/07/2016", xlab = "Log-return", ylab = "Probability (%)")
  })
  output$qq_fig <- renderPlot({
    qqnorm(my.data$data$log_returns)
    qqline(my.data$data$log_returns)
  })
  output$label_CI_1 <- renderText({
    'confidence Interval of Mean'
  })
  output$label_CI_2 <- renderText({
    'confidence Interval of Variance'
  })
  output$mean_CI <- renderPrint({
    paste('[',getCIofMean(input$alpha,my.data$data$log_returns)[1],',',getCIofMean(input$alpha,my.data$data$log_returns)[2],']')
  })
  output$var_CI <- renderPrint({
    paste('[',getCIofVar(input$alpha,my.data$data$log_returns)[1],',',getCIofVar(input$alpha,my.data$data$log_returns)[2],']')
  })
  output$regression_on_time <- renderPlot({
    regression_over_time(my.data1$data)
  })
  output$summary_of_reg <- renderPrint({regression_over_time_sum(my.data$data)})
  output$arima_fig <- renderPlot({
    arima_plot(my.data$data$log_returns)
  })
  output$arima_sum <- renderPrint({
    arima_sum(my.data$data$log_returns)
  })
  output$capm <- renderPlot({
    getBeta(my.data$data)
  })  
  output$getBeta_sum <- renderPrint({
    getBeta_sum(my.data$data)
  })
 
  output$randomness <- renderPrint({
    runs.test(my.data$data$log_returns)
  })
  output$two_stocks_regression <- renderPlot({
    two_stocks_regression(my.data1$data,my.data2$data)
  })
  output$two_stock_reg_sum<- renderPrint({
    two_stocks_regression_sum(my.data1$data,my.data2$data)
  })
  output$ci <- renderPrint({
    ttest(my.data1$data,my.data2$data)
  })
  output$coverance_matrix <- renderPrint({datamatrix1()})
  output$coeff_matrix <- renderPrint({datamatrix2()})
}

shinyApp(ui = ui, server = server)