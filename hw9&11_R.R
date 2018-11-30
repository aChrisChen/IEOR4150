library(quantmod)
setwd("/Users/chenjipeng/Documents/Learn/Columbia/IEOR4150/Project/log_returns")


# homework 9
stocks = c("AAPL", "AMZN", "FB", "GOOG", "GOOGL", "MSFT", "MU", "NFLX", "NOW","NVDA")
bins = c(5, 10, round(sqrt(755)), 50, 100, 500 , 1000)
start_date = "2015-01-01"
end_date = "2018-01-01"


stocks_env = new.env()
getSymbols(Symbols = stocks, from = start_date, to = end_date, env = stocks_env)
stock_means = data.frame()
stock_vars = data.frame()
for (stock in stocks) {
  
  # ********************************
  # homework 9
  # ********************************
  df = data.frame(stocks_env[[stock]])
  colnames(df) <- c("open", "high", "low", "close", "volume", "adjusted")
  df$log_returns = log(df$close / df$open)
  write.csv(x = df["log_returns"], file = paste0(stock, ".csv"))
  
  cur_mean <- t(data.frame(apply(df, 2, mean)))
  row.names(cur_mean) <- stock
  stock_means = rbind(stock_means, cur_mean)
  
  cur_var <- t(data.frame(apply(df, 2, var)))
  row.names(cur_var) <- stock
  stock_vars = rbind(stock_vars, cur_var)
  
  # enter folder ./log_returns/hist/stock
  if(!dir.exists(paste0("hist/", stock))){
    dir.create(file.path("hist", stock), recursive = TRUE)
  }
  setwd(paste0("./hist/", stock))
  
  # enter folder ./log_returns/hist/stock/close_hist
  if(!dir.exists("./close_hist")) {
    dir.create(file.path("close_hist"), recursive = TRUE)
  }
  setwd("./close_hist")
  for (bin in bins) {
    jpeg(paste0(stock, "_", bin, ".jpg"))
    hist(df$close, breaks = bin - 1, main = paste0(stock," close price with bin = ", bin))
    dev.off()
  }
  setwd("..")
  
  # now at ./log_returns/hist/stock
  # enter folder ./log_returns/hist/stock/log_hist
  if(!dir.exists("./log_hist")) {
    dir.create(file.path("log_hist"), recursive = TRUE)
  }
  setwd("./log_hist")
  for (bin in bins) {
    jpeg(paste0(stock, "_", bin, ".jpg"))
    hist(df$log_returns, breaks = bin - 1, main = paste0(stock," log_retur with bin = ", bin))
    dev.off()
  }
  setwd("..")
  

  # finsh and return to the home directory
  setwd("..")
  setwd("..")
}

if(!dir.exists("./stats")) {
  dir.create(file.path("stats"), recursive = TRUE)
}
setwd("./stats")
write.csv(x = stock_means, file = "Means.csv")
write.csv(x = stock_vars, file = "Vars.csv")
setwd("..")


# ********************************
# homework 11
# ********************************
cal_break <- function(df) {
  row = dim(df)[1]
  cal_break = round(sqrt(row)) - 1
  return(cal_break)
}

if(!dir.exists("./hw11")) {
  dir.create(file.path("hw11"), recursive = TRUE)
}
setwd("./hw11")
# 1-(a)
df$year.week <- strftime(row.names(df), format = "%Y.%V")
df.week <- aggregate(df$log_returns, by = list(df$year.week), FUN = sum)
rownames(df.week) <- df.week[, 1]
colnames(df.week) <- c("year.week", "log_returns")


jpeg(paste0(stock, "_", "by_week", ".jpg"))
hist(df.week$log_returns, breaks = cal_break(df.week))
dev.off()

# 1-(b)
# by day
df$year.week <- strftime(row.names(df), format = "%Y.%V")

df1 <- df[1:(dim(df)[1]/2), ]
df1$aim = sqrt(5) * (df1$log_returns - mean(df1$log_returns))

jpeg(paste0(stock, "_fist_half_by_day", ".jpg"))
hist(df1$aim, breaks = cal_break(df1))
dev.off()

# by week
df2 <- df[(dim(df)[1]/2):(dim(df)[1]), ]
df2.week <- aggregate(df2$log_returns, by = list(df2$year.week), FUN = sum)
rownames(df2.week) <- df2.week[, 1]
colnames(df2.week) <- c("year.week", "log_returns")
df2.week$aim = df2.week$log_returns - mean(df2.week$log_returns)

jpeg(paste0(stock, "_second_half_by_week", ".jpg"))
hist(df2.week$aim, breaks = cal_break(df))
dev.off()

# 1-(c)

# CI for mean
n = dim(df)[1]
S = var(df$log_returns)
# t_0.025 with degree n-1
t = qt(1-0.025, n-1)
X_mean = mean(df$log_returns)
length = 2 * S * t / sqrt(n)
print(paste0("The 95% CI for mean is (", X_mean - length/2, ", ", X_mean + length/2, ")."))
# "The 95% CI is (0.00109843674582069, 0.00114803633495787)."

# CI for variance
chi_0.025_n_1 = qchisq(1 - 0.025, n-1)
chi_0.975_n_1 = qchisq(1 - 0.975, n-1)
print(paste0("The 95% CI for vairance is (", (n-1)*S/chi_0.025_n_1, ", ", (n-1)*S/chi_0.975_n_1, ")."))
# "The 95% CI for vairance is (0.000314579436720997, 0.000385004400782479)."


