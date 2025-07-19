# -----------------------------------------------
# 3. Example of Optimization for one pair (Facultative)
# Project: Pairs Trading Thesis
# -----------------------------------------------

library(PerformanceAnalytics)

# Select a pair
asset1 <- "ETH-USD"
asset2 <- "ADA-USD"

# Data
x <- crypto_data[[asset1]]
y <- crypto_data[[asset2]]
df <- na.omit(merge(x, y))
log_x <- log(df[,1])
log_y <- log(df[,2])
model <- lm(log_x ~ log_y)
spread <- residuals(model)
spread_ret <- diff(log_x - coef(model)[2] * log_y)

# Parameters to test
entry_thresholds <- c(1.0, 1.5, 2.0)
exit_thresholds  <- c(0, 0.25, 0.5)

# Initialize results table
opt_results <- data.frame(
  Entry = numeric(),
  Exit = numeric(),
  Sharpe = numeric(),
  Return = numeric(),
  Drawdown = numeric()
)

# Loop over combinations
for (entry in entry_thresholds) {
  for (exit in exit_thresholds) {
    zscore <- (spread - mean(spread)) / sd(spread)
    positions_vec <- rep(0, length(zscore))
    
    for (i in 2:length(zscore)) {
      if (zscore[i - 1] > entry) {
        positions_vec[i] <- -1
      } else if (zscore[i - 1] < -entry) {
        positions_vec[i] <- 1
      } else if (abs(zscore[i - 1]) < exit) {
        positions_vec[i] <- 0
      } else {
        positions_vec[i] <- positions_vec[i - 1]
      }
    }
    
    positions <- xts(positions_vec, order.by = index(zscore))
    strategy_ret <- lag(positions) * spread_ret
    strategy_ret <- na.omit(strategy_ret)
    
    sharpe <- as.numeric(SharpeRatio.annualized(strategy_ret))
    ret <- sum(strategy_ret) * 100
    draw <- maxDrawdown(strategy_ret)
    
    opt_results <- rbind(opt_results, data.frame(
      Entry = entry,
      Exit = exit,
      Sharpe = round(sharpe, 3),
      Return = round(ret, 2),
      Drawdown = round(draw, 4)
    ))
  }
}

# Show ordered results
opt_results <- opt_results[order(-opt_results$Sharpe), ]
print(opt_results)
