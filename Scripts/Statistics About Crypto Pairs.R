# -----------------------------------------------
# 2. Statistics about Crypto Pairs
# Project: Pairs Trading Thesis
# -----------------------------------------------

library(PerformanceAnalytics)

# Strategy Parameters
entry_threshold <- 2
exit_threshold <- 0

# Initialize results table
performance_table <- data.frame(
  Crypto1 = character(),
  Crypto2 = character(),
  Sharpe_Ratio = numeric(),
  Total_Return = numeric(),
  Max_Drawdown = numeric(),
  stringsAsFactors = FALSE
)

# Backtest on cointegrated pairs
for (i in 1:nrow(results)) {
  if (results$Cointegrated[i] == TRUE) {
    pair <- results[i, ]
    asset1 <- pair$Crypto1
    asset2 <- pair$Crypto2
    
    x <- crypto_data[[asset1]]
    y <- crypto_data[[asset2]]
    df <- na.omit(merge(x, y))
    if (nrow(df) < 250) next
    
    # Spread and Z-score
    log_x <- log(df[, 1])
    log_y <- log(df[, 2])
    model <- lm(log_x ~ log_y)
    spread <- residuals(model)
    zscore <- (spread - mean(spread)) / sd(spread)
    
    # Strategy
    positions_vec <- rep(0, length(zscore))
    for (j in 2:length(zscore)) {
      if (zscore[j - 1] > entry_threshold) {
        positions_vec[j] <- -1
      } else if (zscore[j - 1] < -entry_threshold) {
        positions_vec[j] <- 1
      } else if (abs(zscore[j - 1]) < exit_threshold) {
        positions_vec[j] <- 0
      } else {
        positions_vec[j] <- positions_vec[j - 1]
      }
    }
    positions <- xts(positions_vec, order.by = index(zscore))
    
    spread_ret <- diff(log_x - coef(model)[2] * log_y)
    strategy_ret <- lag(positions) * spread_ret
    strategy_ret <- na.omit(strategy_ret)
    
    # Metrics Calculation
    sharpe <- as.numeric(SharpeRatio.annualized(strategy_ret))
    total_ret <- round(100 * sum(strategy_ret), 2)
    drawdown <- round(maxDrawdown(strategy_ret), 4)
    
    # Save
    performance_table <- rbind(performance_table, data.frame(
      Crypto1 = asset1,
      Crypto2 = asset2,
      Sharpe_Ratio = round(sharpe, 3),
      Total_Return = total_ret,
      Max_Drawdown = drawdown
    ))
    
    # Plot
    charts.PerformanceSummary(strategy_ret,
                              main = paste("Pairs Trading:", asset1, "/", asset2))
    
    readline("Press ENTER to continue...")
  }
}

# Final Ranking
performance_table <- performance_table[order(-performance_table$Sharpe_Ratio), ]
print(performance_table)
write.csv(performance_table, "Pairs_Trading_Results.csv", row.names = FALSE)



# Plot for one pair example
# Select the pair with complete historical series
z <- crypto_data[["ETH-USD"]]
w <- crypto_data[["ADA-USD"]]

# Align and clean the series
df_1 <- na.omit(merge(z, w))
log_z <- log(df_1[, 1])
log_w <- log(df_1[, 2])

# Linear regression
model_onepair <- lm(log_z ~ log_w)
spread_onepair <- residuals(model_onepair)

# Z-score calculation
zscore <- (spread_onepair - mean(spread_onepair)) / sd(spread_onepair)
zscore_xts <- xts(zscore, order.by = index(spread_onepair))  # Use correct index

# Plot
plot(zscore_xts, main = "Z-Score of the Spread (ETH-USD / ADA-USD)", 
     ylab = "Z", col = "steelblue")
abline(h = c(-2, 0, 2), col = c("red", "black", "red"), lty = c(2, 1, 2))
