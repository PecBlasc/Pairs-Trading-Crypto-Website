# -----------------------------------------------
# 4. Complete Optimization and Testing 
# Project: Pairs Trading Thesis
# -----------------------------------------------

library(PerformanceAnalytics)

# Parameters to test
entry_thresholds <- c(1.0, 1.5, 2.0)
exit_thresholds  <- c(0, 0.25, 0.5)

# Table for the best results of each pair
optimized_results <- data.frame(
  Crypto1 = character(),
  Crypto2 = character(),
  Entry = numeric(),
  Exit = numeric(),
  Sharpe = numeric(),
  Return = numeric(),
  Drawdown = numeric(),
  stringsAsFactors = FALSE
)

# Optimization loop
for (i in 1:nrow(results)) {
  if (results$Cointegrated[i]) {
    asset1 <- results$Crypto1[i]
    asset2 <- results$Crypto2[i]
    
    x <- crypto_data[[asset1]]
    y <- crypto_data[[asset2]]
    df <- na.omit(merge(x, y))
    if (nrow(df) < 250) next
    
    log_x <- log(df[, 1])
    log_y <- log(df[, 2])
    model <- lm(log_x ~ log_y)
    spread <- residuals(model)
    spread_ret <- diff(log_x - coef(model)[2] * log_y)
    
    best_sharpe <- -Inf
    best_config <- NULL
    
    for (entry in entry_thresholds) {
      for (exit in exit_thresholds) {
        zscore <- (spread - mean(spread)) / sd(spread)
        pos <- rep(0, length(zscore))
        
        for (j in 2:length(zscore)) {
          if (zscore[j - 1] > entry) {
            pos[j] <- -1
          } else if (zscore[j - 1] < -entry) {
            pos[j] <- 1
          } else if (abs(zscore[j - 1]) < exit) {
            pos[j] <- 0
          } else {
            pos[j] <- pos[j - 1]
          }
        }
        
        positions <- xts(pos, order.by = index(zscore))
        strategy_ret <- lag(positions) * spread_ret
        strategy_ret <- na.omit(strategy_ret)
        
        sharpe <- as.numeric(SharpeRatio.annualized(strategy_ret))
        ret <- as.numeric(Return.cumulative(strategy_ret)) * 100  # ✔️ correct return
        draw <- maxDrawdown(strategy_ret)
        
        if (!is.na(sharpe) && sharpe > best_sharpe) {
          best_sharpe <- sharpe
          best_config <- data.frame(
            Crypto1 = asset1,
            Crypto2 = asset2,
            Entry = entry,
            Exit = exit,
            Sharpe = round(sharpe, 3),
            Return = round(ret, 2),
            Drawdown = round(draw, 4)
          )
        }
      }
    }
    
    if (!is.null(best_config)) {
      optimized_results <- rbind(optimized_results, best_config)
    }
  }
}

# Sort and show the final ranking
optimized_results <- optimized_results[order(-optimized_results$Sharpe), ]
print(optimized_results)

for (i in 1:nrow(optimized_results)) {
  asset1 <- optimized_results$Crypto1[i]
  asset2 <- optimized_results$Crypto2[i]
  entry <- optimized_results$Entry[i]
  exit  <- optimized_results$Exit[i]
  
  x <- crypto_data[[asset1]]
  y <- crypto_data[[asset2]]
  df <- na.omit(merge(x, y))
  if (nrow(df) < 250) next
  
  log_x <- log(df[, 1])
  log_y <- log(df[, 2])
  model <- lm(log_x ~ log_y)
  spread <- residuals(model)
  spread_ret <- diff(log_x - coef(model)[2] * log_y)
  
  zscore <- (spread - mean(spread)) / sd(spread)
  pos <- rep(0, length(zscore))
  
  for (j in 2:length(zscore)) {
    if (zscore[j - 1] > entry) {
      pos[j] <- -1
    } else if (zscore[j - 1] < -entry) {
      pos[j] <- 1
    } else if (abs(zscore[j - 1]) < exit) {
      pos[j] <- 0
    } else {
      pos[j] <- pos[j - 1]
    }
  }
  
  positions <- xts(pos, order.by = index(zscore))
  strategy_ret <- lag(positions) * spread_ret
  strategy_ret <- na.omit(strategy_ret)
  
  # Plot with pair name in the title
  charts.PerformanceSummary(strategy_ret,
                            main = paste("Optimized Pairs Trading Strategy:", asset1, "/", asset2))
  
  readline("Press ENTER to continue...")
}
