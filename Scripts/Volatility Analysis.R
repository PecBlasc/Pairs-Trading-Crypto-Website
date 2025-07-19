# -----------------------------------------------
# 5. GARCH Volatility Analysis – Top Optimized Pairs
# Project: Pairs Trading Thesis
# -----------------------------------------------

library(rugarch)
library(xts)
library(PerformanceAnalytics)
library(ggplot2)

# Results list
volatility_results <- list()

# Start loop over pairs
N <- nrow(optimized_results)

for (i in 1:N) {
  asset1 <- optimized_results$Crypto1[i]
  asset2 <- optimized_results$Crypto2[i]
  entry  <- optimized_results$Entry[i]
  exit   <- optimized_results$Exit[i]
  
  x <- crypto_data[[asset1]]
  y <- crypto_data[[asset2]]
  df <- na.omit(merge(x, y))
  
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
  strategy_ret <- na.omit(lag(positions) * spread_ret)
  
  # Indicators
  sd_ret <- if (length(strategy_ret) > 1) sd(strategy_ret) else NA
  realized_vol <- if (length(strategy_ret) > 1) sqrt(sum(strategy_ret^2)) else NA
  annual_vol <- if (!is.na(sd_ret)) sd_ret * sqrt(252) else NA
  mean_ret <- if (length(strategy_ret) > 1) mean(strategy_ret) else NA
  num_trades <- if (length(positions) > 1) sum(diff(positions) != 0, na.rm = TRUE) else 0
  status <- if (length(strategy_ret) < 2 || all(is.na(strategy_ret))) "Strategy Failed" else "OK"
  
  # List of saved results
  volatility_results[[i]] <- data.frame(
    Asset1 = asset1,
    Asset2 = asset2,
    Obs = length(strategy_ret),
    StdDev = sd_ret,
    RealizedVolatility = realized_vol,
    AnnualizedVolatility = annual_vol,
    MeanReturn = mean_ret,
    NumTrades = num_trades,
    Status = status
  )
  
  # ---- STRATEGY PLOT: z-score + signals ----
  plot_df <- data.frame(
    Date = index(zscore),
    ZScore = as.numeric(zscore),
    Position = as.numeric(positions)
  )
  plot_df$Signal <- c(0, diff(plot_df$Position))
  plot_df$SignalType <- ifelse(plot_df$Signal != 0,
                               ifelse(plot_df$Signal > 0, "Long", "Short"),
                               NA)
  
  # Create plot
  p <- ggplot(plot_df, aes(x = Date, y = ZScore)) +
    geom_line(color = "steelblue") +
    geom_hline(yintercept = c(entry, -entry), linetype = "dashed", color = "red") +
    geom_hline(yintercept = c(exit, -exit), linetype = "dashed", color = "orange") +
    geom_point(data = subset(plot_df, !is.na(SignalType)),
               aes(color = SignalType), size = 2, alpha = 0.8) +
    scale_color_manual(values = c("Long" = "green", "Short" = "purple")) +
    labs(
      title = paste("Z-Score & Trading Signals:", asset1, "/", asset2),
      subtitle = paste("Entry:", entry, "Exit:", exit),
      y = "Z-Score", x = "Date"
    ) +
    theme_minimal()
  
  # Display plot in console (or in RMarkdown)
  print(p)
  
  # If used in RMarkdown, you can also save it in a list:
  assign(paste0("plot_", asset1, "_", asset2), p)
}

volatility_df <- do.call(rbind, volatility_results)
print(volatility_df)
