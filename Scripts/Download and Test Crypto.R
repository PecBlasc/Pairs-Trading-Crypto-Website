# -----------------------------------------------
# 1. Download and Testing Crypto
# Project: Pairs Trading Thesis
# -----------------------------------------------

#  Libraries
library(quantmod)
library(urca)
library(dplyr)
library(ggplot2)
library(reshape2)
library(ggfortify)
library(xts)

#Range for the last 10 years (using data from Yahoo Finance))
start_date <- Sys.Date() - 10 * 365
end_date <- Sys.Date()

#  Crypto List
crypto_ids <- c("BTC-USD", "ETH-USD", "SOL-USD", "DOGE-USD", "XRP-USD", "LTC-USD", "ADA-USD")

#  Download data from Yahoo Finance
crypto_data <- list()
for (id in crypto_ids) {
  try({
    getSymbols(id, src = "yahoo", from = start_date, to = end_date, auto.assign = TRUE)
    crypto_data[[id]] <- Cl(get(id))  # Only Closed Prices
    message("Dati scaricati: ", id)
  }, silent = TRUE)
}

# Check the list of crypto downloadse
cat("Crypto disponibili: ", length(crypto_data), "\n")
print(names(crypto_data))

# Check available combinations (at least 2 cryptos)
if (length(crypto_data) >= 2) {
  all_pairs <- combn(names(crypto_data), 2, simplify = FALSE)
} else {
  stop("Less than 2 cryptocurrencies downloaded successfully.")
}

#  Results
results <- data.frame(Crypto1 = character(),
                      Crypto2 = character(),
                      ADF_Statistic = numeric(),
                      P_Value_Est = numeric(),
                      Cointegrated = logical(),
                      stringsAsFactors = FALSE)

#  Test cointegration function
test_cointegration <- function(x, y) {
  df <- na.omit(merge(x, y))
  if (nrow(df) < 250) return(NULL)  # At least 250 observations
  log_x <- log(df[, 1])
  log_y <- log(df[, 2])
  model <- lm(log_x ~ log_y)
  spread <- residuals(model)
  adf <- ur.df(spread, type = "none")
  stat <- adf@teststat[1]
  crit <- adf@cval[1, ]
  
  # Estimated p-value
  if (stat < crit["1pct"]) {
    p_val <- 0.01
  } else if (stat < crit["5pct"]) {
    p_val <- 0.05
  } else if (stat < crit["10pct"]) {
    p_val <- 0.10
  } else {
    p_val <- 1
  }
  
  is_cointegrated <- p_val <= 0.05
  return(c(stat, p_val, is_cointegrated))
}

#  Testing all pairs
for (pair in all_pairs) {
  x <- crypto_data[[pair[1]]]
  y <- crypto_data[[pair[2]]]
  res <- test_cointegration(x, y)
  if (!is.null(res)) {
    results <- rbind(results, data.frame(
      Crypto1 = pair[1],
      Crypto2 = pair[2],
      ADF_Statistic = round(res[1], 3),
      P_Value_Est = res[2],
      Cointegrated = as.logical(res[3])
    ))
  }
}

#  Results
results <- arrange(results, P_Value_Est, ADF_Statistic)
print(results)


# Create labels for couples
results$Pair <- paste(results$Crypto1, results$Crypto2, sep = " - ")

# Sort by ADF
results_plot <- results %>%
  arrange(ADF_Statistic) %>%
  mutate(Pair = factor(Pair, levels = Pair))  # Sort by factors

# Plot
ggplot(results_plot, aes(x = Pair, y = ADF_Statistic, fill = Cointegrated)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Crypto Pairs ADF Statistic",
       y = "ADF value", x = "Pairs") +
  scale_fill_manual(values = c("TRUE" = "forestgreen", "FALSE" = "grey")) +
  theme_minimal(base_size = 12)

# Create empty boolean array
crypto_names <- unique(c(results$Crypto1, results$Crypto2))
coin_matrix <- matrix(FALSE, nrow = length(crypto_names), ncol = length(crypto_names),
                      dimnames = list(crypto_names, crypto_names))

# Fill the matrix with cointegration TRUE/FALSE values
for (i in 1:nrow(results)) {
  c1 <- results$Crypto1[i]
  c2 <- results$Crypto2[i]
  cointegrated <- results$Cointegrated[i]
  coin_matrix[c1, c2] <- cointegrated
  coin_matrix[c2, c1] <- cointegrated 
}

# ggplot
coin_melt <- melt(coin_matrix)
colnames(coin_melt) <- c("Crypto1", "Crypto2", "Cointegrated")

# Heatmap
ggplot(coin_melt, aes(x = Crypto1, y = Crypto2, fill = Cointegrated)) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("TRUE" = "forestgreen", "FALSE" = "grey90")) +
  labs(title = "Crypto Pairs Cointagration",
       x = "Crypto x", y = "Crypto y", fill = "Cointegrated") +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank())

# One Pair Example
# Extract closing prices
eth <- crypto_data[["ETH-USD"]]
doge <- crypto_data[["DOGE-USD"]]

# Align time and remove any NAs
prices_aligned <- na.omit(merge(eth, doge))

# Calculate log-prices
log_prices <- log(prices_aligned)

# Rename columns for clarity
colnames(log_prices) <- c("ETH", "DOGE")

# plot
autoplot(log_prices) +
  ggtitle("Log-Prices: ETH/USD vs DOGE/USD") +
  xlab("Data") +
  ylab("Log Price") +
  theme_minimal()

# Plot residuals
plot(index(spread), spread, type = "l", main = "Regression Residuals (Spread)",
     ylab = "Spread", xlab = "Date", col = "steelblue")
abline(h = mean(spread), col = "darkred", lty = 2)

summary(ur.df(spread, type = "none"))

