# Install and Load Required Libraries
if(!require("GA")) install.packages("GA")
if(!require("quantmod")) install.packages("quantmod")
library(GA)
library(quantmod)

# Define the optimizePortfolio Function
optimizePortfolio <- function(StockData, risk_aversion_factor = 0.5, popSize = 50, maxiter = 100, run = 50) {
  # Compute Returns Helper Function
  computeReturns <- function(StockData) {
    Returns <- lapply(StockData, function(stockData) {
      ROC(Ad(stockData), type = "discrete", na.pad = FALSE)
    })
    Returns <- do.call(merge, Returns)
  }

  # Calculate Average Returns and Covariance
  ReturnsDF <- computeReturns(StockData)
  avgReturns <- colMeans(ReturnsDF, na.rm = TRUE)
  covariance <- cov(ReturnsDF)

  # Fitness Function
  fitness_function <- function(weights) {
    portfolio_return <- sum(weights * avgReturns)
    portfolio_variance <- t(weights) %*% covariance %*% weights
    fitness_value <- risk_aversion_factor * portfolio_variance - (1 - risk_aversion_factor) * portfolio_return
    fitness_value + 1000 * abs(sum(weights) - 1)
  }

  # Run GA
  ga_result <- ga(
    type = "real-valued",
    fitness = fitness_function,
    lower = rep(0, length(StockData)),
    upper = rep(1, length(StockData)),
    popSize = popSize,
    maxiter = maxiter,
    run = run
  )

  return(ga_result)
}

# Function to Calculate Returns Using Weights
calculateReturns <- function(StockData, weights) {
  # Combine stock data into a single matrix
  stock_returns <- do.call(merge, lapply(StockData, function(stock) ROC(Ad(stock), type = "discrete", na.pad = FALSE)))

  # Convert to matrix if not already
  if(!is.matrix(stock_returns)) {
    stock_returns <- as.matrix(stock_returns)
  }

  # Ensure weights is a numeric vector
  weights <- as.numeric(weights)

  # Convert weights to a column matrix (explicitly specify its dimensions)
  weights <- matrix(weights, nrow = length(weights), ncol = 1)

  # Check for NA values
  if(any(is.na(stock_returns)) || any(is.na(weights))) {
    stop("NA values found in stock_returns or weights.")
  }

  # Check and print the structure of stock_returns and weights
  str(stock_returns)
  str(weights)

  # Matrix multiplication
  weighted_returns <- stock_returns %*% weights

  # Sum across rows to get portfolio returns for each time period
  rowSums(weighted_returns, na.rm = TRUE)
}

# Fetch Stock Data
myStocks <- c("AAPL", "JPM", "PFE", "AMZN", "XOM", "SO", "GE", "BHP", "PLD", "VZ")
getSymbols(myStocks, from = "2017-01-01", to = "2022-12-31")
Training_StockData <- lapply(myStocks, function(stock) get(stock)["2017"])
Testing_StockData <- lapply(myStocks, function(stock) get(stock)["2022"])

# Run Optimization
optimized_portfolio <- optimizePortfolio(Training_StockData)
optimized_weights <- optimized_portfolio@solution

# Evaluate on Testing Data
testing_returns <- calculateReturns(Testing_StockData, optimized_weights)
total_return <- sum(testing_returns, na.rm = TRUE)
cat("Total Return on Testing Data (2022):", total_return, "\n")

# Normalize and Print Solutions
normalizeSolution <- function(solution) {
  solution / sum(solution)
}

training_best_solution <- normalizeSolution(optimized_portfolio@solution)
testing_best_solution <- normalizeSolution(optimizePortfolio(Testing_StockData)@solution)

print(training_best_solution)
print(testing_best_solution)

# Save Plots
savePlot <- function(ga_result, filePath) {
  png(file = filePath)
  plot(ga_result)
  dev.off()
}

pathPrefix <- "C:/Users/riley/Documents/GitHub/Postgraduate-Work-/Semester 2/AI for Finance/Assignment 1/"
savePlot(optimized_portfolio, paste0(pathPrefix, "GA_plot_training.png"))
savePlot(optimizePortfolio(Testing_StockData), paste0(pathPrefix, "GA_plot_testing.png"))
