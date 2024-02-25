# Install and Load Required Libraries
if (!require("GA")) install.packages("GA")
if (!require("quantmod")) install.packages("quantmod")
library(GA)
library(quantmod)

# Define the optimizePortfolio Function
optimizePortfolio <- function(StockData, risk_aversion_factors = c(0.2, 0.5, 0.8), popSize_values = c(50, 100), maxiter_values = c(100, 200), run_values = c(50, 100)) {
  results <- list()
  
  for (risk_aversion_factor in risk_aversion_factors) {
    for (popSize in popSize_values) {
      for (maxiter in maxiter_values) {
        for (run in run_values) {
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
          
          result_key <- paste0("risk_", risk_aversion_factor, "_popSize_", popSize, "_maxiter_", maxiter, "_run_", run)
          results[[result_key]] <- ga_result
        }
      }
    }
  }
  
  return(results)
}

# Function to Calculate Returns Using Weights
calculateReturns <- function(StockData, weights) {
  # Combine stock data into a single matrix
  stock_returns <- do.call(merge, lapply(StockData, function(stock) ROC(Ad(stock), type = "discrete", na.pad = FALSE)))
  
  # Convert to matrix if not already
  if (!is.matrix(stock_returns)) {
    stock_returns <- as.matrix(stock_returns)
  }
  
  # Ensure weights is a numeric vector
  weights <- as.numeric(weights)
  
  # Convert weights to a column matrix (explicitly specify its dimensions)
  weights <- matrix(weights, nrow = length(weights), ncol = 1)
  
  # Check for NA values
  if (any(is.na(stock_returns)) || any(is.na(weights))) {
    stop("NA values found in stock_returns or weights.")
  }
  
  # Matrix multiplication
  weighted_returns <- stock_returns %*% weights
  
  # Sum across rows to get portfolio returns for each time period
  rowSums(weighted_returns, na.rm = TRUE)
}

# Fetch Stock Data
myStocks <- c("AAPL", "JPM", "PFE", "AMZN", "XOM", "SO", "GE", "BHP", "PLD", "VZ")
getSymbols(myStocks, from = "2020-01-01", to = "2022-12-31")
Training_StockData <- lapply(myStocks, function(stock) get(stock)["2020"])
Testing_StockData <- lapply(myStocks, function(stock) get(stock)["2021"])

# Run Optimization
risk_aversion_factors <- c(0.2, 0.5, 0.8)
popSize_values <- c(50, 100)
maxiter_values <- c(100, 200)
run_values <- c(50, 100)

optimized_portfolios <- optimizePortfolio(Training_StockData, risk_aversion_factors, popSize_values, maxiter_values, run_values)

# Evaluate on Testing Data for each optimized portfolio
total_returns <- list()
for (risk_aversion_factor in risk_aversion_factors) {
  for (popSize in popSize_values) {
    for (maxiter in maxiter_values) {
      for (run in run_values) {
        result_key <- paste0("risk_", risk_aversion_factor, "_popSize_", popSize, "_maxiter_", maxiter, "_run_", run)
        optimized_weights <- optimized_portfolios[[result_key]]@solution
        testing_returns <- calculateReturns(Testing_StockData, optimized_weights)
        total_returns[[result_key]] <- sum(testing_returns, na.rm = TRUE)
      }
    }
  }
}

# Normalize and Print Solutions
normalizeSolution <- function(solution) {
  solution / sum(solution)
}

normalized_solutions <- lapply(optimized_portfolios, function(portfolio) normalizeSolution(portfolio@solution))

# Save Plots
savePlot <- function(ga_result, filePath) {
  png(file = filePath)
  plot(ga_result)
  dev.off()
}

pathPrefix <- "C:/Users/riley/Documents/GitHub/Postgraduate-Work-/Semester 2/AI for Finance/Assignment 1/"
for (result_key in names(optimized_portfolios)) {
  savePlot(optimized_portfolios[[result_key]], paste0(pathPrefix, paste0("GA_plot_", result_key, ".png")))
}

cat("Total Returns on Testing Data (2022):\n")
print(total_returns)
