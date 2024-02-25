# Install and Load Required Libraries
if (!require("GA")) install.packages("GA")
if (!require("quantmod")) install.packages("quantmod")
library(GA)
library(quantmod)

# Define the function for asset selection using GA
selectAssetsGA <- function(StockData, num_assets_to_select, popSize = 50, maxiter = 100, run = 50) {
  # Fitness function for asset selection
  fitness_function <- function(indices) {
    returns <- calculateReturns(StockData[indices], rep(1, length(indices)))
    -sum(returns)  # Minimize negative returns
  }
  
  # Run GA
  ga_result <- ga(
    type = "permutation",
    fitness = fitness_function,
    lower = 1,
    upper = length(StockData),
    popSize = popSize,
    maxiter = maxiter,
    run = run
  )
  
  # Extract selected asset indices
  selected_indices <- ga_result@solution
  
  return(selected_indices)
}

# Modify optimizePortfolio function to accept selected assets
optimizePortfolio <- function(SelectedStockData, risk_aversion_factor = 0.5, popSize = 50, maxiter = 100, run = 50) {
  # Compute Returns Helper Function
  computeReturns <- function(StockData) {
    Returns <- lapply(StockData, function(stockData) {
      ROC(Ad(stockData), type = "discrete", na.pad = FALSE)
    })
    Returns <- do.call(merge, Returns)
  }

  # Calculate Average Returns and Covariance
  ReturnsDF <- computeReturns(SelectedStockData)
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
    lower = rep(0, length(SelectedStockData)),
    upper = rep(1, length(SelectedStockData)),
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

# Run GA for asset selection
selected_indices <- selectAssetsGA(Training_StockData, num_assets_to_select = 5)

# Extract selected stock data
selected_assets <- Training_StockData[selected_indices]

# Run portfolio optimization on selected assets
optimized_portfolio <- optimizePortfolio(selected_assets)

# Evaluate on Testing Data
testing_returns <- calculateReturns(Testing_StockData, optimized_portfolio@solution)
total_return <- sum(testing_returns, na.rm = TRUE)

# Normalize and Print Solutions
normalizeSolution <- function(solution) {
  solution / sum(solution)
}

# Save Plots
savePlot <- function(ga_result, filePath) {
  png(file = filePath)
  plot(ga_result)
  dev.off()
}

pathPrefix <- "C:/Users/riley/Documents/GitHub/Postgraduate-Work-/Semester 2/AI for Finance/Assignment 1/"
savePlot(optimized_portfolio, paste0(pathPrefix, "GA_plot_training.png"))

# Comparison with previous approaches
# Merging the list of xts objects into a single xts object
merged_testing_data <- do.call(merge, Testing_StockData)

# Calculating returns for each stock
stock_returns <- ROC(merged_testing_data, type = "discrete", na.pad = FALSE)

# Calculating the returns for the Evenly Weighted and Random Portfolios
num_stocks <- ncol(stock_returns)

# Evenly Weighted Portfolio
even_weights <- rep(1/num_stocks, num_stocks)
even_portfolio_return <- sum(even_weights * colMeans(stock_returns, na.rm = TRUE))

# Random Portfolio
set.seed(123) # for reproducibility
random_weights <- runif(num_stocks)
random_weights <- random_weights / sum(random_weights)
random_portfolio_return <- sum(random_weights * colMeans(stock_returns, na.rm = TRUE))

# Compare Returns
cat("GA-selected Portfolio Return:", total_return, "\n")
cat("Evenly Weighted Portfolio Return:", even_portfolio_return, "\n")
cat("Random Portfolio Return:", random_portfolio_return, "\n")
