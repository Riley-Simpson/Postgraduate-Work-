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
    return(fitness_value + 1000 * abs(sum(weights) - 1))
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
    stock_returns <- do.call(merge, lapply(StockData, function(stock) ROC(Ad(stock), type = "discrete", na.pad = FALSE)))
    if (!is.matrix(stock_returns)) {
        stock_returns <- as.matrix(stock_returns)
    }
    weights <- as.numeric(weights)
    weights <- matrix(weights, nrow = length(weights), ncol = 1)
    if (any(is.na(stock_returns)) || any(is.na(weights))) {
        stop("NA values found in stock_returns or weights.")
    }
    return(sum(weights * stock_returns))
}

# Define the function for asset selection using GA
selectAssetsGA <- function(StockData, num_assets_to_select, popSize = 50, maxiter = 100, run = 50) {
  # Fitness function for asset selection
  fitness_function <- function(indices) {
    selectedData <- StockData[indices]
    returns <- calculateReturns(selectedData, rep(1, length(indices)))
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

# Example Usage
# Assuming Training_StockData and Testing_StockData are defined and prepared properly
selected_indices <- selectAssetsGA(Training_StockData, num_assets_to_select = 5)
selected_assets <- Training_StockData[selected_indices]
optimized_portfolio <- optimizePortfolio(selected_assets)

# Evaluate on Testing Data
testing_returns <- calculateReturns(Testing_StockData, optimized_portfolio@solution)
total_return <- sum(testing_returns, na.rm = TRUE)

# Normalize and Print Solutions
normalizeSolution <- function(solution) {
  solution / sum(solution)
}

training_best_solution <- normalizeSolution(optimized_portfolio@solution)
testing_best_solution <- normalizeSolution(optimizePortfolio(Testing_StockData)@solution)

# Save Plots
savePlot <- function(ga_result, filePath) {
  png(file = filePath)
  plot(ga_result)
  dev.off()
}

pathPrefix <- "C:/Users/riley/Documents/GitHub/Postgraduate-Work-/Semester 2/AI for Finance/Assignment 1/"
savePlot(optimized_portfolio, paste0(pathPrefix, "GA_plot_training.png"))
savePlot(optimizePortfolio(Testing_StockData), paste0(pathPrefix, "GA_plot_testing.png"))

cat("Total Return on Testing Data (2022):", total_return, "\n")
