# Install required libraries 
install.packages("GA")
install.packages("quantmod")

# Load required libraries
library(GA)
library(quantmod)

# Define the optimizePortfolio function
optimizePortfolio <- function(StockData, risk_aversion_factor = 0.5, popSize = 50, maxiter = 100, run = 50) {
  # Function to compute Returns
  computeReturns <- function(StockData) {
    Returns <- lapply(ls(StockData), function(sym) {
      stockData <- get(sym, envir = StockData)
      ROC(Ad(stockData), type = "discrete", na.pad = FALSE)
    })
    Returns <- Returns[sapply(Returns, function(x) !is.null(dim(x)))]
    do.call(merge, Returns)
  }

  # Calculate average returns and covariance
  ReturnsDF <- computeReturns(StockData)
  avgReturns <- colMeans(ReturnsDF, na.rm = TRUE)
  covariance <- cov(ReturnsDF)

  # Fitness function
  fitness_function <- function(weights, covariance, avgReturns) {
    portfolio_return <- sum(weights * avgReturns)
    portfolio_variance <- t(weights) %*% covariance %*% weights
    variance_value <- as.numeric(portfolio_variance)
    penalty = 1000 * abs(sum(weights) - 1)
    fitness_value = risk_aversion_factor * variance_value - (1 - risk_aversion_factor) * portfolio_return
    fitness_value + penalty
  }

  # GA Settings
  ga_settings <- list(
      type = "real-valued",
      fitness = function(weights) fitness_function(weights, covariance, avgReturns),
      lower = rep(0, 10),
      upper = rep(1, 10),
      popSize = popSize,
      maxiter = maxiter,
      run = run
  )

  # Run GA
  ga_result <- ga(
      type = ga_settings$type,
      fitness = ga_settings$fitness, 
      lower = ga_settings$lower,
      upper = ga_settings$upper, 
      popSize = ga_settings$popSize,
      maxiter = ga_settings$maxiter, 
      run = ga_settings$run
  )

  return(ga_result)
}

# Get stock symbols
myStocks <- c("AAPL", "JPM", "PFE", "AMZN", "XOM", "SO", "GE", "BHP", "PLD", "VZ")

# Create new environment for stock data
Training_StockData <- new.env()
Testing_StockData <- new.env()
# Fetch stock data

getSymbols(myStocks, src = "yahoo", from = "2017-01-01", to = "2018-01-01", env = Training_StockData)
getSymbols(myStocks, src = "yahoo", from = "2022-01-01", to = "2023-01-01", env = Testing_StockData)
#Avoiding years of unpredictability such as covid 2019-2021

# Run the optimization using the optimizePortfolio function
ga_training_result <- optimizePortfolio(Training_StockData)
ga_testing_result <- optimizePortfolio(Testing_StockData)

# Extracting the best solution and normalizing it
training_best_solution <- ga_training_result@solution
training_best_solution <- training_best_solution / sum(training_best_solution)

testing_best_solution <- ga_testing_result@solution
testing_best_solution <- testing_best_solution / sum(testing_best_solution)


png(file="C:/Users/riley/Documents/GitHub/Postgraduate-Work-/Semester 2/AI for Finance/Assignment 1/GA_plot_training.png")
plot(ga_training_result)
dev.off()


png(file="C:/Users/riley/Documents/GitHub/Postgraduate-Work-/Semester 2/AI for Finance/Assignment 1/GA_plot_testing.png")
plot(ga_testing_result)
dev.off()


print(training_best_solution)
print(testing_best_solution)


