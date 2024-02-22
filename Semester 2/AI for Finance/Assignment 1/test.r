# Install and load required packages
install.packages("quantmod")
install.packages("GA")

library(quantmod)
library(GA)

# Define stock symbols and initialize environment
myStocks <- c("AAPL", "JPM", "PFE", "AMZN", "XOM", "SO", "GE", "BHP", "PLD", "VZ")
Data <- new.env()

# Download training data (2017-2018)
getSymbols(myStocks, src = "yahoo", from = "2017-01-01", to = "2018-12-31", env = Data)

# Calculate returns for training data
TrainReturns <- lapply(ls(Data), function(sym) {
    stockData <- get(sym, envir = Data)
    ROC(Ad(stockData), type = "discrete", na.pad = FALSE)
})
TrainReturns <- TrainReturns[sapply(TrainReturns, function(x) !is.null(dim(x)))]

# Merge data frames and calculate covariance for training data
TrainReturnsDF <- do.call(merge, TrainReturns)
covariance <- cov(na.omit(TrainReturnsDF))

# Define fitness function for GA
fitness_function <- function(weights) {
    portfolio_variance <- t(weights) %*% covariance %*% weights
    variance_value <- as.numeric(portfolio_variance)
    penalty = 1000 * abs(sum(weights) - 1)
    return(variance_value + penalty)
}

# GA settings and execution
ga_settings <- list(type = "real-valued", fitness = fitness_function, lower = rep(0, 10), upper = rep(1, 10), popSize = 50, maxiter = 100, run = 50)
ga_result <- ga(type = ga_settings$type, fitness = ga_settings$fitness, lower = ga_settings$lower, upper = ga_settings$upper, popSize = ga_settings$popSize, maxiter = ga_settings$maxiter, run = ga_settings$run)
best_solution <- ga_result@solution
best_solution <- best_solution / sum(best_solution)

# Clear symbols from the environment to avoid conflicts
symbolsToRemove <- ls(envir = Data)
for (sym in symbolsToRemove) {
  rm(list = sym, envir = Data)
}

# Download testing data (e.g., for 2019)
getSymbols(myStocks, src = "yahoo", from = "2019-01-01", to = "2019-12-31", env = Data)

# Calculate returns for testing data
TestReturns <- lapply(ls(Data), function(sym) {
    stockData <- get(sym, envir = Data)
    ROC(Ad(stockData), type = "discrete", na.pad = FALSE)
})
TestReturns <- TestReturns[sapply(TestReturns, function(x) !is.null(dim(x)))]
TestReturnsDF <- do.call(merge, TestReturns)

# Ensure that the dimensions of the best_solution match those of the TestReturnsDF
if(ncol(TestReturnsDF) == length(best_solution)) {
    portfolio_return_test <- sum(TestReturnsDF * best_solution, na.rm = TRUE)
    print(portfolio_return_test)
} else {
    print("Dimension mismatch between TestReturnsDF and best_solution.")
}
