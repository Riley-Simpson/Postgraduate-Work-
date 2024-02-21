# Install and load required packages
install.packages("quantmod")
install.packages("GA")

library(GA)
library(quantmod)

# Get stock data
myStocks <- c("AAPL", "JPM", "PFE", "AMZN", "XOM", "SO", "GE", "BHP", "PLD", "VZ")
Data <- new.env()
endDate <- format(Sys.Date(), "%Y-%m-%d")  # Current date in YYYY-MM-DD format
getSymbols(myStocks, src = "yahoo", from = "2019-01-01", env = Data)

# Calculate returns
Returns <- lapply(ls(Data), function(sym) {
    stockData <- get(sym, envir = Data)
    ROC(Ad(stockData), type = "discrete", na.pad = FALSE)
})

Returns <- Returns[sapply(Returns, function(x) !is.null(dim(x)))]

# Merge data frames
if(length(Returns) > 0){
  ReturnsDF <- do.call(merge, Returns)
  covariance <- cov(na.omit(ReturnsDF))  # Compute covariance, omitting NA values
} else {
  ReturnsDF <- data.frame()
  covariance <- matrix()
}

# Fitness function for GA
fitness_function <- function(weights) {
  # Calculate portfolio variance
  portfolio_variance <- t(weights) %*% covariance %*% weights

  # Extract the variance value from the matrix
  variance_value <- as.numeric(portfolio_variance)

  # Penalty for not summing to 1
  penalty = 1000 * abs(sum(weights) - 1)

  # Minimize the variance plus penalty
  return(variance_value + penalty)
}

# GA settings
ga_settings <- list(type = "real-valued", 
                    fitness = fitness_function, 
                    lower = rep(0, 10), 
                    upper = rep(1, 10), 
                    popSize = 50, 
                    maxiter = 100, 
                    run = 50)

# Run GA
ga_result <- ga(type = ga_settings$type, fitness = ga_settings$fitness, 
                lower = ga_settings$lower, upper = ga_settings$upper, 
                popSize = ga_settings$popSize, maxiter = ga_settings$maxiter, 
                run = ga_settings$run)

# Best Solution
best_solution <- ga_result@solution

# Ensure that the weights sum up to 1 (due to numerical precision issues)
best_solution <- best_solution / sum(best_solution)

print(best_solution)
