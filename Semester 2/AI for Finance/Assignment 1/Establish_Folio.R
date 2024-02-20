#install.packages("quantmod")

library(quantmod)
myStocks <- c("AAPL", "JPM", "PFE", "AMZN", "XOM", "SO", "GE", "BHP", "PLD","VZ")
Data <- new.env()

# Adjusting the date to a range for which data is available
endDate <- format(Sys.Date(), "%Y-%m-%d")  # Current date in YYYY-MM-DD format
getSymbols(myStocks, src = "yahoo", from = "2019-01-01", env = Data)


## Note: Once you have downloaded the data then try to avoid downloading it again in the same session (unless you have lost or corrupted it).
## Hitting Yahoo finance too frequently can occasionally result in requests getting blocked.


Returns <- lapply(ls(Data), function(sym) 
{
    stockData <- get(sym, envir = Data)
    # Calculating rate of change, ensuring no NA values are returned
    ROC(Ad(stockData), type = "discrete", na.pad = FALSE)
})

Returns <- Returns[sapply(Returns, function(x) !is.null(dim(x)))]

# Merging data frames safely
if(length(Returns) > 0){
  ReturnsDF <- do.call(merge, Returns)
  covariance <- cov(na.omit(ReturnsDF))  # Compute covariance, omitting NA values
} else {
  ReturnsDF <- data.frame()
  covariance <- matrix()
}

#print(covariance)

weights <- c(0.10, 0.10, 0.10, 0.10, 0.10, 0.10, 0.10, 0.10, 0.10, 0.10)

# Weighted Returns
weightedReturns <- ReturnsDF %*% weights

# Portfolio Variance
portfolioVariance <- t(weights) %*% cov(ReturnsDF) %*% weights

fitnessfunction <- function(ReturnsDF, weights) {
    weightedReturns <- ReturnsDF %*% weights
    variance <- t(weights) %*% cov(ReturnsDF, use = "complete.obs") %*% weights
    list(weightedReturns = weightedReturns, variance = variance)
}

#============================================


#Identify the optimal set of weights for your portfolio which gives the best balance of risk versus return:

# Objective function: Minimize portfolio variance
optimizeVariance <- function(weights, returns) {
  # Ensure weights sum to 1
  weights <- weights / sum(weights)
  
  # Calculate portfolio variance
  variance <- t(weights) %*% cov(returns) %*% weights
  return(variance)
}

# Initial weights (equal distribution)
initialWeights <- rep(1/ncol(ReturnsDF), ncol(ReturnsDF))

# Use optim() to minimize the variance
optimalWeights <- optim(
  par = initialWeights,
  fn = optimizeVariance, 
  returns = ReturnsDF
)$par

# Rescale the weights to ensure they sum to 1
optimalWeights <- optimalWeights / sum(optimalWeights)

Folio <- data.frame(Stocks = myStocks, weights = optimalWeights)

Folio