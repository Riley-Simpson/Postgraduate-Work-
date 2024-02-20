

# Splitting the data
trainData <- eapply(Data, function(x) Cl(x["2019/2022"]))
testData <- eapply(Data, function(x) Cl(x["2023"]))

# Calculate returns for training and testing periods
# (Assuming you have a function or method to calculate returns)

trainReturns <- calculateReturns(trainData, optimalWeights)
testReturns <- calculateReturns(testData, optimalWeights)

# Performance Metrics
meanTrainReturn <- mean(trainReturns, na.rm = TRUE)
meanTestReturn <- mean(testReturns, na.rm = TRUE)

stdTrainReturn <- sd(trainReturns, na.rm = TRUE)
stdTestReturn <- sd(testReturns, na.rm = TRUE)

# Comparison
# Print or plot the comparison results
print(c("Mean Return - Training:", meanTrainReturn, "Testing:", meanTestReturn))
print(c("Standard Deviation - Training:", stdTrainReturn, "Testing:", stdTestReturn))

#GTP 


# Assuming you have your stock data in 'trainData' and 'testData'
# and your optimized weights in 'optimalWeights'

# Function to calculate weighted returns
calculateWeightedReturns <- function(data, weights) {
    stockReturns <- lapply(names(data), function(sym) {
        stockData <- get(sym, envir = data)
        ROC(Ad(stockData), type = "discrete", na.pad = FALSE)
    })
    stockReturns <- stockReturns[sapply(stockReturns, function(x) !is.null(dim(x)))]
    
    if(length(stockReturns) > 0){
        returnsDF <- do.call(merge, stockReturns)
        weightedReturns <- returnsDF %*% weights
        return(rowMeans(weightedReturns, na.rm = TRUE))
    } else {
        return(rep(NA, nrow(data)))
    }
}

# Calculate weighted returns for training and testing data
trainWeightedReturns <- calculateWeightedReturns(trainData, optimalWeights)
testWeightedReturns <- calculateWeightedReturns(testData, optimalWeights)

# Performance Metrics
meanTrainReturn <- mean(trainWeightedReturns, na.rm = TRUE)
meanTestReturn <- mean(testWeightedReturns, na.rm = TRUE)

stdTrainReturn <- sd(trainWeightedReturns, na.rm = TRUE)
stdTestReturn <- sd(testWeightedReturns, na.rm = TRUE)

# Print comparison
print(c("Mean Return - Training:", meanTrainReturn, "Testing:", meanTestReturn))
print(c("Standard Deviation - Training:", stdTrainReturn, "Testing:", stdTestReturn))
