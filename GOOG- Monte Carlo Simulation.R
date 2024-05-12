#Financial Modeling : Volatility Assessment and Monte Carlo Simulation
#Target company: Google 

install.packages("quantmod")
install.packages("PerformanceAnalytics")
library(quantmod)
library(PerformanceAnalytics)  

#Getting 2024 Data 
getSymbols("GOOG",auto.assign= TRUE, from = "2024-01-01", to = "2024-12-31")
GOOG <- Ad(GOOG)

adjusted_prices <- as.numeric(GOOG$GOOG.Adjusted)  #used as.numeric to make sure the calculations run correctly. since we're dealing with financial data, the numbers may sometimes be stored as characters ( because of $symbol for example)

# Now we can calculate the statistics which would give us insights into the central tendency, dispersion, and spread of the data.
mean_adjusted <- mean(adjusted_prices, na.rm = TRUE)
median_adjusted <- median(adjusted_prices, na.rm = TRUE)
variance_adjusted <- var(adjusted_prices, na.rm = TRUE)
range_adjusted <- range(adjusted_prices, na.rm = TRUE)
std_deviation_adjusted <- sd(adjusted_prices, na.rm = TRUE)

# Printing the calculated statistics for a quick overview
print(paste("Mean Adjusted Price for Google in 2024:", mean_adjusted))
print(paste("Median Adjusted Price for Google in 2024:", median_adjusted))
print(paste("Variance of Adjusted Prices for Google in 2024:", variance_adjusted))
print(paste("Range of Adjusted Prices for Google in 2024:", range_adjusted))
print(paste("Standard Deviation of Adjusted Prices for Google in 2024:", std_deviation_adjusted))

# Daily Returns / to understand the day-to-day volatility

getSymbols("GOOG",auto.assign= TRUE, from = "2024-01-01", to = "2024-12-31")
dailyReturn(as.xts(GOOG), type= "arithmetic")
daily_returns <- dailyReturn(as.xts(GOOG[, "GOOG.Close"]), type = "arithmetic")
dailyReturn(Cl(as.xts(GOOG)), type = "arithmetic")
plot(dailyReturn(Cl(as.xts(GOOG)), type = "arithmetic"), type="l")

#Compound Interest / let's see how the investments grow over time, taking into account the effect of compounding
plot(cumprod(1+dailyReturn(Cl(as.xts(GOOG)), type = "arithmetic")))
plot(cumprod(1+dailyReturn(Ad(as.xts(GOOG)), type= "arithmetic")))

diff(log(Ad(GOOG)))
plot(diff(log(Ad(GOOG))), type= "l")

GOOG$GOOG.Close <- as.numeric(GOOG$GOOG.Close)
GOOG$GOOG.Adjusted <- as.numeric(GOOG$GOOG.Adjusted)
GOOG[, c("GOOG.Close", "GOOG.Adjusted")]
head(GOOG[, c("GOOG.Close", "GOOG.Adjusted")])

log_diff <- apply(GOOG[, c("GOOG.Close", "GOOG.Adjusted")], 2, function(x) diff(log(x)))
print(log_diff)

apply(GOOG[, c("GOOG.Close", "GOOG.Adjusted")], 2, length)
apply(GOOG[, c("GOOG.Close", "GOOG.Adjusted")], 2, log) # to normalize the data
apply(apply(GOOG[, c("GOOG.Close", "GOOG.Adjusted")], 2, log), 2, diff)

AdtoCl <- data.frame(apply(apply(GOOG[, c("GOOG.Close", "GOOG.Adjusted")], 2, log), 2, diff))
plot(AdtoCl[, 1], type="l")
lines(AdtoCl[, 1], type="l", col="blue")

# Cumulative Sums
AdtoCl <- data.frame(apply(AdtoCl, 2, cumsum))
AdtoClCumSum <- data.frame(apply(AdtoCl, 2, cumsum))
plot(AdtoClCumSum[, 1], type="l")
lines(AdtoClCumSum[, 2], type="l", col="blue")

# MONTE CARLO SIMULATION 

# Setting the seed for reproducibility
set.seed(0)

# Get historical daily returns
GOOG[,"GOOG.Adjusted"] <- as.numeric(as.character(GOOG[,"GOOG.Adjusted"]))

historical_returns <- na.omit(diff(log(GOOG[,"GOOG.Adjusted"])))
mean_return <- mean(historical_returns)
std_dev <- sd(historical_returns)
nsim <- 100 # Number of simulations
ndays <- 252 #typical nb of trading days in a year
last_price <- tail(as.numeric(GOOG[,"GOOG.Adjusted"]), 1)

simulated_prices <- matrix(NA, nrow = ndays, ncol = nsim)
for (i in 1:nsim) {
  daily_returns <- rnorm(ndays, mean = mean_return, sd = std_dev)
  simulated_prices[, i] <- last_price * cumprod(1 + daily_returns)
}

matplot(simulated_prices, type = "l", main = "Monte Carlo Simulation for GOOG Stock Prices", xlab = "Day", ylab = "Price", col = rgb(0.2, 0.4, 0.6, 0.1)) #used rgb for a faded blue shadow 

