library(quantmod)
library(tidyverse)

#To align with current market trends, I chose the AI sector and NVIDIA has been a strategic choice after it made headlines following the publication of its 2023 earnings.

NVDA <- data.frame(getSymbols("NVDA", from = "2023-01-01", to = "2023-12-31", auto.assign = FALSE))
colnames(NVDA) <- c("NVDA_Adjusted") #naming only one to just focus on the adjusted close column 

INTC <- data.frame(getSymbols("INTC", from = "2023-01-01", to = "2023-12-31", auto.assign = FALSE))
colnames(INTC) <- c("INTC_Adjusted")

# Displaying the head and tail of the NVDA dataframe
head(NVDA)
tail(NVDA)

# Displaying the head and tail of the INTC dataframe
head(INTC)
tail(INTC)

#To calculate the return, I used nrow() rather than the exact number of obs which is 250 just to keep the script general as if we're assuming we don't know the last obs
NVDA_return <- 100 * ((Ad(NVDA)[nrow(NVDA)] - Ad(NVDA)[1]) / Ad(NVDA)[1])
INTC_return <- 100 * ((Ad(INTC)[nrow(INTC)] - Ad(INTC)[1]) / Ad(INTC)[1])

#total returns
print(paste("Total return for NVDA in 2023:", NVDA_return, "%"))
print(paste("Total return for INTC in 2023:", INTC_return, "%"))

# STD: PLEASE SEE INTERPRETATION AT THE END OF THE SCRIPT

# Calculating standard deviation for NVDA
NVDA_sd <- sd(NVDA$NVDA_Adjusted)
print(paste("Standard deviation for NVDA in 2023:", NVDA_sd))

# Calculating standard deviation for INTC
INTC_sd <- sd(INTC$INTC_Adjusted)
print(paste("Standard deviation for INTC in 2023:", INTC_sd))

# Extracting again the adjusted prices to make the process clearer
NVDA_adj <- Ad(NVDA)
INTC_adj <- Ad(INTC)

# Creating the plot
dates <- as.Date(index(NVDA)) # I wanted to show the dates on the X axis so I added the as.Date so that R can recognize them from the dataframe

# Creating a single plot for both stocks ( had to make the script longer just to create one range for both)
plot(dates, NVDA$NVDA_Adjusted, type = "l", col = "green", ylim = range(NVDA$NVDA_Adjusted, INTC$INTC_Adjusted), xlab = "Date", ylab = "Adjusted Price")
lines(dates, INTC$INTC_Adjusted, col = "blue")
#( if it helps visually, I chose the logo colors of each company)

#Visually, we can see how NVidia stock return has risen drastically compared to Intel, which is aligned what the recent earnings report of nvdia has shown

#Interpretation: 
 #For NVDA: The standard deviation of 108.5 indicates that the adjusted prices of NVDA stock had a higher degree of variability/dispersion around the mean throughout 2023. In other words, NVDA's stock prices experienced larger fluctuations/ were more volatile compared to INTC's stock prices during the same period.
 #For INTC: The standard deviation of 5.4 tells us that Intel's stock prices experienced smaller fluctuations compared to NVDA's stock prices during 2023.

library(quantmod)

NVDA_xts <- getSymbols("NVDA", from = "2023-01-01", to = "2023-12-31", auto.assign = FALSE)
INTC_xts <- getSymbols("INTC", from = "2023-01-01", to = "2023-12-31", auto.assign = FALSE)

NVDA_daily_returns <- dailyReturn(NVDA_xts, type = "arithmetic")
INTC_daily_returns <- dailyReturn(INTC_xts, type = "arithmetic")

plot(NVDA_daily_returns, type = "l", main = "NVDA Daily Returns")

plot(cumprod(1 + NVDA_daily_returns), type = "l", main = "NVDA Cumulative Daily Returns")

NVDA_log_diffs <- diff(log(Ad(NVDA_xts)))
plot(NVDA_log_diffs, type = "l", main = "NVDA Log Differences")

INTC_log_diffs <- diff(log(Ad(INTC_xts)))
plot(INTC_log_diffs, type = "l", main = "INTC Log Differences")

NVDA_close_adjusted <- NVDA_xts[,c("NVDA.Close","NVDA.Adjusted")]
INTC_close_adjusted <- INTC_xts[,c("INTC.Close","INTC.Adjusted")]


#trying another script

getSymbols("NVDA", from = "2023-01-01", to = "2023-12-31")
getSymbols("INTC", from = "2023-01-01", to = "2023-12-31")

# Now NVDA and INTC are in the environment as xts objects

NVDA_close_adjusted <- NVDA[,c("NVDA.Close","NVDA.Adjusted")]
head(NVDA_close_adjusted)
NVDA_log_diffs <- diff(log(NVDA_close_adjusted))
lengths_NVDA <- apply(NVDA_close_adjusted, 2, length)
log_NVDA <- apply(NVDA_close_adjusted, 2, log)
diff_log_NVDA <- apply(log_NVDA, 2, diff)

INTC_close_adjusted <- INTC[,c("INTC.Close","INTC.Adjusted")]
head(INTC_close_adjusted)
INTC_log_diffs <- diff(log(INTC_close_adjusted))
lengths_INTC <- apply(INTC_close_adjusted, 2, length)
log_INTC <- apply(INTC_close_adjusted, 2, log)
diff_log_INTC <- apply(log_INTC, 2, diff)

head(NVDA_close_adjusted)
NVDA_log_diffs

install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
library(PerformanceAnalytics)

df_NVDA <- data.frame(getSymbols("NVDA", from = "2023-01-01", to = "2023-12-31", auto.assign = FALSE))
NVDA_RET <- df_NVDA$NVDA.Close / lag(df_NVDA$NVDA.Close) - 1

df_INTC <- data.frame(getSymbols("INTC", from = "2023-01-01", to = "2023-12-31", auto.assign = FALSE))
INTC_RET <- df_INTC$INTC.Close / lag(df_INTC$INTC.Close) - 1

set.seed(0) 

ndays <- 1000 

NVDA_sim <- sample(NVDA_RET, ndays, replace = TRUE)

INTC_sim <- sample(INTC_RET, ndays, replace = TRUE)

