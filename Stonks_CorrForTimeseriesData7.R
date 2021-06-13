library(readxl)
library(dynlm)
library(forecast)

# Better ACF plot
plot.acf <- function(ACFobj) {
  # lagSize <- 20
  rr <- ACFobj$acf[-1]
  kk <- length(rr)
  lagSize <- kk/2
  nn <- ACFobj$n.used
  plot(seq(kk)-lagSize, rr, type = "h", lwd = 2, yaxs = "i", xaxs = "i", 
       # ylim = c(floor(min(rr)), 1), xlim = c(0, kk + 1), xlab = "Lag", 
       ylim = c(-1, 1), xlim = c(-lagSize, lagSize), xlab = "Lag", 
       ylab = "Correlation (r)", las = 1)
  abline(h = -1/nn + c(-2, 2)/sqrt(nn), lty = "dashed", col = "blue")
  abline(h = 0)
}

# Read in data
seriesX <- read.csv("GME.csv")
seriesY <- read.csv("SPY.csv")

# # Choose what to compare
x <- seriesX$Open
y <- seriesY$Open

# Graph price data (percentage)
# Percent change might need scaling depending on what you are looking at
# Change the ylim = c(-3,3) to change the scale

xForPlot <- x
yForPlot <- y

plot(xForPlot/sum(xForPlot)*100, type="l", ylab = "Percent Change", xlab = "Days", ylim = c(-3,3))
abline(a=0,b=0)
lines(yForPlot/sum(yForPlot)*100, col = 'blue')

# Differencing
xd = diff(x,1)
yd = diff(y,1)

# Cross correlation
ccfout = ccf(xd,yd,na.action=na.omit, plot = FALSE)

# Plot results
plot.acf(ccfout)

# Notes:
# When you run a ccf, you are running a bunch of correlations in different time windows.
# You need enough data to run a correlation, so the ccf cannot capture the entire dataset.
# In particular, the max lag = 10*log10(N/m) where N is the number of observations and m the number of series




