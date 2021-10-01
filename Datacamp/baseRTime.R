# Base R: ----
# Nile
is.ts(Nile)
Nile
length(Nile)
head(Nile, n = 10)

# - eda
plot(Nile)
plot(Nile, xlab = "Year", ylab = "River Volume (1e9 m^{3})")
plot(Nile, xlab = "Year", ylab = "River Volume (1e9 m^{3})", main = "Annual River Nile Volume at Aswan, 1871-1970", type = "b")
# - MA Model
MA <- arima(Nile, order = c(0, 0, 1))
print(MA)
MA_fit <- Nile - resid(MA)
points(MA_fit, type = "l", col = 2, lty = 2)


# AirPassengers
is.ts(AirPassengers)
# - eda
plot(AirPassengers)
start(AirPassengers)
end(AirPassengers)
deltat(AirPassengers)
frequency(AirPassengers)
time(AirPassengers)
cycle(AirPassengers)

# Compute the mean of AirPassengers
mean(AirPassengers, na.rm = TRUE)

# Impute mean values to NA in AirPassengers
AirPassengers[85:96] <- mean(AirPassengers, na.rm = TRUE)

# Generate another plot of AirPassengers
plot(AirPassengers)

# Add the complete AirPassengers data to your plot
rm(AirPassengers)
points(AirPassengers, type = "l", col = 2, lty = 3)


# Elimianting Trend & Seasonality
log()
diff(, s = 4)

# White Noise
# - fixed constant mean
# - fixed constant variance
# - no correlation
# ARIMA(p,d,q)
# - p = autoregressive order
# - d = order of differencing
# - q = moving average order
white_noise <- arima.sim(model = list(order = c(0, 0, 0)), n = 100)
ts.plot(white_noise)
arima(white_noise, order = c(0, 0, 0))
white_noise_2 <- arima.sim(list(order = c(0, 0, 0)), n = 100, mean = 100, sd = 10)
ts.plot(white_noise_2)
arima(white_noise_2, order = c(0, 0, 0))

# Random Walk
# - No specified mean or variance
# - Strong dependence over time
# - Its increments follow a White Noise
# Today = Yesterday + Noise
random_walk <- arima.sim(model = list(order = c(0, 1, 0)), n = 100)
ts.plot(random_walk)
random_walk_diff <- diff(random_walk)
ts.plot(random_walk_diff)

rw_drift <- arima.sim(model = list(order = c(0, 1, 0)), n = 100, mean = 1)
ts.plot(rw_drift)
rw_drift_diff <- diff(rw_drift)
ts.plot(rw_drift_diff)


# EU Stocks
is.ts(EuStockMarkets)

# - eda
plot(EuStockMarkets)
ts.plot(EuStockMarkets, col = 1:4, xlab = "Year", ylab = "Index Value", main = "Major European Stock Indices, 1991-1998")
legend("topleft", colnames(EuStockMarkets), lty = 1, col = 1:4, bty = "n")

start(EuStockMarkets)
end(EuStockMarkets)
frequency(EuStockMarkets)

# Convert prices into returns
returns <- EuStockMarkets[-1,] / EuStockMarkets[-1860,] - 1
# Convert returns into ts
returns <- ts(returns, start = c(1991, 130), frequency = 260)
plot(returns)
# Convert prices into log returns
logreturns <- diff(log(EuStockMarkets))
plot(logreturns)

# DAX & FTSE
DAX <- EuStockMarkets[,1]
FTSE <- EuStockMarkets[,4]
# - eda
plot(DAX, FTSE)
pairs(EuStockMarkets)
# - convert eu_stocks to log returns
logreturns <- diff(log(EuStockMarkets))
plot(logreturns)
pairs(logreturns)
DAX_logreturns <- diff(log(DAX))
FTSE_logreturns <- diff(log(FTSE))
# - covariance & correlation
cov(logreturns)
cov(DAX_logreturns, FTSE_logreturns)
cor(logreturns)
cor(DAX_logreturns, FTSE_logreturns)
# - autocorrelation
acf(DAX, lag.max = 10, plot = T)


# Autoregressive Model
x <- arima.sim(model = list(ar = 0.5), n = 100)
y <- arima.sim(model = list(ar = 0.9), n = 100)
z <- arima.sim(model = list(ar = -0.75), n = 100)
plot.ts(cbind(x, y, z))

acf(x)
acf(y)
acf(z)

# Moving Average Model
x <- arima.sim(model = list(ma = 0.5), n = 100)
y <- arima.sim(model = list(ma = 0.9), n = 100)
z <- arima.sim(model = list(ma = -0.5), n = 100)
plot.ts(cbind(x, y, z))

acf(x)
acf(y)
acf(z)

# ARIMA: ----
library(astsa)
library(dplyr)

plot(AirPassengers)
plot(djia$Close)
plot(soi)

plot(globtemp)
plot(diff(globtemp))

WN <- arima.sim(model = list(order = c(0, 0, 0)), n = 200)
plot(WN)
MA <- arima.sim(model = list(order = c(0, 0, 1), ma = .9), n = 200)  
plot(MA)
AR <- arima.sim(model = list(order = c(2, 0, 0), ar = c(1.5, -.75)), n = 200) 
plot(AR)
