dev.new(noRStudioGD = TRUE)

library(astsa)
library(forecast)
library(tseries)

## 1. US Personal Consumption

# Read uschange.csv

us.consump <- read.csv("Week 3/uschange.csv")
us.consump.ts <- ts(us.consump$Consumption, frequency = 4, start = c(1970,1))
plot(us.consump.ts)

# What modeling assumption do we have to check? A. Stationarity

kpss.test(us.consump.ts)
# Fail to reject H0 - Series is stationary
# The series is a day over day percentage change - which means it is already differenced

# Plot the ACF and PACF
acf2(us.consump.ts)
# Graph shows LAG÷4 because we read the data with freq = 4. 
# That means 1 on the x-axis is 1 year or 4 quarters

# Use the ACF and PACF plots to identify what kind of series does this follow?
# Remember:
# Characteristics of AR processes:
#   - ACF decays exponentially or is sinusoidal (like a sine wave)
# -
#   - PACF decays exponentially or is sinusoidal
# -
#   There is a significant spike at lag in PACF, but none beyond · Characteristics of MA processes:
#   There is a significant spike at lag in ACF, but none beyond q

# Based on this we guess MA 3

cons.ts.ma3 <- sarima(us.consump.ts, p = 0, d = 0, q =3)
cons.ts.ma3

# What do the plot results mean?

# Standardized residual plot: Checking for constant variance assumption
# ACF of residuals: We want the residuals to be non-significant and white noise
# So if we find that the ACF of residuals has significant spikes, it means we have missed AR/MA terms
# QQ plot: Checking the normality assumption - useful when we make forecasts because we assume normality
# Ljung-Box p-value plot: Indication of white noise, we want to see all the p-values above the blue line

# Let's try an AR3 model - Instructor's recommendation

cons.ts.ar3 <- sarima(us.consump.ts, p = 3, d = 0, q = 0)
cons.ts.ar3

cons.ts.ma4 <- sarima(us.consump.ts, p = 0, d = 0, q = 4)

auto.arima(us.consump.ts, seasonal = FALSE, trace = TRUE)

# Why are we seeing such large AICc values in auto.arima() compared to sarima?
# A. Because sarima() scales the AICc by series length

auto.arima(us.consump.ts, seasonal = FALSE, trace = TRUE, stepwise = FALSE)
# stepwise = FALSE allows us to iterate over more model options