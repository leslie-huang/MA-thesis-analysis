
#################################################################################
#################################################################################
# TIME SERIES ANALYSIS
# run this file after instantiating all the data in the sentiment analysis script
#################################################################################
#################################################################################

# let's check out these time series of data

# ADF tests for stationarity
sapply(monthly_viol[2:4], function(x) {summary(ur.df(na.omit(x), type = "trend", lags = 1))})
# Results: FARC actions / casualties / demobilization: unit root, trend, and drift / unit root, trend, and drift / unit root, no trend, no drift
sapply(monthly_viol[2:4], function(x) {summary(ur.df(na.omit(x), type = "drift", lags = 1))})
# FARC actions / casualties / demobilization: can't reject null / reject null / reject null: unit root, and no drift
sapply(monthly_viol[2:4], function(x) {summary(ur.df(na.omit(x), type = "none", lags = 1))})
# FARC actions / casualties / demobilization: reject null / can't reject / can't reject

# KPSS
sapply(monthly_viol[2:4], function(x) {kpss.test(x, null = "T")})

# check ndiffs: 1 differencing needed for each trend to make stationary
sapply(monthly_viol[2:4], function(x) { ndiffs(x)})

# Now let's see if FARC actions and army casualties are cointegrated.
viol_VAR <- VAR(na.omit(monthly_viol[,2:3]), p = 2, type = "both")

# test for serial correlation of residuals. Increase lags p = 2 in the VAR model to get p val = 0.94: no autocorrelation
serial.test(viol_VAR)

# Now let's run the Johansen cointgration test
summary(ca.jo(na.omit(monthly_viol[,2:3]), type = "trace", K = 2, ecdet = "trend"))
# result: cointegration

# test Granger causality both ways

# significant only when lags = 3, with p = 0.01
grangertest(na.omit(monthly_viol[,2]) ~ na.omit(monthly_viol[,3]), order = 3)

# significant with p = 0.0007
grangertest(na.omit(monthly_viol[,3]) ~ na.omit(monthly_viol[,2]), order = 1)
# result: army deaths are Granger caused by FARC actions
