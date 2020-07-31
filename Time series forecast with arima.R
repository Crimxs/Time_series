####################################################
# Time Series and Forecasting Assignment 
#  
#                             Under the Guidance of:-
#                            Prof. Sridhar Srinivasan
# Created By:-                       
# Mohd. Arshan  (E200  ) 
# Indrajit Singh (E20012)
# Satyam (E20030)
# Shantam (E20031)
##################################################


library(tseries)
data <- read.csv("nyc_energy_consumption.csv")
View(data)

str(data)
df <- data[-c(3:4)]
View(df)

df <- log(df)
View(df)
df <- ts(df)

library(lubridate)
library(AnomalyDetection)
View(df)
df <- as.data.frame(df)
str(df)

df$timeStamp <- ymd_hms(df$timeStamp)
res = AnomalyDetectionTs(df, max_anoms=0.001,direction='both', plot=TRUE)
res$plot
View(res)

res_df <- res$anoms
View(res_df)

############################################
######  Missing Value Imputation
##### Data Used here is tsAirgap
###########################################


library("imputeTS")
library(graphics)
library(tseries)
library(forecast)


#data = read.csv('D:/study/time series/nyc_energy_consumption.csv')
data = tsAirgap
tsAirgapComplete = tsAirgapComplete
# View(ts_data)

# summary(ts_data_interpolation)

# we can notice 81 NA's in demand.
# inter polation= we try to fit a line though the data and fill missing 
# values(just like linear regression)
# We can also fit a polynomial line to do so.
# Forward Fill= will use index 1 to fill index 2
# Backward Fill= will use index 3 to fill index 2(LOCF)
# Mean imputation= we simply impute the mean of the data in place of missing values
# Moving Average (MA) Imputation = he average in this algorithm  is  taken
# from the same number of data points  on  either  side of the central  value.
# Error Metric= RMSE
# 

ts_data = df$demand

ts_data = ts(ts_data)
# data = data[!complete.cases(data), ]


ts_data_interpolation = na.interpolation(ts_data)

# ts_data_LOCF = na.locf(ts_data)
# 
# ts_data_LOCF_L = na.locf(ts_data, fromLast = TRUE) 
# 
# ts_data_mean = na.mean(ts_data)
# 
# ts_data_MA = na_ma(ts_data)
# 
# 
# 
# 
# 
# plotNA.imputations(ts_data, ts_data_seadec, tsAirgapComplete)
# 
# 
# # <--------seasonal adjusment-------->
# # The Combination of Seasonal Adjustment and other methods
# # This approach is very effective when seasonality is present.
# # We de-seasonlize the data first, and then do interpolation on the data.
# # Once the mising values are inputed, we need to re-seasonalize the data.
# 
# ts_data_seadec=na_seadec(ts_data, algorithm = "mean")
# 
# 
# ts_data_seadec=na_seadec(ts_data, algorithm = "interpolation")
# 
# plotNA.imputations(ts_data, ts_data_seadec, tsAirgapComplete)

####################################
str(ts_data_interpolation)
summary(ts_data_interpolation)
summary(ts_data)
View(ts_data_interpolation)
###################################

############################################
######  AR , MA , ARMA , ARIMA, SARIMA
##### Data Used here is tsAirgap
###########################################

#Load the forecasting Packages
install.packages("TSstudio")
library(TSstudio)
library(fpp2)
library(ggplot2)

# Load the data
data(USVSales)
str(USVSales)
View(USVSales)

################################
# Prelimniary Analysis
###############################

ts_info(USVSales)

##Let's now plot USVSales:

ts_plot(USVSales,
        title = "US Monthly Total Vehicle Sales",
        Ytitle = "Thousands of Units",
        Xtitle = "Years",
        Xgrid = TRUE,
        Ygrid = TRUE)

# to decompose the data 
plot(decompose(USVSales))

# Investigate data for trend and seasonality as it is hard to guess
# from the Graph
# Take the first difference to remove the trend


ndiffs(USVSales) # to check how many differencing is required
#to make it stationary
DY <- diff(USVSales)

ts_plot(DY,
        title = "US Monthly Total Vehicle Sales",
        Ytitle = "Thousands of Units",
        Xtitle = "Years",
        Xgrid = TRUE,
        Ygrid = TRUE)

# Series appears to be trend-stationary, needs to check seasonality
View(DY)
ggseasonplot(DY)+
  ggtitle("US Monthly Total Vehicle Sales") +
  ylab("Thousands of Units")

# Let's Look at another seasonality plot
# we are not able to get much information about seasonality but we can
# infer the average value of sale in aa month which represents seasonlaity

ggsubseriesplot(DY)


####################################################
#Our series had trend and seasonality
# to remove trend, we take the first difference.
# the first difference series still has seasonality.
# 
# Forecast with various methods
######################################################

library(tseries)
adf = adf.test(DY)
kpss = kpss.test(DY)
adf
kpss

##########
# Use a benchmark method to forecast
# Let's use the seasonal naive method as our benchmark.
# y_t = y_{t-s} + e_t
#####################################################

fit <- snaive(DY)  # Residual sd is 125.29
print(summary(fit))
checkresiduals(fit)

fcst_ets <- forecast(fit,h = 24)
autoplot(fcst_ets)
autoplot(fcst_ets,include = 20)

##########
# Fit ETS Model
############

fit_ets <- ets(DY) # Residual sd 0.0755
print(summary(fit_ets))
checkresiduals(fit_ets)

fcst_ets <- forecast(fit_ets,h = 24)
autoplot(fcst_ets)
autoplot(fcst_ets,include = 20)

##############
# Fit an Arima
#############

fit_arima_1 <- arima(USVSales, order = c(1,1,0))
print(summary(fir_arima))
checkresiduals(fir_arima) 

fcst <- forecast(fit_arima_1)
autoplot(fcst,include = 10)

################################
fit_arima_1 <- arima(USVSales, order = c(1,0,1),seasonal = list(order = c(0, 0, 0)))
print(summary(fit_arima_1))
checkresiduals(fit_arima_1) 

fcst <- forecast(fit_arima_1)
autoplot(fcst)


fir_arima <- auto.arima(USVSales,D=1,max.P= 1, max.Q=1,stepwise = FALSE,approximation = FALSE, trace = TRUE)
print(summary(fir_arima))
checkresiduals(fir_arima) # Residual sd = 90

fcst <- forecast(fir_arima)
autoplot(fcst)


par(mfrow = c(2,2))
acf(DY)
pacf(DY)
acf(USVSales)
pacf(USVSales)

################
# forecast with arima model
####################

fcst <- forecast(fir_arima,h = 24)
autoplot(fcst)
autoplot(fcst, include = 20)


##################################
# Using Tractor sales dataset
##################################

data = read.csv('Tractor-Sales.csv')
data = ts(data[,2],start = c(2003,1),frequency = 12)
plot(data, xlab='Years', ylab = 'Tractor Sales')

View(data)

###################################
# Let's Try 1st level differencing
# DY2 = data(t)-data(t-1)
##################################


DY2 <- diff(data)

ts_plot(DY2,
        title = "Differeced Tractor Sales",
        Xgrid = TRUE,
        Ygrid = TRUE)

##################################################
# We need to reduce the oscillation between many 
# data points to reduce the variance , which can easily 
# be done by taking the log transformation.
####################################################

DY2_log <- log10(data) 

DY2_log_diff <- diff(DY2_log) 

ts_plot(DY2_log_diff,
        title = "DnT Tractor Sales",
        Xgrid = TRUE,
        Ygrid = TRUE)

#######################################################
# Plot ACF and PACF to identify potential AR and MA model
#######################################################

par(mfrow = c(1,2))
acf(ts(DY2_log_diff),main='ACF Tractor Sales')
pacf(ts(DY2_log_diff),main='PACF Tractor Sales')


require(forecast)
ARIMAfit = auto.arima(log10(data), approximation=FALSE,trace=FALSE)
summary(ARIMAfit)

par(mfrow = c(1,1))
pred = predict(ARIMAfit, n.ahead = 36)
pred
plot(data,type='l',xlim=c(2004,2018),ylim=c(1,1600),xlab = 'Year',ylab = 'Tractor Sales')
lines(10^(pred$pred),col='blue')
lines(10^(pred$pred+2*pred$se),col='orange')
lines(10^(pred$pred-2*pred$se),col='orange')


par(mfrow=c(1,2))
acf(ts(ARIMAfit$residuals),main='ACF Residual')
pacf(ts(ARIMAfit$residuals),main='PACF Residual')


####################
#Simple Exponential Smoothing ARIMA(0,1,1)
#Holt's Exponential Smoothing  ARIMA(0,2,2)
#White noise ARIMA(0,0,0)
#Random walk ARIMA(0,1,0) with no constant
#Random walk with drift ARIMA(0,1,0) with a constant
#Autoregression ARIMA(p,0,0)
#Moving average ARIMA(0,0,q)
#########################



