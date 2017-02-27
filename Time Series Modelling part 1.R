
rm(list = ls())
cat("\f")

# Install packages if you have never installed them.
install.packages("xlsx")
install.packages("rJava")
install.packages("tseries")

#Loading packages
library(tseries)
library(rJava)
library(xlsx)

# Part I. Constructing prediction models for different variables
# 
# Read in data
# 
# The data for Part I is contained in the Excel file
# "time_series_data_updated.xlsx." Each sheet contains one of the 4
# different time series to work with. 

getwd()
datas1 = read.xlsx('time_series_data_2017.xlsx',1,rowIndex = 11:826,colIndex = 1:2);
datas2= read.xlsx('time_series_data_2017.xlsx',2,rowIndex = 11:839,colIndex = 1:2);
datas3 = read.xlsx('time_series_data_2017.xlsx',3,rowIndex = 11:839,colIndex = 1:2);
datas4 = read.xlsx('time_series_data_2017.xlsx',4,rowIndex = 8:1761,colIndex = 1:2);

########## Unemployment Rate ##########

mytimeseries = ts(data = datas1$UNRATE, start = 1948, frequency = 12)
plot(mytimeseries)
class(mytimeseries)
decompose(mytimeseries, "multiplicative") # decompose is in R base
plot(decompose(mytimeseries, type = "multiplicative"))
plot(aggregate(mytimeseries,FUN=mean))
boxplot(mytimeseries~cycle(mytimeseries))

## There is no clear indication of trend when we decompose the time series
## There is clear indication of seasonality when we decompose the time series
#############



# Plot raw data
# Part 1
plot(datas1$observation_date, datas1$UNRATE, xlab = 'date', ylab = 'unemployment', type='l')

#  Augmented Dickey-Fuller test for non-stationarity
urate = datas1$UNRATE
pValue = adf.test(urate,k=1)$p.value
pValue

## The (augmented) Dickey-Fuller test indicates non-stationarity.
# Part 2
# Plot the autocorrelation function for the first 10 lags
acf(urate)
urateAutocorrs = acf(urate)$acf
urateAutocorrs
#Perform LBQ tests for significance of autocorrelations
?Box.test
Box.test(urate, lag = 1)
Box.test(urate, lag = 10)
# The serial correlation is statistically significant. i.e there is correlation

# Part 3
# 
# Test out different combinations of AR and MA terms and use AIC and BIC to
# determine fit

maxAR = 4; # max of 4 AR terms
maxMA = 4; # max of 4 MA terms

urateModelCriteria = matrix(0,(maxAR+1)*(maxMA+1)-1,2)
ind = 1
lagCombinations = urateModelCriteria;

# Double for loop over AR and MA lags
for(ii in 0:maxMA){
  for (jj in 0:maxAR) {
    if(ii != 0 || jj != 0){
      urateModelCriteria[ind,1] = AIC(arima(urate, order = c(ii, 0 ,jj)))
      urateModelCriteria[ind,2] = BIC(arima(urate, order = c(ii, 0 ,jj)))
      lagCombinations[ind,] = matrix(c(ii,jj),1,2)
      ind = ind+1
    }
    
  }
}

# Find models with lowest AIC and BIC criteria, corresponding to best statistical fit
minIndices = apply(urateModelCriteria,2,which.min)

# Store the number of AR and MA terms associated with best model according # to AIC
bestAICModelLags = lagCombinations[minIndices[1],]
bestAICModelLags
## ARMA(3,3) is the best model with lowest BIC
library(lmtest)
modelEstimate = arima(urate,order=c(lagCombinations[minIndices[1],1],0,lagCombinations[minIndices[1],2]) )
# Estimate model chosen by AIC
coeftest(modelEstimate)
##  estimates are statistically significant


# Part 4
# Save residuals from fitted ARMA model
resid = modelEstimate$residual
# Test for serial correlation
Box.test(resid,lag = 10)
# Visually inspect fitted residuals
plot(datas1$observation_date,resid,xlab = 'date',ylab = 'residual',type = 'l')

# Construct fittef values from residual and original time series
urateFit = urate-resid

#Plot time series and fitted values
plot(datas1$observation_date, urate, xlab = 'date', ylab = 'unemployment', type='l',col='blue')
lines(datas1$observation_date, urateFit,col='red')
legend(min(datas1$observation_date),10,c('urate','urateFit'),col = c('blue','red'),lty = c(1,1))

## The model fits well and the residual plot suggest no heteroscedasticity. 



########## Consumer Price Index ##########

# Part 1
# 
# Plot raw data

plot(datas2$observation_date, datas2$CPIAUCSL, xlab = 'date', ylab = 'Consumer Price Index', type='l')


mytimeseries = ts(data = datas2$CPIAUCSL, start = 1947, frequency = 12)
plot(mytimeseries)
class(mytimeseries)
decompose(mytimeseries, "multiplicative") # decompose is in R base
plot(decompose(mytimeseries, type = "multiplicative"))
plot(aggregate(mytimeseries,FUN=mean))
boxplot(mytimeseries~cycle(mytimeseries))

## There is clear indication of trend when we decompose the time series
## There is no indication of seasonality when we decompose the time series
#############


#  Augmented Dickey-Fuller test for non-stationarity on the differene log transformed 
adf.test(datas2$CPIAUCSL, alternative="stationary", k=1)
## The (augmented) Dickey-Fuller test indicates non-stationarity. therefore we proceed with log transformation
CPIAUCSL = diff(log(datas2$CPIAUCSL))
pValue = adf.test(CPIAUCSL,k=1)$p.value
adf.test(diff(log(datas2$CPIAUCSL)), alternative="stationary", k=0)

## The (augmented) Dickey-Fuller test indicates non-stationarity.

# plot the transformed series, and use this in the further analysis. 
plot(datas2$observation_date[1:length(datas2$observation_date)-1], CPIAUCSL, xlab = 'date', ylab = 'Consumer Price Index', type='l')

# Part 2
# 
# Plot the autocorrelation function for the first 10 lags

?acf
acf(CPIAUCSL)
pacf(CPIAUCSL)
# Display the autocorrelations
CPIAUCSLAutocorrs = acf(CPIAUCSL)$acf


#Perform LBQ tests for significance of autocorrelations
?Box.test
Box.test(CPIAUCSL, lag = 1)
Box.test(CPIAUCSL, lag = 10)
# The serial correlation is statistically significant. i.e there is correlation

# Part 3
# 
# Test out different combinations of AR and MA terms and use AIC and BIC to determine fit

maxAR = 4; # max of 4 AR terms
maxMA = 4; # max of 4 MA terms

CPIAUCSLModelCriteria = matrix(0,(maxAR+1)*(maxMA+1)-1,2)
ind = 1
lagCombinations = CPIAUCSLModelCriteria;

# Double for loop over AR and MA lags

for(ii in 0:maxMA){
  for (jj in 0:maxAR) {
    if(ii != 0 || jj != 0){
      CPIAUCSLModelCriteria[ind,1] = AIC(arima(CPIAUCSL, order = c(ii, 0 ,jj)))
      CPIAUCSLModelCriteria[ind,2] = BIC(arima(CPIAUCSL, order = c(ii, 0 ,jj)))
      lagCombinations[ind,] = matrix(c(ii,jj),1,2)
      ind = ind+1
    }
    
  }
}

# Find models with lowest AIC and BIC criteria, corresponding to best statistical fit
minIndices = apply(CPIAUCSLModelCriteria,2,which.min)

# Store the number of AR and MA terms associated with best model according to AIC
bestAICModelLags = lagCombinations[minIndices[1],]
bestAICModelLags
## ARMA(3,4) is the best model with lowest BIC

# Estimate model chosen by AIC
library(lmtest)
modelEstimate = arima(CPIAUCSL,order=c(lagCombinations[minIndices[1],1],0,lagCombinations[minIndices[1],2]) )
coeftest(modelEstimate)
##  estimates are statistically significant


# Part 4
# Save residuals from fitted ARMA model
resid = modelEstimate$residual
# Test for serial correlation
Box.test(resid,lag = 10)
# Visually inspect fitted residuals
plot(datas2$observation_date[1:length(datas2$observation_date)-1],resid,xlab = 'date',ylab = 'residual',type = 'l')

# Construct fittef values from residual and original time series
CPIAUCSLFit = CPIAUCSL-resid

#Plot time series and fitted values
plot(datas2$observation_date[1:length(datas2$observation_date)-1], CPIAUCSL, xlab = 'date', ylab = 'Consumer Price Index', type='l',col='blue')
lines(datas2$observation_date[1:length(datas2$observation_date)-1], CPIAUCSLFit,col='red')
legend(min(datas2$observation_date[1:length(datas2$observation_date)-1]),10,c('CPIAUCSL','CPIAUCSLFit'),col = c('blue','red'),lty = c(1,1))
## The model fits well and the residual plot suggest no heteroscedasticity. 




########## Gold Fixing Price ##########

colnames(datas3) <- c("observation_date","GOLDPrice")
###############
# GOLD = diff(log(datas3$GOLDPrice))

mytimeseries = ts(data = datas3$GOLDPrice, start = 1968, frequency = 12)
plot(mytimeseries)
class(mytimeseries)
decompose(mytimeseries, "multiplicative") # decompose is in R base
plot(decompose(mytimeseries, type = "multiplicative"))
plot(aggregate(mytimeseries,FUN=mean))
boxplot(mytimeseries~cycle(mytimeseries))

## There is clear indication of trend when we decompose the time series
## There is clear indication of seasonality during Sep and Oct month when we decompose the time series
#############

#


# Part 1
# 
# Plot raw data

plot(datas3$observation_date, datas3$GOLDPrice, xlab = 'date', ylab = 'Gold Fixing Price', type='l')

#  Augmented Dickey-Fuller test for non-stationarity on the differene log transformed 
## The (augmented) Dickey-Fuller test indicates non-stationarity. therefore we proceed with log transformation
adf.test(datas3$GOLDPrice, alternative="stationary", k=1)
GOLD = diff(log(datas3$GOLDPrice))
pValue = adf.test(GOLD,k=1)$p.value



# plot the transformed series, and use this in the further analysis. 
plot(datas3$observation_date[1:length(datas3$observation_date)-1], GOLD, xlab = 'date', ylab = 'Gold Fixing Price', type='l')
# Part 2
# 
# Plot the autocorrelation function for the first 10 lags
acf(GOLD)
pacf(GOLD)
# Display the autocorrelations
GOLDAutocorrs = acf(GOLD)$acf

#Perform LBQ tests for significance of autocorrelations
Box.test(GOLD, lag = 1)
Box.test(GOLD, lag = 10)
# The serial correlation is not statistically significant. i.e there no is correlation

# Part 3
# 
# Test out different combinations of AR and MA terms and use AIC and BIC to determine fit

maxAR = 4; # max of 4 AR terms
maxMA = 4; # max of 4 MA terms

GOLDModelCriteria = matrix(0,(maxAR+1)*(maxMA+1)-1,2)
ind = 1
lagCombinations = GOLDModelCriteria;

# Double for loop over AR and MA lags

for(ii in 0:maxMA){
  for (jj in 0:maxAR) {
    if(ii != 0 || jj != 0){
      GOLDModelCriteria[ind,1] = AIC(arima(GOLD, order = c(ii, 0 ,jj)))
      GOLDModelCriteria[ind,2] = BIC(arima(GOLD, order = c(ii, 0 ,jj)))
      lagCombinations[ind,] = matrix(c(ii,jj),1,2)
      ind = ind+1
    }
    
  }
}

# Find models with lowest AIC and BIC criteria, corresponding to best statistical fit
minIndices = apply(GOLDModelCriteria,2,which.min)

# Store the number of AR and MA terms associated with best model according to AIC
bestAICModelLags = lagCombinations[minIndices[1],]
bestAICModelLags
## ARMA(4,4) is the best model with lowest BIC


# Estimate model chosen by AIC
library(lmtest)
modelEstimate = arima(GOLD,order=c(lagCombinations[minIndices[1],1],0,lagCombinations[minIndices[1],2]) )
# modelEstimate = arima(GOLD,order=c(1,0,1) )
coeftest(modelEstimate)
##  Coefficient estimates are statistically significant


# Part 4
# Save residuals from fitted ARMA model
resid = modelEstimate$residual
# Test for serial correlation
Box.test(resid,lag = 10)
# Visually inspect fitted residuals
plot(datas3$observation_date[1:length(datas3$observation_date)-1],resid,xlab = 'date',ylab = 'residual',type = 'l')

# Construct fittef values from residual and original time series
GOLDFit = GOLD-resid

#Plot time series and fitted values
plot(datas3$observation_date[1:length(datas3$observation_date)-1], GOLD, xlab = 'date', ylab = 'Gold Fixing Price', type='l',col='blue')
lines(datas3$observation_date[1:length(datas3$observation_date)-1], GOLDFit,col='red')
legend(min(datas3$observation_date[1:length(datas3$observation_date)-1]),10,c('GOLD','GOLDFit'),col = c('blue','red'),lty = c(1,1))
## The model doesn't fit well however the residual plot suggest no heteroscedasticity. 




########## Stock Market Data ##########


###############
colnames(datas4) <- c("observation_date","STOCKPrice")
# STOCK = diff(log(datas$P))

mytimeseries = ts(data = datas4$STOCKPrice, start = 1871, frequency = 12)
plot(mytimeseries)
class(mytimeseries)
decompose(mytimeseries, "additive") # decompose is in R base
plot(decompose(mytimeseries, type = "additive"))
plot(aggregate(mytimeseries,FUN=mean))
boxplot(mytimeseries~cycle(mytimeseries))

## There is no clear indication of trend during the initial period , however the later years
## shows clear trend when we decompose the time series
## There is clear indication of seasonality when we decompose the time series
#############




# Part 1
# 
# Plot raw data

plot(datas4$observation_date, datas4$STOCKPrice, xlab = 'date', ylab = 'Stock Market', type='l')

#  Augmented Dickey-Fuller test for non-stationarity 
## The (augmented) Dickey-Fuller test indicates non-stationarity. therefore we proceed with log transformation
adf.test(datas4$STOCKPrice, alternative="stationary", k=1)
STOCK = diff(log(datas4$STOCKPrice))
pValue = adf.test(STOCK,k=1)$p.value
# plot the transformed series, and use this in the further analysis. 
plot(datas4$observation_date[1:length(datas4$observation_date)-1], STOCK, xlab = 'date', ylab = 'Stock Market', type='l')
# Part 2
# 
# Plot the autocorrelation function for the first 10 lags
acf(STOCK)
pacf(STOCK)
# Display the autocorrelations
STOCKAutocorrs = acf(STOCK)$acf

#Perform LBQ tests for significance of autocorrelations
Box.test(STOCK, lag = 1)
Box.test(STOCK, lag = 10)
# The serial correlation is statistically significant. i.e there is correlation

# Part 3
# 
# Test out different combinations of AR and MA terms and use AIC and BIC to determine fit

maxAR = 4; # max of 4 AR terms
maxMA = 4; # max of 4 MA terms

STOCKModelCriteria = matrix(0,(maxAR+1)*(maxMA+1)-1,2)
ind = 1
lagCombinations = STOCKModelCriteria;

# Double for loop over AR and MA lags

for(ii in 0:maxMA){
  for (jj in 0:maxAR) {
    if(ii != 0 || jj != 0){
      STOCKModelCriteria[ind,1] = AIC(arima(STOCK, order = c(ii, 0 ,jj)))
      STOCKModelCriteria[ind,2] = BIC(arima(STOCK, order = c(ii, 0 ,jj)))
      lagCombinations[ind,] = matrix(c(ii,jj),1,2)
      ind = ind+1
    }
    
  }
}

# Find models with lowest AIC and BIC criteria, corresponding to best statistical fit
minIndices = apply(STOCKModelCriteria,2,which.min)

# Store the number of AR and MA terms associated with best model according to AIC
bestAICModelLags = lagCombinations[minIndices[1],]
bestAICModelLags
## ARMA(2,4) is the best model with lowest BIC

# Estimate model chosen by AIC
library(lmtest)
modelEstimate = arima(STOCK,order=c(lagCombinations[minIndices[1],1],0,lagCombinations[minIndices[1],2]) )
coeftest(modelEstimate)
##  coefficient estimates are statistically significant

# Part 4
# Save residuals from fitted ARMA model
resid = modelEstimate$residual
# Test for serial correlation
Box.test(resid,lag = 10)
# Visually inspect fitted residuals
plot(datas4$observation_date[1:length(datas4$observation_date)-1],resid,xlab = 'date',ylab = 'residual',type = 'l')

# Construct fittef values from residual and original time series
STOCKFit = STOCK-resid

#Plot time series and fitted values
plot(datas4$observation_date[1:length(datas4$observation_date)-1], STOCK, xlab = 'date', ylab = 'Stock Market', type='l',col='blue')
lines(datas4$observation_date[1:length(datas4$observation_date)-1], STOCKFit,col='red')
legend(min(datas3$observation_date[1:length(datas4$observation_date)-1]),10,c('STOCK','STOCKFit'),col = c('blue','red'),lty = c(1,1))

## The model fits well and the residual plot suggest no heteroscedasticity. 

