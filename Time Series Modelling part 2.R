
rm(list = ls())
cat("\f")

# Install packages if you have never installed them.
install.packages("xlsx")
install.packages("rJava")
install.packages("tseries")
install.packages("forecast")

#Loading packages
library(tseries)
library(rJava)
library(xlsx)
library(lubridate)

getwd()
datas1 = read.xlsx('Keeling_CO2data_2017.xlsx',1,rowIndex = 8:713,colIndex = 1:5);
colnames(datas1) <- c("Yr","month","Date1","Date2","CO2.ppm")
# datas1 <- datas1[12:nrow(datas1),]

str(datas1)

datas1$Yr <- ifelse(datas1$month == 12,datas1$Yr-1,datas1$Yr )

datas1$datenew <- mdy(paste(datas1$month,'01',datas1$Yr,sep = '-'))
datas1 <- datas1[,c(5:6)]
datas1$month <- as.factor(month(datas1$datenew))
datas1$month <- factor(datas1$month, levels=unique(datas1$month))
str(datas1)

########## CO2 PPM Rate ##########
# Part 1

# 1.	Plot the CO2 time series in column E. Briefly summarize the evidence of seasonal effects and 
# trends in the time-series plot.

# Plot raw data
plot(datas1$datenew, datas1$CO2.ppm, xlab = 'date', ylab = 'co2', type='l')

mytimeseries = ts(data = datas1$CO2.ppm, start=c(1958, 2), end=c(2006, 10), frequency = 12)
plot(mytimeseries)
class(mytimeseries)

plot(datas1$CO2.ppm, type='l')
abline(reg=lm(datas1$CO2.ppm~time(datas1$CO2.ppm)))


decompose(mytimeseries, "multiplicative") # decompose is in R base
plot(decompose(mytimeseries, type = "multiplicative"))
plot(aggregate(mytimeseries,FUN=mean))
#Box plot across months will give us a sense on seasonal effect
boxplot(mytimeseries~cycle(mytimeseries))

# Solution 1 -
#### Important Inferences
# 1.	The year on year trend clearly shows that the amount of CO2 have been increasing without fail.
# 2.	The mean value starts increasing in March, reaches its peak in April and then falls until August and then is more or less consistent for the next 4-5 months.
# 3.	The variance of CO2 contents is consistent across all months.
# 4.	The mean value of each month is a bit different and their variance is negligible. Hence, we have seasonal effect across months.
#############

#


# Part 2
# Using data up to 2005, estimate a linear trend model for the CO2 measurements. 
# Is the trend significant? Is the linear trend a good specification that yields reliable forecasts?

datas1.2005 <- datas1[datas1$datenew<= '2005-12-01',]
datas1.2005$month <- factor(datas1.2005$month, levels=unique(datas1.2005$month))
str(datas1.2005)
reg=lm(datas1.2005$CO2.ppm~time(datas1.2005$CO2.ppm))
summary(reg)
r <- reg$residuals # get the residuals from the fit
plot.ts(r)
Box.test(reg$residuals)

#Solution 2-  Yes the trend is significant . Looking at the statistical significance of the intercepts and 
# coefficient , we can say that there is a positive trend(Coefficient estimate for the time stamp is postive)
# of CO2 PPM level over time.
# However there is a clear trend in the residuals plot as it tends to have non constant variance 
# and is heteroscedastic; so this is clearly not the best model and there is scope of improvement
# to make the model better, we can include inverse/squared terms in our regression and check
# if we get a model with no/little trend in the residuals

# Part 3
# 3.	Using data up to 2005, develop a better trend specification. Report your trend estimates and 
# explain what your specification is and what makes it better.
datas1.2005$CO2.ppm.sq <- datas1.2005$CO2.ppm^2
datas1.2005$CO2.ppm.in <- 1/datas1.2005$CO2.ppm^2
datas1.2005$CO2.ppm.cube <- datas1.2005$CO2.ppm^3
datas1.2005$time <- I(1:length(datas1.2005$CO2.ppm))
datas1.2005$time.sq <- I(datas1.2005$time^2)
datas1.2005$time.cube <- I(datas1.2005$time^3)

str(datas1.2005)
reg2=lm(CO2.ppm.in~time, data=datas1.2005)
summary(reg2)
r1 <- reg2$residuals
plot.ts(r1)
Box.test(reg2$residuals)

# The residual plot indicates having no trend and all the coefficients estimates are statistically significant
# as the p values are less than .05 . The r squared value is improved when compared with the first model.
# Trend Estimates -

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  1.023e-05  9.826e-09  1041.2   <2e-16 ***
#   time        -5.696e-09  2.956e-11  -192.7   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 1.177e-07 on 573 degrees of freedom
# Multiple R-squared:  0.9848,	Adjusted R-squared:  0.9848 
# F-statistic: 3.714e+04 on 1 and 573 DF,  p-value: < 2.2e-16

# Our Specification is to introduce an inverse of the dependent variable i.e CO2 PPM . We earlier saw the residual plot in the 
# previous case to be inverted. So an inverted dependent variable will take care of that.

# Part 4
# 4.	Include seasonal effects and report results for your preferred model that includes both 
# trend and seasonal effects. Again, use data only up to 2005.
reg3=lm(CO2.ppm.in~time+ month, data=datas1.2005)
summary(reg3)
r1 <- reg3$residuals
plot.ts(r1)
Box.test(reg3$residuals)
## Results from this model indicate an increase in Adjusted R-Square to 99.66% from 98.48%

# Part 5
# 5.	Using data up to 2005m12, forecast future CO2 for the period 2006m01 to 2016m10. 
# Evaluate how good the forecasts from this model are. Are there any obvious problems with your 
# forecasts [hint: are the forecast errors unpredictable]?
str(datas1)
datas1.2006 <- datas1[datas1$datenew > '2005-12-01',]
datas1.2006$month <- factor(datas1.2006$month, levels=unique(datas1.2006$month))
str(datas1.2006)
datas1.2006$CO2.ppm.sq <- datas1.2006$CO2.ppm^2
datas1.2006$CO2.ppm.in <- 1/datas1.2006$CO2.ppm^2
datas1.2006$CO2.ppm.cube <- datas1.2006$CO2.ppm^3
datas1.2006$time <- c(1:length(datas1.2006$CO2.ppm))
datas1.2006$time <- datas1.2006$time+575
datas1.2006$time.sq <- I(datas1.2006$time^2)
datas1.2006$time.cube <- I(datas1.2006$time^3)

str(datas1.2006)
str(datas1.2005)
str(datas1)
# datas1.2006 <- datas1.2006[,c("time","month")]
# length(datas1.2005$CO2.ppm)
?predict.lm

# df <- as.data.frame(time(datas1.2005$CO2.ppm),datas1.2006$month)
# pred.val <- predict.lm(reg3,newdata = as.data.frame(time(datas1.2005$CO2.ppm),datas1.2006[,c("month")])

pred.val <- predict(reg3,newdata = datas1.2006[,c("time","month")])

plot(datas1.2005$datenew, 1/(datas1.2005$CO2.ppm.in)^0.5, xlab = 'time', ylab = 'CO2 -PPM', type='l',col='green')
lines(datas1.2006$datenew, 1/(pred.val)^0.5,col='red')
plot(datas1.2006$datenew, 1/(pred.val)^0.5, col='red',type='l')

predicted.error <- 1/(datas1.2006$CO2.ppm.in)^0.5- 1/(pred.val)^0.5
plot.ts(predicted.error) 

# the errors are almost constant .though it has in increasing trend towards the later part of the 
# predicted months

library(forecast)

accuracy(1/(datas1.2006$CO2.ppm.in)^0.5,1/(pred.val)^0.5)
plot(datas1.2006$datenew, 1/(datas1.2006$CO2.ppm.in)^0.5, xlab = 'time', ylab = 'CO2 -PPM', type='l',col='green')
lines(datas1.2006$datenew, 1/(pred.val)^0.5,col='red')

# The predictions has been close for the most of the months and the RMSE for the predicted data is 2.4  
# The months that has low CO2 PPM has the highest deviation in prediction
?accuracy

# Part 6

# We believe that Population of Hawaai could be an important feature that could explain the variance in the data
# Therefore we donwloaded the population data from this Source -
browseURL("https://united-states.reaproject.org/analysis/comparative-trends-analysis/population/reports/150000/0/")
library(tidyverse)
Hawaii_population = read.xlsx('Hawaai_Population.xlsx',1);
datas1.2006$Year <- year(datas1.2006$datenew)
datas1.2006.new <- datas1.2006 %>% left_join(Hawaii_population,by="Year") 

datas1.2005$Year <- year(datas1.2005$datenew)
datas1.2005.new <- datas1.2005 %>% left_join(Hawaii_population,by="Year") 

reg4=lm(CO2.ppm.in~time+ month + Population, data=datas1.2005.new)
summary(reg4)
r1 <- reg4$residuals
plot.ts(r1)
Box.test(reg4$residuals)

pred.val.new <- predict(reg4,newdata = datas1.2006.new[,c("CO2.ppm.in","time","month","Population")])

plot(datas1.2005.new$datenew, 1/(datas1.2005.new$CO2.ppm.in)^0.5, xlab = 'time', ylab = 'CO2 -PPM', type='l',col='green')
lines(datas1.2006.new$datenew, 1/(pred.val.new)^0.5,col='red')
plot(datas1.2006.new$datenew, 1/(pred.val.new)^0.5, col='red',type='l')

predicted.error <- 1/(datas1.2006.new$CO2.ppm.in)^0.5- 1/(pred.val)^0.5
plot.ts(predicted.error) 

# the errors are almost constant .though it has in increasing trend towards the later part of the 
# predicted months

library(forecast)
accuracy(1/(datas1.2006$CO2.ppm.in)^0.5,1/(pred.val)^0.5)
accuracy(1/(datas1.2006.new$CO2.ppm.in)^0.5,1/(pred.val.new)^0.5)
plot(datas1.2006.new$datenew, 1/(datas1.2006.new$CO2.ppm.in)^0.5, xlab = 'time', ylab = 'CO2 -PPM', type='l',col='green')
lines(datas1.2006.new$datenew, 1/(pred.val.new)^0.5,col='red')

# As we can see that introducing the Population of Hawaai as one of the component, we are able 
# to generate better prediction accuracy even on the Test/unseen data. The RMSE has gone down from
# 2.4 to 2.11, a drop of ~16%.

# Part 7

No.of.mon.frm.2005 <- 12*(2020-2005) +575
December.2020 <- data.frame(time = No.of.mon.frm.2005, month = '12')

December.2020.CO2.ppm <- 1/(predict(reg3,newdata = December.2020))^0.5
December.2020.CO2.ppm
# The prediction for December 2020 is 410.76
# This forecast might not be accurate due to the increasing error term in the model that needs to be addressed
