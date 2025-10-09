####Lab 1: Linear regression with time series data####
###Libraries##################################
library(readxl)
library(lmtest) 
library(forecast)
library(fpp2)
##############################################

##some plots for time series##################
plot(a10, ylab="million dollars", xlab="Year", main="Antidiabetic drugs")

tsdisplay(a10) ##a general view of the data

seasonplot(a10, ylab="million dollars", xlab="Year", main="Seasonal plot: Antidiabetic drugs", year.labels=T, year.labels.left=T, col=1:20, pch=19)


###Linear regression for time series 
#Facebook example

###read the data
setwd("/home/walterjtv/Escritorio/College/Q11/BEFD-Business-Economical-and-Financial-Data/LAB/SESSION1")
facebook<- read_excel("facebook.xlsx")
str(facebook)
##create a variable 'time'
tt<- 1:NROW(facebook)

#create the variable 'fb'
fb <- facebook$fb

##make a plot
plot(tt, fb, xlab="Time", ylab="Facebook users")

##acf of variable "fb"
acf(fb)
acf(fb)

##fit a linear regression model 
fit1 <- lm(fb~ tt)
summary(fit1)
anova(fit1)

##plot of the model
plot(tt, fb, xlab="Time", ylab="Facebook users")
abline(fit1, col=3)

##check the residuals? are they autocorrelated? Test of DW
dwtest(fit1)

##check the residuals
resfit1<- residuals(fit1)
plot(resfit1,xlab="Time", ylab="residuals" )


##we can do the same with a linear model for time series, so we transform the data into a 'ts' object
fb.ts <- ts(fb, frequency = 4)
ts.plot(fb.ts, type="o")

fb.ts
## we fit a linear model with the tslm function
fitts<- tslm(fb.ts~trend + season)

###obviously, it gives the same results as the first model
summary(fitts)

dwtest(fitts)


#################################### 
##Linear regression for iMac

apple<- read_excel("apple.xlsx")
str(apple)
imac <- apple$iMac
#data visualization
plot(imac,type="l", xlab="quarter", ylab="iMac sales")

#variable tt for a linear model 
tt<- 1:NROW(apple)

# linear model
fit2 <- lm(imac~tt)
summary(fit2)

plot(imac,type="l", xlab="quarter", ylab="iMac sales")
abline(fit2, col=3)

# Durbin-Watson test
# Still the value is tiny so, there is positive autocorrelation between residuals
# the model still does not capture correctly everything.
dwtest(fit2)

###check the residuals
res2<- residuals(fit2)
plot(res2, xlab="quarter", ylab="residuals", type="l")

acf(res2)


#data transformed as time series
mac.ts<-ts(imac, frequency=4)

#Model with trend and seasonality
fit3 <- tslm(mac.ts~ trend+season)
summary(fit3)

#check the residuals
res3 <- residuals(fit3)

plot(res3, ylab="residuals")
dwtest(fit3)

###plot of the model
plot(mac.ts, ylab="iMac sales", xlab="Time")
lines(fitted(fit3), col=2)


#####
################################################
####Linear regression with trend and seasonality and forecasting exercise 

#### Y = b0 + b1 * x1 + b2 * x2 (price of beer)
#### We cannot forecast demand without X2 (explanatory variable)
#### I don't have the real information
#### I can approximate this X2 using another model or 


#####################################
###data on Australian beer production
#####################################

beer<- ausbeer
beer
plot(beer)
Acf(beer)


#take a portion of data and fit a linear model with tslm
beer1<- window(ausbeer, start=1992, end=2006 -.1)
beer1
plot(beer1)
m1<- tslm(beer1~ trend+ season)
summary(m1)
fit<- fitted(m1)

plot(beer1)
lines(fitted(m1), col=2)


fore <- forecast(m1)
plot(fore)
##forecasts from regression model for beer production, The dark shaded region shows 80% prediction intervals and the light shaded 95% prediction intervals (range of values the random variable could take with relatively high probability). 

#analysis of residuals
res<- residuals(m1) 
# We don't see much pattern but some of it there is, however more rigor needs to be 
# applied for assessing information in residuals, using ACF 
plot(res) 
# There are autocorrelations in the data!
#the form of residuals seems to indicate the presence of negative autocorrelation
Acf(res)

####################################################################################################
####Data on quarterly percentage change in US consumption, income, production, savings, unemployment
####################################################################################################

uschange<- uschange
str(uschange)
plot(uschange)
autoplot(uschange) 
pairs(uschange)
#different way of seeing the same series

cons<- uschange[,1]
inc<- uschange[,2]
prod<- uschange[,3]
sav<- uschange[,4]
unem<- uschange[,5]


#### Consider the series of consumption as the dependent variable and study with the other explanatory variables in a multiple regression model
fit.cons<- tslm(cons~inc+prod+sav+unem)
summary(fit.cons)
AIC(fit.cons)

plot(cons)
lines(fitted(fit.cons), col=2)

res<- residuals(fit.cons)
plot(res)
acf(res)
Acf(res) #note the difference

## We remove the 'production' variable  
fit.cons1<- tslm(cons~inc+sav+unem)
summary(fit.cons1)


##Exercise with scenario hypotheses 
## fit the model again (by using the data differently)

fit.consBest <- tslm(
  Consumption ~ Income + Savings + Unemployment,
  data = uschange)
  
h <- 4 
##window for prediction

## We assume a constant increase of 1 and 0.5% for income and savings, and no change for unemployment

newdata <- data.frame(
    Income = c(1, 1, 1, 1),
    Savings = c(0.5, 0.5, 0.5, 0.5),
    Unemployment = c(0, 0, 0, 0))

##forecasts
fcast.up <- forecast(fit.consBest, newdata = newdata)

plot(fcast.up)

####we assume a constant decrease of 1 and 0.5% for income and savings and no change for unemployment

newdata <- data.frame(
    Income = rep(-1, h),
    Savings = rep(-0.5, h),
    Unemployment = rep(0, h))
fcast.down <- forecast(fit.consBest, newdata = newdata)

plot(fcast.down)


################################################################################################
################################################################################################
#################################################################################################
###Boston Data#####
##Exercise on multiple linear regression##

library(MASS)
library(ISLR2)

###
head(Boston)
###

## Multiple Linear Regression

###
lm.fit <- lm(medv ~ lstat + age, data = Boston)
summary(lm.fit)
###
lm.fit <- lm(medv ~ ., data = Boston)
summary(lm.fit)
###
library(car)
vif(lm.fit)
###
lm.fit1 <- lm(medv ~ . - age, data = Boston)
summary(lm.fit1)
###
lm.fit1 <- update(lm.fit, ~ . - age)



