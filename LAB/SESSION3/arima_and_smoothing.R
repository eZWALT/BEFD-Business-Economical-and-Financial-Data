####Lab 3: ARIMA models, Exponential Smoothing####

#############################################################################
#############################################################################
library(fpp2)
library(forecast) 
?fpp2


#########################################################################
#####ARIMA models
#########################################################################
####1. Data on quarterly percentage change in US consumption, income, production, savings, unemployment

uschange<- uschange
str(uschange)
plot(uschange)
autoplot(uschange) 
#different way of seeing the same series

####consider the series of consumption
cons<- uschange[,1]
plot(cons)
Acf(cons)
Pacf(cons)
consts<- tsdisplay(cons)
###General indication: if the ACF is exponentially decaying or sinusoidal and there is a significant spike at lag p in PACF and nothing else, 
##it may be an ARMA(p,d,0). If the PACF is exponentially decaying or sinusoidal and there is a significant spike at lag p in ACF and nothing else, it may be an ARMA(0,d,q). 


arima1<- Arima(cons, order=c(0,0,3))
summary(arima1)

resid1<- residuals(arima1)
tsdisplay(resid1)


plot(cons)
lines(fitted(arima1), col=2)

for1<- forecast(arima1)
plot(for1)

######Exercise: try other options and with the different series in the dataset


##############################################################
##########2. Data on retail trade index in Euro area (1996-2011)
plot(euretail, ylab="retail index",xlab="year")
tsdisplay(euretail)

##first difference
diff1<- diff(euretail) 
###seasonal difference
diff4<- diff(euretail, lag=4) 
tsdisplay(diff1)
tsdisplay(diff4)


####first Arima model 
a1<- Arima(euretail, order=c(0,1,1), seasonal=c(0,0,1))
fit1<- fitted(a1)

plot(euretail)
lines(fit1, col=2)

f1<- forecast(a1)
plot(f1)

r1<- residuals(a1)
tsdisplay(r1) 


#####second Arima model
a2<- Arima(euretail, order=c(0,1,1), seasonal=c(0,0,2))
fit2<- fitted(a2)

plot(euretail)
lines(fit2, col=2)

f2<- forecast(a2)
plot(f2)

r2<- residuals(a2)
tsdisplay(r2) 


#######third Arima model
a3<- Arima(euretail, order=c(0,1,1), seasonal=c(0,1,1))
fit3<- fitted(a3)

plot(euretail)
lines(fit3, col=2)

f3<- forecast(a3)
plot(f3)

r3<- residuals(a3)
tsdisplay(r3) 

##########fourth Arima model 

a4<- Arima(euretail, order=c(0,1,2), seasonal=c(0,1,1))
fit4<- fitted(a4)

plot(euretail)
lines(fit4, col=2)

f4<- forecast(a4)
autoplot(f4)

r4<- residuals(a4)
tsdisplay(r4) 

############fifth Arima model 

auto.a<- auto.arima(euretail)
auto.a

plot(forecast(auto.a))
autoplot(forecast(auto.a)) #another way to inspect the same plot
checkresiduals(auto.a)

#####Exercise: Try other solutions for modeling this series

#########################################
####3.Cortecosteroid drug sales in Australia 
lh02<- log(h02)

##plot the data
plot(h02, ylab="sales", xlab="year")

##plot log transformation
plot(lh02, ylab="logsales", xlab="year")


##plot of seasonal differentiated data, with ACF and PACF
tsdisplay(diff(lh02,12), main="seasonal differenced data", xlab="year")

##We fit an ARIMA model based on the inspection of ACF and PACF with 3 AR components
fit<- Arima(lh02, order=c(3,0,1),seasonal=c(0,1,2))
summary(fit)

##Check residuals
tsdisplay(residuals(fit))


##perform the forecasting
f<- forecast(fit)

plot(f, ylab="sales", xlab="year")

##auto.arima?


###Air Passengers data

##Plot the data 
plot(AirPassengers)
tsdisplay(AirPassengers)

#m<- auto.arima(AirPassengers)

## It is possible to split the data in two parts (train and test)

# Fit model to first few years of AirPassengers data
air.model <- Arima(window(AirPassengers,end=1956+11/12),order=c(0,1,1),
                   seasonal=list(order=c(0,1,1),period=12))
plot(forecast(air.model,h=48))
lines(AirPassengers)

# Apply fitted model to later data
air.model2 <- Arima(window(AirPassengers,start=1957),model=air.model)

# in-sample one-step forecasts
accuracy(air.model)
# out-of-sample one-step forecasts
accuracy(air.model2)

##############################################################################
##############################################################################
##############################################################################
##Exponential Smoothing methods

##1.Simple exponential smoothing
oildata<- window(oil, start=1996)
plot(oildata, ylab="Oil", xlab="Year")

fit1<- ses(oildata, alpha=0.2, initial="simple", h=5)
fit2<- ses(oildata, alpha=0.6, initial="simple", h=5)
fit3<- ses(oildata, h=5)


plot(oildata, ylab="Oil", xlab="Year")
lines(fitted(fit1), col="blue", type="o")
lines(fitted(fit2), col="red", type="o")
lines(fitted(fit3), col="green", type="o")

fc<- ses(oildata, h=5)
round(accuracy(fc), 2)

summary(fc)

plot(fc)


##2.Trend methods (Holt method)

air<- window(ausair, start=1990, end=2004)
fit1<- holt(air, alpha=0.8, beta=0.2, initial="simple",h=5)
fit2<- holt(air, damped=T, phi=0.9, h=5)

f<- holt(air, h=5)

plot(forecast(fit1))
plot(forecast(fit2))
plot(forecast(f))

###3.Trend and seasonality methods (Holt-Winters method)
aust<- window(austourists, start=2005)
plot(aust)

fit1<- hw(aust, seasonal="additive")
fit2<- hw(aust, seasonal="multiplicative")

plot(fit1)
plot(fit2)


