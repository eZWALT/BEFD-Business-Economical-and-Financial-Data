####Lab 4: ARMAX models#######
################################################################
library(forecast)
library(fpp2)
library(DIMORA)


################################################################
#####1. data on US personal consumption and income
#### We want to extend the ARIMA by combining the regression model and the ARIMA model to obtain a regression with ARIMA errors

uschange
plot(uschange) ##'classical view'
autoplot(uschange) ## 'all the series together'

##if we want to see just the first two series
par(mfrow=c(2,1))
plot(uschange[,1])
plot(uschange[,2])

tsdisplay(uschange[,1])

##to go back to 1 panel 
par(mfrow=c(1,1))


##estimate an ARMAX model
armax1<- Arima(uschange[,1], xreg=uschange[,2], order=c(1,0,1))
res1<- residuals(armax1)
Acf(res1)

fitted(armax1)
plot(uschange[,1])
lines(fitted(armax1), col=2)

armax2<- Arima(uschange[,1], xreg=uschange[,2], order=c(1,0,2))
res2<- residuals(armax2)
Acf(res2)

fitted(armax2)
plot(uschange[,1])
lines(fitted(armax2), col=2)
AIC(armax2)

####Note: forecasts are available only if we have future values of 'income'

########procedure also available with auto.arima
auto.arima<- auto.arima(uschange[,1], xreg=uschange[,2])


####2. Lagged Predictors
#Insurance quotations and advertising data

plot(insurance, main="Insurance advertising and quotations", xlab="year", xaxt="n")
axis(1, at=c(2002,2003,2004,2005),labels=c("2002","2003","2004","2005"), line=-1, lwd=0, lwd.ticks=0.5)

## We consider 1 lagged predictor
advert<- cbind(insurance[,2], c(NA,insurance[1:39,2]))
colnames(advert)<- paste("AdLag",0:1, sep="")
fit<- auto.arima(insurance[,1], xreg=advert[,1:2],d=0)
summary(fit)

plot(insurance[,1])
lines(fitted(fit), col=2)

checkresiduals(fit)

## We make the forecast by assuming that advertising is 8 units in each future month
f<-forecast(fit,h=20, xreg=cbind(AdLag0=rep(8,20),AdLag1=c(advert[40,1],rep(8,19))))
plot(f)


####################################
###3. Forecasting electricity demand
elecdaily
plot(elecdaily)

##We fit a quadratic regression model with ARMA errors using the auto.arima function
xreg <- cbind(MaxTemp = elecdaily[, "Temperature"],
              MaxTempSq = elecdaily[, "Temperature"]^2,
              Workday = elecdaily[, "WorkDay"])
fit <- auto.arima(elecdaily[, "Demand"], xreg = xreg)
summary(fit)

plot(elecdaily[, "Demand"])
lines(fitted(fit), col=2)

checkresiduals(fit)
##The model has some significant autocorrelation in the residuals

## Using the estimated model, we forecast 14 days ahead, setting the temperature for the next 14 days at a constant level of 26.
fcast <- forecast(fit,
  xreg = cbind(MaxTemp=rep(26,14), MaxTempSq=rep(26^2,14),
    Workday=c(0,1,0,0,1,1,1,1,1,0,0,1,1,1)))

plot(fcast)




#############################################################################
####4. SARMAX refinement for diffusion models
### Twitter Revenues

library(readxl)
library(DIMORA)
###1. Twitter Revenues

twitter<- read_excel("twitter.xlsx")
length(twitter$twitter)
tw<- (twitter$twitter)


######GGM 
GGM_tw<- GGM(tw, prelimestimates=c(4.463368e+04, 0.001, 0.01, 1.923560e-03, 9.142022e-02))
summary(GGM_tw)

pred_GGM_tw<- predict(GGM_tw, newx=c(1:60))
pred_GGM_tw.inst<- make.instantaneous(pred_GGM_tw)

plot(tw, type= "b",xlab="Quarter", ylab="Quarterly revenues",  pch=16, lty=3, cex=0.6, xlim=c(1,60))
lines(pred_GGM_tw.inst, lwd=2, col=2)


###Analysis of residuals
res_GGMtw<- residuals(GGM_tw)
acf<- acf(residuals(GGM_tw))


fit_GGMtw<- fitted(GGM_tw) ##they are cumulative values
fit_GGMtw_inst<- make.instantaneous(fit_GGMtw)


####SARMAX refinement
library(forecast)
####SARMAX model with external covariate 'fit_GGM' 
s2 <- Arima(cumsum(tw), order = c(3,0,1), seasonal=list(order=c(3,0,1), period=4),xreg = fit_GGMtw)
summary(s2)
pres2 <- make.instantaneous(fitted(s2))


plot(twitter$twitter, type= "b",xlab="Quarter", ylab="Quarterly revenues",  pch=16, lty=3, xaxt="n", cex=0.6)
axis(1, at=c(1,10,19,28,37,46), labels=twitter$quarter[c(1,10,19,28,37,46)])
lines(fit_GGMtw_inst, lwd=1, lty=2)
lines(pres2, lty=1,lwd=1, col=2)

####Interest in Zoom according to searches in Google ####
####
#
interest<- read.csv("zoomgoogle.csv")
int<- interest$zoom[90:180]
week <- as.Date(interest$week[90:180])

#
##Exploratory plots
plot(int, type= "b",xlab="Week", ylab="Weekly Google searches",  pch=16, lty=3, cex=0.6, xaxt="n")
axis(1, at=c(1,18,36,54,72,90), labels=format(week[c(1,18,36,54,72,90)], "%d/%m/%y"))
#
#
plot(cumsum(int), type= "b",xlab="Week", ylab="Cumulative Google searches",  pch=16, lty=3, cex=0.6, xaxt="n")
axis(1, at=c(1,18,36,54,72,90), labels=format(week[c(1,18,36,54,72,90)], "%d/%m/%y"))
#
#
##GBM with one exponential shock
gbme1.go<- GBM(int,shock = "exp",nshock = 1,prelimestimates = c(4.046997e+03, 0.001, 0.1, 30,-01,5))
#
##Prediction with GBMe1
pred.gbme1go<- predict(gbme1.go, newx=c(1:150))
pred.gbme1goi<- make.instantaneous(pred.gbme1go)
#
#
##Plots observed vs predicted
plot(int, type= "b",xlab="Week", ylab="Weekly Google searches",  pch=16, lty=3, cex=0.6, xaxt="n", col=1)
axis(1, at=c(1,18,36,54,72,90), labels=format(week[c(1,18,36,54,72,90)], "%d/%m/%y"))
lines(pred.gbme1goi, lwd=2, col=2)
#
#
plot(cumsum(int), type= "b",xlab="Week", ylab="Cumulative Google searches",  pch=16, lty=3, cex=0.6, xaxt="n",col=1)
axis(1, at=c(1,18,36,54,72,90), labels=format(week[c(1,18,36,54,72,90)], "%d/%m/%y"))
lines(pred.gbme1go, lwd=2, col=2)
#
#
##SARMAX refinement
library(forecast)
#
fit.google<- fitted(gbme1.go)
s2 <- Arima(cumsum(int), order = c(1,0,1), seasonal=list(order=c(0,0,1), period=52), xreg = fit.google)
summary(s2)
#
pres2 <- make.instantaneous(fitted(s2))
#
##Plots observed vs predicted with SARMAX refinement
plot(int, type= "b",xlab="week", ylab="Weekly Google searches",  pch=16, lty=3, cex=0.6, xaxt="n", col=1)
axis(1, at=c(1,18,36,54,72,90), labels=format(week[c(1,18,36,54,72,90)], "%d/%m/%y"))
lines(pred.gbme1goi, lwd=1, lty=2, col=2)
lines(pres2, lty=1,lwd=1, col=3)
legend("topright", legend=c("GBMe1","GBMe1+SARMAX"), lty=c(2,1))
#
plot(cumsum(int), type= "b",xlab="week", ylab="Cumulative Google searches",  pch=16, lty=3, cex=0.6, xaxt="n", col=1)
axis(1, at=c(1,18,36,54,72,90), labels=format(week[c(1,18,36,54,72,90)], "%d/%m/%y"))
lines(pred.gbme1go, lwd=1, lty=2, col=2)
lines(cumsum(pres2), lty=1,lwd=1, col=3)
legend("bottomright", legend=c("GBMe1","GBMe1+SARMAX"), lty=c(2,1))




######################################################################################################
##################
#######6. Quarterly international arrivals (in thousands) to Australia from Japan, New Zealand, the UK and the US. 1981Q1 - 2012Q3
?arrivals
autoplot(arrivals)
autoplot(arrivals[,c(1,2)])

Japan<- arrivals[,1]
NZ<- arrivals[,2]
UK<- arrivals[,3]
US<- arrivals[,4]

####we try with a simple arima model, ARMAX
auto.a<- auto.arima(NZ, xreg=Japan) 
AIC(auto.a)

###### We try with a regression model with trend, season, and external variable 'Japan'
mod<- tslm(NZ~ trend+season+Japan) 
summary(mod)
fitted(mod)
plot(NZ)
lines(fitted(mod), col=2)
plot(residuals(mod))

#####analysis of residuals: autocorrelation? 
tsdisplay(residuals(mod))
#####fit an arima model to residuals
aar<- auto.arima(residuals(mod))
fitted(aar)

#######complete the analysis by summing predictions made with linear model and arma on residuals
plot(NZ)
lines(fitted(mod)+fitted(aar), col=3)

######Notice the difference between the two methods: ARMAX and linear regression+ Arima on residuals

######Another way of performing the same linear regression

tt<- (1:length(NZ))
seas <- factor(c(rep(1:4,length(NZ)/4),1:3)) #1:3 because there are three observations 'out' 
mod2 <- lm(NZ~ tt+seas+Japan)
summary(mod2)
AIC(mod2)
AIC(mod)

