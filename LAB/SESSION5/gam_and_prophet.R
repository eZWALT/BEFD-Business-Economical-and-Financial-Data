####Lab 5: Generalized Additive Models (GAM), Prophet model########


########################################
######Generalized Additive Models#######
########################################

##1. Quarterly international arrivals (in thousands) to Australia from Japan, New Zealand, UK and the US. 1981Q1 - 2012Q3
library(fpp2)

?arrivals
autoplot(arrivals)
autoplot(arrivals[,c(1,2)])

Japan<- arrivals[,1]
NZ<- arrivals[,2]
UK<- arrivals[,3]
US<- arrivals[,4]



######linear regression

tt<- (1:length(NZ))
seas <- factor(c(rep(1:4,length(NZ)/4),1:3)) 
#####1:3 because there are three observations 'out' 
mod2 <- lm(NZ~ tt+seas)
summary(mod2)
AIC(mod2)


###linear model with no seasonality
mod2a <- lm(NZ~ tt)
summary(mod2a)

#######we now try with a GAM
library(gam)
?s 

#Values for df should be greater than 1, with df=1 implying a linear fit. Default is df=4
g1 <- gam(NZ~s(tt)+seas)
summary(g1)

par(mfrow=c(1,2))
plot(g1, se=T)
AIC(g1)

#######time has a nonlinear effect

g1a <- gam(NZ~s(tt))
par(mfrow=c(1,2))
plot(g1a, se=T)
summary(g1a)



####linear model may be also performed with library(gam)

### Here if the model is linear the result then ---> NO NON-PARAMETRIC ANOVA EFFECTS
g0 <- gam(NZ~(tt)+seas)
summary(g0)
par(mfrow=c(1,2))
plot(g0, se=T)
AIC(g0)

####GAM with splines performs better###

####try another option with loess (lo)
g2<- gam(NZ~lo(tt)+seas)
summary(g2)
par(mfrow=c(1,2))
plot(g2, se=T)
AIC(g2)


### DIFFICULT TO SEE A PATTERN (PERIODICTY AND TRENDS VISIBLE THOUGH) BUT FOR SURE ITS NOT WHITE NOISE
### WE SHOULD HANDLE THE TREND, ITS NOT COMPLETELY STATIONARY YET
#######perform analysis of residuals
tsdisplay(residuals(g1))
aar1<- auto.arima(residuals(g1))


# BIG SCALE and SMALL SCALE VARIATIONS, with this arima + linear residuals we will be able to capture both
# But this can also fit to the noise of the data... So overfitting is common
plot(as.numeric(NZ), type="l")
# This model is less complex so its preferable
lines(fitted(g1), col=3)
# The combined model can be used for prediction not only explanation, but 
lines(fitted(aar1)+ fitted(g1), col=4)

###Exercise: try also with the other variables 

#######################################################################
####Prophet####
###############
library(prophet)

##2. iPhone sales
##data 
setwd("/home/walterjtv/Escritorio/College/Q11/BEFD-Business-Economical-and-Financial-Data/LAB/SESSION5")
iphone=read.csv("iphone.csv", sep=";")
str(iphone)

##some preliminary analysis on the data
iphone$quarter=as.Date(iphone$quarter, format="%d/%m/%Y")
str(iphone)
colnames(iphone)=c("y","ds","cap")

summary(iphone$y) ##to explain why cap is 100

#plot
plot(iphone$y, type="l", x=iphone$ds, ylab="", xlab="Year") 
#nonlinear trend 
#seasonality

#Prophet model
# #model with no seasonality
# m2=prophet(iphone, yearly.seasonality=F, daily.seasonality=F, weekly.seasonality = F, growth="logistic", n.changepoints=15)


#model with automatic selection of seasonality (default is "auto")
#in this case just yearly seasonality, if daily and weekly seasonsonality are needed we need to specify
##changepoints not specified, automatically set
m2=prophet(iphone,  growth="logistic") 

#n.changepoints default number is 25
summary(m2)
 
##create a future 'window' for prediction
future2 <- make_future_dataframe(m2, periods = 20, freq="quarter", include_history = T)
tail(future2)
future2$cap=100

forecast2 <- predict(m2, future2)
tail(forecast2[c("ds", "yhat", "yhat_lower", "yhat_upper")])

#prediction plot

plot(m2, forecast2)

#Dynamic plot
dyplot.prophet(m2, forecast2) 

#plot with change points

plot(m2, forecast2)+add_changepoints_to_plot(m2, threshold=0)

#dates corresponding to change points
m2$changepoints

##Prophet with no change points and multiplicative seasonality 
m2=prophet(iphone,  growth="logistic", n.changepoints=0, seasonality.mode='multiplicative')

#summary(m2)
#m2$seasonalities 


future2 <- make_future_dataframe(m2, periods = 20, freq="quarter", include_history = T)
tail(future2)
future2$cap=100

forecast2 <- predict(m2, future2)
tail(forecast2[c("ds", "yhat", "yhat_lower", "yhat_upper")])

#prediction plot

plot(m2, forecast2)
dyplot.prophet(m2, forecast2) 



### DIFFERENCES BETWEEN BASS AND PROPHET IN THIS DATA:
# 1. Predictions are smoother 
# 2. With bass we assume its going to follow a structure over time...
# so bass model is cumulative data and prophet instateneous
################################################################
###Generalized Additive Models######
###############################################
##3. 'Movies' Dataset
##preliminary analysis

data <- read.csv("movies.csv", stringsAsFactors=TRUE)
str(data)
#erase columns of indicator variables
data<-data[,-c(1,2)]

#transform variable release_date in format "data"
data$release_date <- as.Date(data$release_date, "%d/%m/%Y")

str(data)

# Response variable
summary(data$vote_average)
#boxplot(data$vote_average, col="orange", ylim=c(0,10), main="Movies", ylab="Rating")
#
hist(data$vote_average, col="orange", main="Movies", xlab="Rating")
#
#explanatory variables
summary(data)


summary(data[,c(1,2,5,6)])
par(mfrow=c(2,2))
for(i in c(1,2,5,6)){
  hist(data[,i], col="orange", main=paste(colnames(data)[i]), xlab="")
}

#transform quantitative variables in log scale (To make them more symmetric and scaled)
data$budget <- log(data$budget)
data$popularity <- log(data$popularity)
data$revenue <- log(data$revenue)

summary(data[,c(1,2,5,6)])

par(mfrow=c(2,2))
for(i in c(1,2,5,6)){
  hist(data[,i], col="orange", main=paste(colnames(data)[i]), xlab="")
}
#go back to orginal panel 
par(mfrow=c(1,1))

#transform release_date in numeric 
data$release_date<-as.numeric(data$release_date)
summary(data$release_date)

#as.numeric(as.Date("1970-01-01"))


# Set train and test
set.seed(1)
train = sample (1:nrow(data), 0.7*nrow(data))
data.train=data[train ,]
data.test=data[-train ,]

# make some variables factor
data.train[,c(3,7, 10:24)]= lapply(data.train[,c(3,7, 10:24)],factor)
data.test[,c(3,7, 10:24)]= lapply(data.test[,c(3,7, 10:24)],factor)

str(data.train)

####Linear Model######################
# THEY ARE HIGHLY CORRELATED SO WE MUST ELIMINATE IT FROM THE MODEL 
m1 <- lm(vote_average~.-vote_classes, data=data.train)

summary(m1)

# Stepwise Regression (AIC criterion), both senses means adding and subtracting variables
m2 <- step(m1, direction="both")
summary(m2)

#Prediction
p.lm <- predict(m2, newdata=data.test)
dev.lm <- sum((p.lm-data.test$vote_average)^2)
dev.lm

AIC(m2)

#############################
##GAM#########################

library(gam)

###################
##Stepwise GAM
#Start with a linear model (df=1)
g3 <- gam(vote_average~.-vote_classes, data=data.train)

#Show the linear effects 
par(mfrow=c(3,5))
plot(g3, se=T) 

#Perform stepwise selection using gam scope
#Values for df should be greater than 1, with df=1 implying a linear fit

sc = gam.scope(data.train[,-8], response=8, arg=c("df=2","df=3","df=4"))
g4<- step.Gam(g3, scope=sc, trace=T)
summary(g4)

AIC(g4)

par(mfrow=c(3,5))
plot(g4, se=T)

# if we want to see better some plot
par(mfrow=c(1,1))
plot(g4, se=T, ask=T)


#Prediction
data.test[,c(3,7, 10:24)]= lapply(data.test[,c(3,7, 10:24)],factor)
p.gam <- predict(g4,newdata=data.test)     
dev.gam <- sum((p.gam-data.test$vote_average)^2)
dev.gam


################################################################
######Comparison between GAM and Prophet
#####4. Airline Passengers######################################

#install.packages("gam")

library(gam)

# Dataset 
data("AirPassengers")
ap <- AirPassengers

x <- 1:144   # sequence of months

date <- as.Date("1949-01-01") + months(x - 1)
date



df <- data.frame(
  ds = date,
  y  = as.numeric(ap))


df$month <- as.numeric(format(df$ds, "%m"))
df$t     <- seq_along(df$y)



# GAM model
model_gam <- gam(
  y ~ s(t) + s(month),
  data = df)

summary(model_gam)

# Predictions
df$pred_gam <- predict(model_gam)

#plot predictions
plot(df$ds, df$y, type="l", col="black", lwd=2,
     main="GAM (library gam) - AirPassengers",
     xlab="", ylab="Passengers")

lines(df$ds, df$pred_gam, col="blue", lwd=2)
legend("topleft", legend=c("Observed","Predicted"), col=c("black","blue"), lwd=2)


#install.packages("prophet")
library(prophet)

df_prophet <- data.frame(ds=df$ds, y=df$y)

m_prophet <- prophet(df_prophet)

future <- make_future_dataframe(m_prophet, periods = 24, freq = "month")
forecast <- predict(m_prophet, future)

# Plot (standard Prophet plot)
plot(m_prophet, forecast)


# Plot, comparison between models
N <- nrow(df)  
df$prophet_pred <- forecast$yhat[1:N]

plot(df$ds, df$y, type="l", col="black", lwd=2,
     main="GAM (gam) vs Prophet",
     xlab="", ylab="Passengers")

lines(df$ds, df$pred_gam, col="blue", lwd=2)
lines(df$ds, df$prophet_pred, col="red", lwd=2)

legend("topleft",
       legend=c("Observed", "GAM", "Prophet"),
       col=c("black","blue","red"),
       lwd=2)



