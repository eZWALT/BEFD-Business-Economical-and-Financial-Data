####Lab 2: Nonlinear regression####

###Libraries##################################
library(readxl)
library(DIMORA)
##############################################
setwd("/home/walterjtv/Escritorio/College/Q11/BEFD-Business-Economical-and-Financial-Data/LAB/SESSION2")


#############################################################
#############################################################
####Nonlinear models for new product growth (diffusion models)
#############################################################

###Music data (RIAA)
music<- read_excel("music.xlsx")
str(music)

##create the variable cassette
cassette<- music$cassette[1:36]

###some simple plots
plot(cassette, type="b")
plot(cumsum(cassette), type="b")

###a better plot of the yearly time series
plot(cassette, type= "b",xlab="Year", ylab="Annual sales",  pch=16, lty=3, xaxt="n", cex=0.6)
axis(1, at=c(1,10,19,28,37), labels=music$year[c(1,10,19,28,37)])

###we estimate a simple Bass Model 
bm_cass<-BM(cassette,display = T)
summary(bm_cass)

###prediction (out-of-sample)
pred_bmcas<- predict(bm_cass, newx=c(1:50))
pred.instcas<- make.instantaneous(pred_bmcas)

###plot of fitted model 
plot(cassette, type= "b",xlab="Year", ylab="Annual sales",  pch=16, lty=3, xaxt="n", cex=0.6)
axis(1, at=c(1,10,19,28,37), labels=music$year[c(1,10,19,28,37)])
lines(pred.instcas, lwd=2, col=2)


###we estimate the model with 50% of the data

bm_cass50<-BM(cassette[1:18],display = T)
summary(bm_cass50)

pred_bmcas50<- predict(bm_cass50, newx=c(1:50))
pred.instcas50<- make.instantaneous(pred_bmcas50)

plot(cassette, type= "b",xlab="Year", ylab="Annual sales",  pch=16, lty=3, xaxt="n", cex=0.6)
axis(1, at=c(1,10,19,28,37), labels=music$year[c(1,10,19,28,37)])
lines(pred.instcas50, lwd=2, col=2)


###we estimate the model with 25% of the data
bm_cass75<-BM(cassette[1:9],display = T)
summary(bm_cass75)

pred_bmcas75<- predict(bm_cass75, newx=c(1:50))
pred.instcas75<- make.instantaneous(pred_bmcas75)


###Comparison between models (instantaneous)
###instantaneous
plot(cassette, type= "b",xlab="Year", ylab="Annual sales",  pch=16, lty=3, xaxt="n", cex=0.6)
axis(1, at=c(1,10,19,28,37), labels=music$year[c(1,10,19,28,37)])
lines(pred.instcas75, lwd=2, col=2)
lines(pred.instcas50, lwd=2, col=3)
lines(pred.instcas, lwd=2, col=4)


###Comparison between models (cumulative)
plot(cumsum(cassette), type= "b",xlab="Year", ylab="Annual sales",  pch=16, lty=3, xaxt="n", cex=0.6)
axis(1, at=c(1,10,19,28,37), labels=music$year[c(1,10,19,28,37)])
lines(pred_bmcas75, lwd=2, col=2)
lines(pred_bmcas50, lwd=2, col=3)
lines(pred_bmcas, lwd=2, col=4)

music
###exercise: try the same with the CD time series
#### DIFFUSSION OF CDS HAVE A BIGGER MARKET POTENTIAL, THERE IS A BIGGER MARKET.
cds <- music[10:49,0:2]
plot(cds, type="b")
plot(cumsum(cds), type="b")
bm_cds <- BM(cds$cd, display=T)
summary(bm_cds)
pred_cds <- predict(bm_cds, newx=c(1:50))
preds_inst <- make.instantaneous(pred_cds)
plot(cds, type="b", xlab= "Year", ylab="Annual sales", pch=16, lty=3, xaxt="n", cex=0.6)
lines(preds_inst, lwd=2, col=2)

## IF 0 is included in the C.I ranges of the parameters then its not statistically significant
## In this case they are all significant :)

###Twitter (revenues)

twitter<- read_excel("twitter.xlsx")
length(twitter$twitter)


plot(twitter$twitter, type= "b",xlab="Quarter", ylab="Quarterly revenues",  pch=16, lty=3, xaxt="n", cex=0.6)
axis(1, at=c(1,10,19,28,37,46), labels=twitter$quarter[c(1,10,19,28,37,46)])



###BM
tw<- (twitter$twitter)
# We want to infer trend & seasonality
# Significant autocorrelations with a increasing trend (autocorr. are ), no seasonality 
# can be easily infered from the plot. We see every 4 lags the variable is significant (although its not locally higher than its neighbor values)
# 
# 
Acf(tw)


bm_tw<-BM(tw,display = T)
summary(bm_tw)


pred_bmtw<- predict(bm_tw, newx=c(1:60))
pred.insttw<- make.instantaneous(pred_bmtw)

plot(cumsum(tw), type= "b",xlab="Quarter", ylab="Cumulative revenues",  pch=16, lty=3, cex=0.6, xlim=c(1,60), ylim=c(0,40000))
lines(pred_bmtw, lwd=2, col=2)

# Judging from this plot 
plot(tw, type= "b",xlab="Quarter", ylab="Quarterly revenues",  pch=16, lty=3, cex=0.6, xlim=c(1,60))
lines(pred.insttw, lwd=2, col=2)


###GBMr1
# Estimates of the parmaters (n, p ,q) and type of shock (rectangular) and its hyperparams
# (a, b, c) must be given an initial value.

# As preliminary estimates we use the values of the previous normal bass model
# Its a reasonable choice of initial estimates because they were estimated by the data and
# because the bass model is a nested model inside of GBM. (n, p ,q)
GBMr1tw<- GBM(tw,shock = "rett",nshock = 1,prelimestimates = c(4.463368e+04, 1.923560e-03, 9.142022e-02, 24,38,-0.1))
# Interpretation of the parameters:

######GBMe1
### THIS GMB models just are able to capture mean behaviour, not variability
### (Yes Mean, NO Variance)

GBMe1tw<- GBM(tw,shock = "exp",nshock = 1,prelimestimates = c(4.463368e+04, 1.923560e-03, 9.142022e-02, 12,-0.1,0.1))
summary(GBMe1tw)
# This model seems to capture the trend better than the last one. The 2 models represent different situations.
# Parameter b has to be negative
# parameters (m, p) are not significant because of early life cycle, the estimate of m is unstable.

pred_GBMe1tw<- predict(GBMe1tw, newx=c(1:60))
pred_GBMe1tw.inst<- make.instantaneous(pred_GBMe1tw)

plot(cumsum(tw), type= "b",xlab="Quarter", ylab="Cumulative revenues",  pch=16, lty=3, cex=0.6, xlim=c(1,60), ylim=c(0,50000))
lines(pred_GBMe1tw, lwd=2, col=2)


plot(tw, type= "b",xlab="Quarter", ylab="Quarterly revenues",  pch=16, lty=3, cex=0.6, xlim=c(1,60))
lines(pred_GBMe1tw.inst, lwd=2, col=2)


######GGM 2 processes
# COMMUNICATION: We need to estimate the initial values (magnitude like learning rate) of p_c, q_c usually 1/100 ratio
# SELLING: A good initial estimate is to simply pick the previously estimated parameters of the regular BASS MODEL
GGM_tw<- GGM(tw, prelimestimates=c(4.463368e+04, 0.001, 0.01, 1.923560e-03, 9.142022e-02))
summary(GGM_tw)
# Not all the parameters are signifcant in this model (K, p_c)

####### IMPORTANT: 
# Modelling is not a big of a deal nowadays
# What it counts is the possibility of choosing well between models and knowing its usecase
# Decision ability


pred_GGM_tw<- predict(GGM_tw, newx=c(1:60))
pred_GGM_tw.inst<- make.instantaneous(pred_GGM_tw)

plot(tw, type= "b",xlab="Quarter", ylab="Quarterly revenues",  pch=16, lty=3, cex=0.6, xlim=c(1,60))
lines(pred_GGM_tw.inst, lwd=2, col=2)


###Analysis of residuals
### Its important to analyze the residuals after fitting to see if any remaining information its still there

### Analysis: There are some significant autocorrelations and we can slightly see a cosine like pattern

### Residual modelling? Later in the course
res_GGMtw<- residuals(GGM_tw)
acf<- acf(residuals(GGM_tw))

plot(res_GGMtw)





#####################################################################################
###Let us consider the Germany energy transition case. 
#We study consumption of coal, nuclear, and renewables, until year 2019. 

##read the data
bp<- read_excel("BP1.xlsx")
str(bp)
 
###we consider coal, nuclear and renewables
GC<- bp$GermanyC[1:55]
GN<- bp$GermanyN[1:55]
GR<- bp$GermanyR[28:55] 
###eliminate some NA in the data

####Plots

plot(GC, type= "b",xlab="Year", ylab="Annual consumption",  pch=16, lty=3, xaxt="n", cex=0.6)
axis(1, at=c(1,10,19,28,37,46,55), labels=bp$year[c(1,10,19,28,37,46,55)])


plot(GN, type= "b",xlab="Year", ylab="Annual consumption",  pch=16, lty=3, xaxt="n", cex=0.6)
axis(1, at=c(1,10,19,28,37,46,55), labels=bp$year[c(1,10,19,28,37,46,55)])


plot(GR, type= "b",xlab="Year", ylab="Annual consumption",  pch=16, lty=3, xaxt="n", cex=0.6)
axis(1, at=c(1,10,19,28,37), labels=bp$year[28:56][c(1,10,19,28,37)])



#####Coal modelling

##BM
bm_GC<-BM(GC,display = T)
summary(bm_GC)


pred_bmGC<- predict(bm_GC, newx=c(1:60))
pred.instGC<- make.instantaneous(pred_bmGC)


plot(GC, type= "b",xlab="Year", ylab="Annual consumption",  pch=16, lty=3, xaxt="n", cex=0.6)
axis(1, at=c(1,10,19,28,37,46,55), labels=bp$year[c(1,10,19,28,37,46,55)])
lines(pred.instGC, lwd=2, col=2)



####Nuclear modelling

### THIS IS NOT ACCURATE IT HAS A PLATEAU AT THE TOP
bm_GN<-BM(GN,display = T)
summary(bm_GN)

pred_bmGN<- predict(bm_GN, newx=c(1:60))
pred.instGN<- make.instantaneous(pred_bmGN)



plot(GN, type= "b",xlab="Year", ylab="Annual consumption",  pch=16, lty=3, xaxt="n", cex=0.6, ylim=c(0,2))
axis(1, at=c(1,10,19,28,37,46,55), labels=bp$year[c(1,10,19,28,37,46,55)])
lines(pred.instGN, lwd=2, col=2)

### we fit a GGM (better model...but we need to interpret well the parameters)Ç

### why is the market potential dynamic?

GGM_GN<- GGM(GN, prelimestimates=c(56.339566617, 0.001, 0.01, 0.001481412, 0.129385437))
summary(GGM_GN)

pred_GGMGN<- predict(GGM_GN, newx=c(1:60))
pred.instGGMGN<- make.instantaneous(pred_GGMGN)


plot(GN, type= "b",xlab="Year", ylab="Annual consumption",  pch=16, lty=3, xaxt="n", cex=0.6)
axis(1, at=c(1,10,19,28,37,46,55), labels=bp$year[c(1,10,19,28,37,46,55)])
lines(pred.instGGMGN, lwd=2, col=2)




########Renewables modelling

bm_GR<-BM(GR,display = T)
summary(bm_GR)

pred_bmGR<- predict(bm_GR, newx=c(1:60))
pred.instGR<- make.instantaneous(pred_bmGR)


plot(GR, type= "b",xlab="Year", ylab="Annual consumption",  pch=16, lty=3, xaxt="n", cex=0.6)
axis(1, at=c(1,10,19,28,37), labels=bp$year[28:56][c(1,10,19,28,37)])
lines(pred.instGR, lwd=2, col=1)


###we fit a GBM with one exponential shock

GBMe1GR<- GBM(GR,shock = "exp",nshock = 1,prelimestimates = c(3.250461e+01, 5.708759e-04, 1.914512e-01, 15,-0.1,0.1))
summary(GBMe1GR)

pred_GBMe1GR<- predict(GBMe1GR, newx=c(1:60))
pred.instGBMe1GR<- make.instantaneous(pred_GBMe1GR)


plot(GR, type= "b",xlab="Year", ylab="Annual consumption",  pch=16, lty=3, xaxt="n", cex=0.6)
axis(1, at=c(1,10,19,28,37), labels=bp$year[28:56][c(1,10,19,28,37)])
lines(pred.instGBMe1GR, lwd=2, col=1)


### EXPLANATION OF THE PARAMETERS --> 
### Germany wanted to be less depenedent on fossil fuel due to 9/11
# A1: 
# B1:
# C1:

####Comparison between BM and GBMe1

plot(GR, type= "b",xlab="Year", ylab="Annual consumption",  pch=16, lty=3, cex=0.6, xlim=c(1,60), ylim=c(0,3))

lines(pred.instGBMe1GR, lwd=2, col=2)
lines(pred.instGR, lwd=2, col=3)


### We Perform a competition study between nuclear and renewables

year<- bp$year[1:55]

###make a plot of the two series
plot(year,GN,xlab="year", ylab="Consumption (Mtoe)",   type= "b", pch=16, lty=3, cex=0.7)
points(year[28:55],GR, type= "b", pch=16, lty=3, cex=0.7, col=2)

###estimate the UCRCD (with delta and gamma)
ucrcdNR<- UCRCD(GN,GR, display=T)
summary(ucrcdNR)


##we make a plot of the UCRCD model 

plot(year,GN,xlab="year", ylab="Consumption (Mtoe)",   type= "b", pch=16, lty=3, cex=0.7)
points(year[28:55],GR, type= "b", pch=16, lty=3, cex=0.7, col=2)

lines(year[1:55], (ucrcdNR$fitted.i[[1]]), lwd=2, col=1)
lines(year[28:55], (ucrcdNR$fitted.i[[2]]), lwd=2, col=2)

###we also add a line and some text within the plot
abline(v=1991, lty=2)
text(1991, 0.8, pos=4, "renewables enter in 1992")



