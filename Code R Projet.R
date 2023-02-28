rm(list=ls())
graphics.off()

getwd()

library(xts)
library(readxl)
library(astsa)
library(urca)
library(ggfortify)
library(forecast)
library(fpp2)
library(gets)
library(xlsx)

#FRANCE
france <- read.xlsx("Base de données Projet.xlsx", 1)
france$TIME <- as.Date(as.yearqtr(france$TIME,format = "%Y-Q%q"))
fra <- xts(x = france[,2:ncol(france)], order.by = france$TIME)
plot(france)
fra1 <- fra["1975-01-01/1986-10-01"]
fra2 <- fra["2000-01-01/2016-10-01"]

#BRENT
brent <- read.xlsx("Base de données Projet.xlsx", 2)
brent$TIME <- as.Date(as.yearqtr(brent$TIME,format = "%Y-Q%q"))
prixbrent <- xts(x = brent[,2:ncol(brent)], order.by = brent$TIME)
plot(prixbrent)
brent1<- prixbrent["1975-01-01/1986-10-01"]
brent2 <- prixbrent["2000-01-01/2016-10-01"]
prixbrent<-prixbrent["1975-01-01/2016-10-01"]
fra<-fra["1975-01-01/2016-10-01"]
plot(fra1)
summary(brent1)
summary(brent2)


plot(fra1)
fra1 <- log(fra1)
plot(fra1)
fra2 <- log(fra2)
lfrance <-log(fra)
lbrent <-log(prixbrent)
brent1 <- log(brent1)
brent2 <- log(brent2)

#---------------------------------------------------------------------------------------
#!                                      TEST ADF                                        !
#---------------------------------------------------------------------------------------
summary(ur.df(fra1, type = "trend", lags = 4, selectlags="AIC")) 
# test trend (tt) : t-stat = 3.262 < t-df = 3.74 => on accepte h0 => tendance non significative
summary(ur.df(fra1, type = "drift", lags = 4, selectlags="AIC")) 
# test constante (intercept): t-stat = 2.893 < t-df = 3.19 => on accepte h0 => constante
summary(ur.df(fra1, type = "none", lags = 4, selectlags="AIC")) 
# test RU: t-stat = 1.478 > t-df = -2.58 => on accepte h0 => RU


summary(ur.pp(fra1, model = "trend", type="Z-tau", lags = "short")) 
# test trend (tt):t-stats  =  0.854  < t-df = 2.79 => on accepte H0
summary(ur.pp(fra1, model = "constant", type="Z-tau", lags = "short")) 
# test RU: Z-tau (PP) = -1.7329  >  t-df = -3.57438  => on accepte H0 => RU

summary(ur.kpss(fra1, type="tau", lags = "short")) 
# Test RU: t-kpss = 0.2684 > t-tabulé(5%) = 0.146 => on accepte H0 => RU



#DIFF


summary(ur.df(dfra1, type = "trend", lags = 4, selectlags="AIC")) 
# test trend (tt) : t-stat = -1.976  < t-df = 3.74 => on accepte h0 => tendance non significative
summary(ur.df(dfra1, type = "drift", lags = 4, selectlags="AIC")) 
# test constante (intercept): t-stat = 1.543 < t-df = 3.19 => on accepte h0 => consante
summary(ur.df(dfra1, type = "none", lags = 4, selectlags="AIC")) 
# test RU: t-stat = -1.4468  > t-df = -2.58 => on accepte h0 =>pas de RU
# Tendance DS (à 1%)


summary(ur.pp(dfra1, model = "trend", type="Z-tau", lags = "short")) 
# test trend (tt):t-stats  =  -3.036  < t-df = 2.79 => on accepte H0
summary(ur.pp(dfra1, model = "constant", type="Z-tau", lags = "short")) 
# test RU: Z-tau (PP) = -7.5571  <  t-df = -3.57438  => on rejette H0 => pas de RU
summary(ur.kpss(dfra1, type="tau", lags = "short")) 
# Test RU: t-kpss = 0.0782  < t-tabulé(5%) = 0.146 => on rejette H0 (règle de décision normale) => pas de RU



#FRANCE2


summary(ur.df(fra2, type = "trend", lags = 4, selectlags="AIC")) 
# test trend (tt) : t-stat = 3.262 < t-df = 3.74 => on accepte h0 => tendance non significative
summary(ur.df(fra2, type = "drift", lags = 4, selectlags="AIC")) 
# test constante (intercept): t-stat = 1.015 < t-df = 3.19 => on accepte h0 => constante
summary(ur.df(fra2, type = "none", lags = 4, selectlags="AIC"))
# test RU: t-stat = 2.4761 > t-df = -2.58 => on accepte h0 => RU


summary(ur.pp(fra2, model = "trend", type="Z-tau", lags = "short")) 
# test trend (tt):t-stats  =  1.255  < t-df = 2.79 => on accepte H0
summary(ur.pp(fra2, model = "constant", type="Z-tau", lags = "short")) 
# test RU: Z-tau (PP) = 2.143 >  t-df = -3.57438  => on accepte H0 => RU

summary(ur.kpss(fra2, type="tau", lags = "short")) 
# Test RU: t-kpss = 0.2751  > t-tabulé(5%) = 0.146 => on rejette H0 (règle de décision normale) => RU

#STATIONNARISATION
plot(fra2)
dfra1 <- diff(fra1)
dfra2 <- diff(fra2)
plot(dfra1)
plot(dfra2)
dfra1<- na.trim(dfra1)
dfra2<- na.trim(dfra2)
plot(brent2)
dbrent1 <- diff(brent1)
dbrent2 <- diff(brent2)
plot(dbrent2)
dbrent1<- na.trim(dbrent1)
dbrent2<- na.trim(dbrent2)

#DIFF
summary(ur.pp(dfra1, model = "trend", type="Z-tau", lags = "short")) 
# test trend (tt):t-stats  =- -1.901  < t-df = 2.79 => on accepte H0
# => Tendance non-significative => on passe au modele 2
summary(ur.pp(dfra1, model = "constant", type="Z-tau", lags = "short")) 
# test RU: Z-tau (PP) = -4.1539 <  t-df = -3.635244  => on refuse H0 => pas de RU-> Stationnaire


summary(ur.df(dfra2, type = "trend", lags = 4, selectlags="AIC")) 
# test trend (tt) : t-stat = -0.546  < t-df = 3.74 => on accepte h0 => tendance non significative
summary(ur.df(dfra2, type = "drift", lags = 4, selectlags="AIC")) 
# test constante (intercept): t-stat = 2.115 < t-df = 3.19 => on accepte h0 => constante
summary(ur.df(dfra2, type = "none", lags = 4, selectlags="AIC")) 
# test RU: t-stat =  -2.7789  < t-df = -2.58 => on accepte h0 =>pas de RU



summary(ur.pp(dfra2, model = "trend", type="Z-tau", lags = "short")) 
# test trend (tt):t-stats  =  -0.646  < t-df = 2.79 => on accepte H0
summary(ur.pp(dfra2, model = "constant", type="Z-tau", lags = "short")) 
# test RU: Z-tau (PP) = -5.0328  <  t-df = -3.57438  => on rejette H0 => pas de RU

summary(ur.kpss(dfra2, type="tau", lags = "short")) 
# Test RU: t-kpss = 0.0531  < t-tabulé(5%) = 0.146 => on rejette H0 (règle de décision normale) => pas de RU






dlfrance<- diff(lfrance)
dlbrent<- diff(lbrent)
dlfrance<- na.trim(dlfrance)
dlbrent<- na.trim(dlbrent)
francebrent<- merge(dlfrance,dlbrent)

#COINTEGRATION

autoplot(francebrent, facets = FALSE)
plot(prixbrent)
plot(x=coredata(francebrent[,1]), y=coredata(francebrent[,2]))
abline(reg=lm(coredata(francebrent[,2]) ~ coredata(francebrent[,1])), col = "red", lwd=2)

myols <- lm(coredata(dlfrance) ~ coredata(dlbrent))
summary(myols)
resid_ols <- xts(residuals(myols),index(dlfrance))
plot(resid_ols)
summary(ur.df(resid_ols, type = "none", lags = 4, selectlags="AIC")) 
# Test de cointégration: t-stat = -4.9584  < t(5%) = -1.95 => on rejette H0 => pas de RU
#                                => resid_ols (z_t) est stationnaire (ou I(0)) => cointégration



myols <- lm(coredata(dfra1) ~ coredata(dbrent1))
summary(myols)
resid_ols <- xts(residuals(myols),index(dfra1))
plot(resid_ols)
summary(ur.df(resid_ols, type = "none", lags = 4, selectlags="AIC")) 
# Test de cointégration: t-stat = -2.1655  < t(5%) = -1.95 => on rejette H0 => pas de RU
#                                => resid_ols (z_t) est stationnaire (ou I(0)) => cointégration




myols <- lm(coredata(dfra2) ~ coredata(dbrent2))
summary(myols)
resid_ols <- xts(residuals(myols),index(dfra2))
plot(resid_ols)
summary(ur.df(resid_ols, type = "none", lags = 4, selectlags="AIC")) 
# Test de cointégration: t-stat = -3.6458   < t(5%) = -1.95 => on rejette H0 => pas de RU
#                                => resid_ols (z_t) est stationnaire (ou I(0)) => cointégration



summary(ur.pp(dfra1, model = "trend", type="Z-tau", lags = "short")) 
# test trend (tt):t-stats  =- -1.901  < t-df = 2.79 => on accepte H0
# => Tendance non-significative => on passe au modele 2

summary(ur.pp(dfra1, model = "constant", type="Z-tau", lags = "short")) 
# test RU: Z-tau (PP) = -4.1539 <  t-df = -3.635244  => on refuse H0 => pas de RU-> Stationnaire





#---------------------------------------------------------------------------------------
#!                           Fonction de transfert, ARDL avec GeTS                         !
#---------------------------------------------------------------------------------------
#FRANCE PERIODE 1
lagdbrent1 <- lag(dbrent1, k = 0:4)
datafra1 <- na.trim(merge(fra1, lagdbrent1))
mgets <- arx(datafra1[,1], mc=TRUE, ar=1:4, mxreg=datafra1[,2:6], normality.JarqueB = TRUE)
mgets

modelfra1_gets <- getsm(mgets)
modelfra1_gets

fmodelfra1_gets <- Arima(datafra1[,1], order = c(3, 0, 0), xreg=datafra1[,5], include.constant = TRUE)
fmodelfra1_gets
checkresiduals(fmodelfra1_gets)

#FRANCE PERIODE 2
lagdbrent2 <- lag(dbrent2, k = 0:4)
datafra2 <- na.trim(merge(fra2, lagdbrent2))
mgets <- arx(datafra2[,1], mc=TRUE, ar=1:4, mxreg=datafra2[,2:6], normality.JarqueB = TRUE)
mgets

modelfra2_gets <- getsm(mgets)
modelfra2_gets

fmodelfra2_gets <- Arima(datafra2[,1], order = c(2, 0, 0), xreg=datafra2[,2], include.constant = TRUE)
fmodelfra2_gets
checkresiduals(fmodelfra2_gets)


#ITALIE
italie <- read.xlsx("Base de données Projet.xlsx", 3)
View(italie)
italie$TIME <- as.Date(as.yearqtr(italie$TIME,format = "%Y-Q%q"))
ita <- xts(x = italie[,2:ncol(italie)], order.by = italie$TIME)
plot(ita)
ita2 <- ita["2000-01-01/2016-10-01"]
ita2 <- log(ita2)
dita2 <- diff(ita2)
dita2<- na.trim(dita2)
dataita2 <- na.trim(merge(ita2, lagdbrent2))


mgets <- arx(dataita2[,1], mc=TRUE, ar=1:4, mxreg=dataita2[,2:5], normality.JarqueB = TRUE)
mgets

modelita2_gets <- getsm(mgets)
modelita2_gets
fmodelita2_gets <- Arima(dataita2[,2], order = c(1, 0, 0), xreg=dataita2[,3], include.constant = TRUE)
fmodelita2_gets
checkresiduals(fmodelita2_gets)



#FA et FAP
acf2(dbrent1)
acf2(dbrent2)
acf2(dfra1)
acf2(dfra2)
ggtsdisplay(dbrent1)
ggtsdisplay(dbrent2)
ggtsdisplay(dfra1)
ggtsdisplay(dfra2)



