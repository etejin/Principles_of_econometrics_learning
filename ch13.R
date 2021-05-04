###### chapter 13 VEC and VAR models
rm(list = ls())
#
pkgs <- c("tseries", "dynlm", "vars", "nlWaldTest", "lmtest",
          "broom", "PoEdata", "car", "sandwich", "knitr", "forecast")
sapply(pkgs, require, character.only = TRUE)
#### VAR and VEC models
data("gdp")
View(gdp)
gdp <- ts(gdp, start = c(1970,1), end = c(2000,4), frequency = 4)
#
ts.plot(gdp[,1], gdp[,2], type = "l",
        lty = c(1, 2), col = c(2, 4))
legend("topleft", border = NULL, legend = c("USA", "AUS"),
       lty = c(1, 2), col = c(2, 4), seg.len = 0.8)
##
ndiffs(gdp[,"usa"])
ndiffs(gdp[,"aus"])
#
adf.test(gdp[,"usa"], k = 2)
adf.test(gdp[,"aus"], k = 2) # both two are nonstationary
#
ndiffs(diff(gdp[,"usa"]))
ndiffs(diff(gdp[,"aus"]))
#
adf.test(diff(gdp[,"usa"]), k = 1)
adf.test(diff(gdp[,"aus"]), k = 1) # both are stationary now
# both series are I(1)
## test for integration
cint1.dyn <- dynlm(aus ~ usa-1, gdp)
kable(tidy(cint1.dyn), 
      caption = "the results of the cointegration equation 'cint1.dyn'")
#
ehat <- resid(cint1.dyn)
cint2.dyn <- dynlm(d(ehat) ~ L(ehat)-1)
summary(cint2.dyn)
##
vecaus <- dynlm(d(aus) ~ L(ehat), gdp)
vecusa <- dynlm(d(usa) ~ L(ehat), gdp)
#
tidy(vecaus) # error correction term is significant, us economy do 
# affect the australian eco
tidy(vecusa) # error correction term is insignificant, aus economy do not
# affect the us eco
# L(ehat) measures the deviation of aus eco from its cointergrating 
# level of 0.985 of the us eco
#### Estimating a var model
data("fred")
View(fred)
##
fred <- ts(fred, start = c(1960,1), end = c(2009, 4),
           frequency = 4)
#
ts.plot(fred[,1], fred[,2], type = "l",
        lty = c(1, 2), col = c(7, 13))
legend("topleft", border = NULL, legend = c("consumption", "income"),
       lty = c(1, 2), col = c(7, 13), seg.len = 0.8)
#
ndiffs(fred[,1]) # 1
ndiffs(fred[,2]) # 2
#
Acf(fred[,1])
Acf(fred[,2])
#
adf.test(fred[,1], k = 1)
adf.test(fred[,2], k = 2) # nonstationary
##
ndiffs(diff(fred[,1])) # 0
ndiffs(diff(fred[,2])) # 1
#
adf.test(diff(fred[,1]), k = 0)
adf.test(diff(fred[,2]), k = 2) # both I(1)
##
cointcy <- dynlm(c ~ y, fred)
ehat <- resid(cointcy) 
#
ndiffs(ehat) # 1
adf.test(ehat) # nonstationary, nocointegrated
##
Dc <- diff(fred[,1])
Dy <- diff(fred[,2])
#
varmat <- as.matrix(cbind(Dc, Dy)) # matrix includes endogenous 
# variables in the VAR model
varfit <- vars::VAR(varmat)
summary(varfit)
#
??VAR
#### Impulse responses and variance decompositions
impresp <- vars::irf(varfit)
plot(impresp)
#
plot(vars::fevd(varfit))
####