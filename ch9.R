###### chapter 9 Time-series: stationary variables
rm(list = ls()) # remove all the items in envir
pkgs <- c("dynlm", "orcutt", "nlWaldTest", "zoo", "pdfetch", "lmtest" ,"broom", 
          "PoEdata", "car", "sandwich", "knitr", "forecast", "WDI")
sapply(pkgs, require, character.only = TRUE)
#### Finite distributed lags
data("okun")
View(okun)
#
check.ts <- is.ts(okun) # false
okun.ts <- ts(okun, start = c(1985,2), end = c(2009,3), frequency = 4)
#
okun.ts.tab <- cbind(okun.ts,
                     lag(okun.ts[,2], -1),
                     diff(okun.ts[,2], lag = 1),
                     lag(okun.ts[,1], -1),
                     lag(okun.ts[,1], -2),
                     lag(okun.ts[,1], -3))
#
kable(head(okun.ts.tab),
      caption = "The 'okun' dataset with differences and lags",
      col.names = c("g", "u", "uL1", "du", "gL1", "gL2", "gL3"))
#

okunL3.dyn <- dynlm::dynlm(d(u)~L(g,0:3), 
                           data = okun.ts) # perform dynamic model
# d(u) is equal to diff(u); L(g,0:3) is equal to lag(g, 0:-3)
#
okunL2.dyn <- dynlm::dynlm(d(u)~L(g,0:2),
                           data = okun.ts)
#
kable(tidy(summary(okunL3.dyn)), digits = 4,
      caption = "The 'okun' distributed lag model with three lags")
kable(tidy(summary(okunL2.dyn)), digits = 4,
      caption = "The 'okun' distributed lag model with two lags")
#
glL3 <- glance(okunL3.dyn)[c("r.squared", "statistic", "AIC", "BIC")]
glL2 <- glance(okunL2.dyn)[c("r.squared", "statistic", "AIC", "BIC")]
tbl <- rbind(glL3, as.numeric(glL2)) # learn it
kable(tbl, caption = "Goodness-of-fit statistics for 'okun' models")
#### Serial correlation
plot(okun.ts[,"g"], ylab = "growth")
plot(okun.ts[,"u"], ylab = "unemployment") # seasonal, cyclical
### plot autocorrelation
##
ggL1 <- data.frame(cbind(okun.ts[,"g"], lag(okun.ts[,"g"], -1)))
names(ggL1) <- c("g", "gL1")
#
plot(ggL1) # plot autocorrelation
meang <- mean(ggL1$g, na.rm = TRUE)
meangL1 <- mean(ggL1$gL1, na.rm = TRUE)
abline(v = meang, lty = 2)
abline(h = meangL1, lty = 2)
#
ggL2 <- data.frame(cbind(okun.ts[,"g"], lag(okun.ts[,"g"], -2)))
names(ggL2) <- c("g", "gL2")
#
plot(ggL2)
meangL2 <- mean(ggL2$gL2, na.rm = TRUE)
abline(v = meang, lty = 2)
abline(h = meangL2, lty = 2)
#
### autocorrelation test
growth_rate <- okun.ts[,"g"]
acf(growth_rate) # correlogram
## acf test on the error term
data("phillips_aus")
View(phillips_aus)
#
phill.ts <- ts(phillips_aus, 
               start = c(1987,1), end = c(2009,3),
               frequency = 4)
inflation <- phill.ts[,1]
Du <- diff(phill.ts[,2]) # by default, lag = 1
plot(inflation)
plot(Du)
#
phill.dyn <- dynlm::dynlm(inf ~ diff(u), phill.ts)
ehat <- resid(phill.dyn)
kable(tidy(phill.dyn), 
      caption = "Summary of the 'phillps' model")
#
plot(ehat) # has some certain pattern
abline(h = 0, lty = 2)
#
corrgm <- acf(ehat)
# the significant relationship between inflation and unempolyment
# rate is not so reliable
### lagrange multiplier test
a <- lmtest::bgtest(phill.dyn, order = 1, type = "F", fill = 0)
b <- lmtest::bgtest(phill.dyn, order = 1, type = "F", fill = NA)
#
c <- lmtest::bgtest(phill.dyn, order = 4, type = "Chisq", fill = 0)
d <- lmtest::bgtest(phill.dyn, order = 4, type = "Chisq", fill = NA)
#
dfr <- data.frame(rbind(a[c(1, 2, 4)],
                        b[c(1, 2, 4)],
                        c[c(1, 2, 4)],
                        d[c(1, 2, 4)]))
dfr <- cbind(c("1, F, 0", "1, F, NA", 
               "4, Chisq, 0", "4, Chisq, NA"), dfr)
#
names(dfr) <- c("Method", "Statistic", "Parameters", "p-Value")
#
kable(dfr, caption = "Breusch-Godfrey test for the phillps example")
### durbin-watson test
dwtest(phill.dyn)
#### Estimation with serially correlated errors
s0 <- coeftest(phill.dyn)
s1 <- coeftest(phill.dyn,
               vcov. = sandwich::vcovHAC(phill.dyn))
s2 <- coeftest(phill.dyn, 
               vcov. = sandwich::NeweyWest(phill.dyn))
s3 <- coeftest(phill.dyn,
               vcov. = sandwich::kernHAC(phill.dyn))
#
tbl <- data.frame(cbind(s0[c(3, 4)],
                        s1[c(3, 4)],
                        s2[c(3, 4)],
                        s3[c(3, 4)]))
#
names(tbl) <- c("Incorrect", "vcovHAC", "NeweyWest", "kernHAC")
row.names(tbl) <- c("Intercept", "Du")
#
kable(tbl, digits = 3,
      caption = "Comparing standard errors for the phillps model")
# compare three versions of the HAC ste
##
ac <- acf(ehat, plot = FALSE)
ac$acf[2:6] # first five lags in the residuals of phillips model
#### Nonlinear least square estimation
phill.dyn <- dynlm::dynlm(inf ~ diff(u), phill.ts)
# non-linear AR(1) model with 'Cochrane-Orcutt' method 'nls'
phill.ts.tab <- cbind(phill.ts[,1],
                      phill.ts[,2],
                      lag(phill.ts[,1], -1),
                      diff(phill.ts[,2], lag = 1),
                      lag(diff(phill.ts[,2], lag = 1), -1))
phill.dfr <- data.frame(phill.ts.tab)
names(phill.dfr) <- c("inf", "u", "Linf", "Du", "LDu")
#
phill.nls <- nls(inf ~ b1*(1-rho)+b2*Du+rho*Linf-rho*b2*LDu,
                 data = phill.dfr,
                 start = list(rho = 0.5, b1 = 0.5, b2 = -0.5))
#
s1 # 'phill.dyn' with HAC errors
s0
coeftest(phill.nls) # new model better than old model with HAC errors
#
coef(phill.nls)[["rho"]] # estimate of auto-correlation coefs
#### A more general model
s.nsl <- summary(phill.nls)
#
phill.gen <- dynlm::dynlm(inf ~ L(inf)+d(u)+L(d(u)), data = phill.ts)
s.gen <- summary(phill.gen)
#
nlWaldTest::nlWaldtest(phill.gen, texts = "b[4]=-b[2]*b[3]")
# test non-linear restrictions
##
phill1.gen <- dynlm::dynlm(inf ~ 
                             lag(inf, -1)+diff(u)+lag(diff(u),-1), 
                           data = phill.ts)
#
kable(tidy(phill.gen), 
      caption = "Using dynlm with L and d operators")
kable(tidy(phill1.gen),
      caption = "Using dynlm with lag and diff operators")
# they are the same.
#### Autoregressive models
okun.ar2 <- dynlm::dynlm(g ~ L(g)+L(g,2), okun.ts)
kable(tidy(okun.ar2), digits = 4,
      caption = "Autoregressive model of order 2 using the dataset $okun$")
## acf test
res.ar2 <- resid(okun.ar2)
forecast::Acf(res.ar2, lag.max = 12,
              main = "Residual autocorrelation in okun.ar2 model")
# model comparison
aics <- rep(0, 5)
bics <- rep(0, 5)
y <- okun.ts[,"g"]
for (i in 1:5){
  ari <- dynlm::dynlm(y~L(y,1:i), start = i)
  aics[i] <- AIC(ari)
  bics[i] <- BIC(ari)
}
#
tbl <- data.frame(rbind(aics, bics))
names(tbl) <- c("1", "2", "3", "4", "5")
row.names(tbl) <- c("AIC", "BIC")
#
kable(tbl, digits = 1, align = "c",
      caption = "Lag order selection for an AR model")
#### Forecasting
y <- okun.ts[,"g"]
g.ar2 <- dynlm::dynlm(y~L(y,1:2))
kable(tidy(g.ar2), caption = "The AR(2) growth model")
# ar model
ar2g <- ar(y, aic = FALSE, order.max = 2, method = "ols") # fits an autoregressive model 
# to a univariate ts
fcst <- data.frame(forecast(ar2g, 3))
kable(fcst, digits = 3,
      caption = "Forecasts for the AR(2) growth model")
#
plot(forecast(ar2g, 3))
## exponential forecast model
okun.ets <- forecast::ets(y)
okunf.est <- forecast::forecast(okun.ets, 1) # one period of forecast
#
plot(okunf.est)
#
okun.HW <- HoltWinters(y, beta = FALSE, gamma = FALSE)
plot(okun.HW) # plot both observed and predicted level of y
#
okunf.HW <- forecast::forecast(okun.HW,1)
#### 





















