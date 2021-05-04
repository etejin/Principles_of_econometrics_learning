###### chapter 14 Time-varing volatility and ARCH models
pkgs <- c("FinTS", "rugarch", "tseries", "dynlm", "vars", "nlWaldTest",
          "lmtest", "broom", "PoEdata", "car", "sandwich", "knitr",
          "forecast")
sapply(pkgs, require, character.only = TRUE)
#### The ARCH model
data("byd")
View(byd)
rTS <- ts(byd$r)
#
plot.ts(rTS)
hist(rTS, main = "", breaks = 20, freq = FALSE, col = 3)
#
byd.mean <- dynlm(rTS ~ 1) # regress response on a constant
summary(byd.mean)
#
ehatsq <- ts(resid(byd.mean)^2)
#
byd.ARCH <- dynlm(ehatsq ~ L(ehatsq))
summary(byd.ARCH)
#
T <- nobs(byd.mean)
q <- length(coef(byd.ARCH))-1
Rsq <- glance(byd.ARCH)[[1]]
LM <- (T-q)*Rsq # LM statistic
#
alpha <- 0.05
Chicr <- qchisq(1-alpha, q)
# 
LM; Chicr
# reject H0, the series has ARCH effects
##
bydArchTest <- FinTS::ArchTest(byd, lags = 1, demean = TRUE)
bydArchTest
## ARCH model
byd.arch <- tseries::garch(rTS, order = c(0, 1))
#
sbydarch <- summary(byd.arch)
sbydarch
#
hhat <- ts(2*byd.arch$fitted.values[-1,1]^2)
plot.ts(hhat) # plot variance
#### The GARCH model
## standard GARCH model
garchSpec <- ugarchspec(
  variance.model = list(model = 'sGARCH',
                        garchOrder = c(1,1)),
  mean.model = list(armaOrder = c(0,0)),
  distribution.model = "std")
#
garchFit <- ugarchfit(spec = garchSpec, data = rTS)
coef(garchFit)
#
rhat <- garchFit@fit$fitted.values
plot.ts(rhat)
#
hhat <- ts(garchFit@fit$sigma^2)
plot.ts(hhat)
## tGARCH model
garchMod <- ugarchspec(variance.model = list(model = "fGARCH",
                                             garchOrder = c(1, 1),
                                             submodel = "TGARCH"),
                       mean.model = list(armaOrder = c(0,0)),
                       distribution.model = 'std')
#
garchFit <- ugarchfit(spec = garchMod, data = rTS)
coef(garchFit)
#alu
rhat <- garchFit@fit$fitted.values
plot.ts(raht)
#
hhat <- ts(garchFit@fit$sigma^2)
plot.ts(hhat)
## GARCH-in-mean
garchMod <- ugarchspec(
  variance.model = list(model = "fGARCH",
                        garchOrder = c(1, 1),
                        submodel = "APARCH"),
  mean.model = list(armaOrder = c(0,0),
                    include.mean = TRUE,
                    archm = TRUE,
                    archpow = 2),
  distribution.model = "std"
)
#
garFit <- ugarchfit(spec = garchMod, data = rTS)
coef(garFit)
#
rhat <- garFit@fit$fitted.values
plot.ts(rhat)
#
hhat <- ts(garFit@fit$sigma^2)
plot.ts(hhat)
####
















