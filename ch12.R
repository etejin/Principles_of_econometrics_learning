###### chapter 12 Time series: nonstationary
rm(list = ls())
#
pkgs <- c("tseries", "dynlm", "nlWaldTest", "lmtest", "broom", "PoEdata", "car", 
          "sandwich", "knitr", "forecast")
sapply(pkgs, require, character.only = TRUE)
####
data("usa")
View(usa)
#
usa.ts <- ts(usa, start = c(1984,1), end = c(2009, 4),
             frequency = 4)
Dgdp <- diff(usa.ts[,1])
Dinf <- diff(usa.ts[,2])
Df <- diff(usa.ts[,3])
Db <- diff(usa.ts[,4])
#
usa.ts.diff <- ts.union(gdp = usa.ts[,1],
                        inf = usa.ts[,2],
                        f = usa.ts[,3],
                        b = usa.ts[,4],
                        Dgdp, Dinf, Df, Db,
                        dframe = TRUE)
#
opar <- par(no.readonly = TRUE)
par(mfrow = c(2, 2))
#
plot(usa.ts.diff$gdp)
plot(usa.ts.diff$inf)
plot(usa.ts.diff$f)
plot(usa.ts.diff$b)
#
plot(usa.ts.diff$Dgdp)
plot(usa.ts.diff$Dinf)
plot(usa.ts.diff$Df)
plot(usa.ts.diff$Db) # remove the trend
#
par(opar)
#
kable(head(usa.ts.diff),
      caption = "Time series data frame constructed with 'ts.union'")
#### AR(1), the first-order autoregressive model
N <- 500
a <- 1 # constant
l <- 0.01
rho <- 0.7
#
set.seed(246810)
v <- ts(rnorm(N, 0, 1)) # create error term
##
y <- ts(rep(0, N))
for (t in 2:N){
  y[t] <- rho*y[t-1]+v[t]
} # create startionary AR(1)
# 
plot(y, type = 'l', ylab = 'rho*y[t-1]+v[t]')
abline(h = 0)
##
y <- ts(rep(0, N))
for (t in 2:N){
  y[t] <- y[t-1] + v[t]
} # create AR(1) with random walk
# 
plot(y, type = "l", ylab = 'y[t-1] + v[t]')
abline(h = 0, lwd = 2, col = 7)
##
y <- ts(rep(0,N))
for (t in 2:N){
  y[t] <- a + rho*y[t-1] + v[t]
} # create ts with constant
#
plot(y, type = "l", ylab = 'a + rho*y[t-1]+v[t]')
abline(h = 0, lwd = 2, col = 3)
##
y <- ts(rep(0, N))
for (t in 2:N){
  y[t] <- a + l*time(y)[t] + rho*y[t-1] + v[t]
} # create ts with constant and trend
# 
plot(y, type = "l", ylab = 'a + l*time(y) + rho*y[t-1] + v[t]')
abline(h = 0, lwd = 2, col = 7)
##
y <- ts(rep(0, N))
for (t in 2:N){
  y[t] <- l*time(y)[t] + rho*y[t-1] + v[t]
} # create ts with trend
# 
plot(y, type = "l", ylab = 'l*time(y) + rho*y[t-1] + v[t]')
abline(h = 0, lwd = 2, col = 7)
##
a <- 0.1
#
y <- ts(rep(0, N))
for (t in 2:N){
  y[t] <- a + y[t-1] + v[t]
} # random walk ts with constant
# 
plot(y, type = "l", ylab = 'a + y[t-1] + v[t]')
abline(h = 0, lwd = 2, col = 7)
##
y <- ts(rep(0, N))
for (t in 2:N){
  y[t] <- a + l*time(y)[t] + y[t-1] + v[t]
} # create AR(1) with random walk
# 
plot(y, type = "l", ylab = 'a + l*time(y) + y[t-1] + v[t]')
abline(h = 0, lwd = 2, col = 7)
#### Spurious regression
T <- 1000
set.seed(1357)
y <- ts(rep(0, T))
vy <- ts(rnorm(T)) # error term, innovation or shock
#
for (t in 2:T){
  y[t] <- y[t-1] + vy[t]
}
#
set.seed(4365)
x <- ts(rep(0, T))
vx <- ts(rnorm(T))
#
for (t in 2:T){
  x[t] <- x[t-1] + vx[t]
}
#
y <- ts(y[300:1000])
x <- ts(x[300:1000])
#
ts.plot(y, x, ylab = "regress y on x") # can have different time basis, 
# but must have the same frequency
#
spurious.ols <- lm(y ~ x)
summary(spurious.ols)
#
plot(x, y, type = "p", col = "grey")
#### Unit-root test for stationary
plot(usa.ts.diff$f) # indicate both intercept and slope
Acf(usa.ts.diff$f) # correlagram test, 10 lags
#
tseries::adf.test(usa.ts.diff$f, k = 10) # specify the lag, cannot reject
tseries::adf.test(usa.ts.diff$f) # r will automatically calculate the lag, the lag 
# it calculates is 4, results are different; reject successfully, false
##
plot(usa.ts.diff$b)
Acf(usa.ts.diff$b)
#
tseries::adf.test(usa.ts.diff$b, k = 10) # fail to reject
##
f <- usa.ts.diff$f
f.dyn <- dynlm::dynlm(d(f) ~ L(f)+L(d(f)))
#
tidy(f.dyn)
##
b <- usa.ts.diff$b
b.dyn <- dynlm::dynlm(d(b) ~ L(b)+L(d(b)))
tidy(b.dyn)
##
f_diff <- diff(f)
plot(f_diff)
Acf(f_diff) # lag is 2
#
adf.test(f_diff, k = 2)
##
b_diff <- diff(b)
plot(b_diff)
#
adf.test(b_diff, k = 2)
##
df.dym <- dynlm::dynlm(d(f_diff) ~ L(f_diff)-1) # (-1) tells R we do not want 
# intercept
db.dym <- dynlm::dynlm(d(b_diff) ~ L(b_diff)-1)
#
tidy(df.dym)
tidy(db.dym)
##
forecast::ndiffs(f) # determine the order of integration of a series
forecast::ndiffs(b, alpha = 0.05, test = "adf")
#### Cointegration
fb.dyn <- dynlm::dynlm(b ~ f)
ehat.fb <- resid(fb.dyn)
#
forecast::ndiffs(ehat.fb) # 1
##
output <- dynlm::dynlm(d(ehat.fb) ~ L(ehat.fb)+L(d(ehat.fb))-1)
foo <- tidy(output)
foo
#
adf.test(ehat.fb,k=1)
##
bfx <- as.matrix(cbind(b,f), demean = FALSE)
tseries::po.test(bfx) # marginally reject the null of no cointegration 
# at 5 percent level
#### The error correction model (ECM)
b.ols <- dynlm::dynlm(L(b) ~ L(f))
#
b1ini <- coef(b.ols)[[1]]
b2ini <- coef(b.ols)[[2]]
#
d.ols <- dynlm::dynlm(b ~ L(b)+f+L(f))
aini <- 1-coef(d.ols)[[2]]
d0ini <- coef(d.ols)[[3]]
d1ini <- coef(d.ols)[[4]]
##
Db <- diff(b)
Df <- diff(f)
Lb <- lag(b, -1)
Lf <- lag(f, -1)
LDf <- lag(diff(f), -1)
#
bfset <- data.frame(ts.union(cbind(Db, Df, Lb, Lf, LDf)))
formula <- Db ~ -a*(Lb-b1-b2*Lf)+d0*Df+d1*LDf
bf.nls <- nls(formula, na.action = na.omit,
              data = bfset,
              start = list(a = aini, b1 = b1ini, b2 = b2ini,
                           d0 = d0ini, d1 = d1ini))
#
kable(tidy(bf.nls),
      caption = "Parameyer estimates in the ECM")
##
ehat <- bfset$Lb-coef(bf.nls)[[2]]-coef(bf.nls)[[3]]*bfset$Lf
ehat <- ts(ehat)
#
ehat.adf <- dynlm(d(ehat) ~ L(ehat)+d(L(ehat))-1)
kable(tidy(ehat.adf),
      caption = "Stationary test within the ECM")
####

















