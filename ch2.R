###### Chapter 2 The Simple Linear Regression Model
#### General model
require(PoEdata)
data("cps_small")
# linear regression model and linear regression equation
# in the linear regression equation:
# x is not random
# all x have the same value of the variance of error
# no correlation between the error terms of two obs
# the expected value (mean) of the error term is 0
with(cps_small,
     plot(wage ~ educ, xlab = "education", ylab = "wage",
          family = "mono"))
# x is not random, so several observations are sharing the same
# value of x
#### Example
data(food)
head(food, 3)
with(food,
     plot(food_exp ~ income,
          ylim = c(0, max(food_exp)),
          xlim = c(0, max(income)),
          xlab = "weekly income in $100",
          ylab = "weekly food expenditure in $",
          type = "p",
          family = "mono"))
# visualize the relationship between two vairables using scatter plot
####
mod1 <- lm(food_exp ~ income, food)
b0 <- coef(mod1)[[1]] # intercept parameter is not so important in 
# econometric models
b1 <- coef(mod1)[[2]] # slope parameter is more instereted
# the value of 10.2096 means that the food expenditure will increase
# 10.2096 when income increases by 1 unit
smod1 <- summary(mod1)
abline(b0, b1)
#
names(mod1)
names(smod1)
# 
mod1$coefficients
smod1$coefficients
#
mod1$model # that may explain in the predicted model, x are not randomly
# selected
#
smod1$aliased # finding linearly dependent terms (aliases) in a linear
# model specified by a formula
# most useful for experimental designs
# complete aliasing refers to effects in linear models taht cannot
# estimated independently of the terms which occur earlier in the model
# and so have their coefs omitted from the fit
# partial aliasing refers to effects that can  be estimated less precisely
# beacuse of correlations induced by the design
#### Prediction
newx <- data.frame(income = c(20, 25, 27)) 
yhat <- predict(mod1, newx)
names(yhat) <- paste("$", newx$income * 100)
yhat
#### Repeated samples to assess regression coefs
# regression coefs are random variables, for we use sample to generate
# it
N <- nrow(food)
C <- 50 # desired number of subsamples
S <- 30 # desired sample size
#
sumb2 <- 0
for (i in 1:C) {
  set.seed(3*i)
  subsample <- food[sample(1:N, size = S, replace = TRUE),]
  mod2 <- lm(food_exp ~ income, data = subsample)
  # sum b2 for subsamples:
  sumb2 <- sumb2 + coef(mod2)[[2]]
}
print(sumb2 / C, digits = 3)
#### Estimated variances and covariance of regression coefficients
vcov(mod1) # return the variance-covariance or covariance matrix of
# the main parameter of a fitted  model object
# the diagonal elements of the matrix contain the variances of the
# variables and the off-diagonal elements contain the covariances
# between all possible pairs of variables
varb0 <- vcov(mod1)[1,1] # intercept variance
varb1 <- vcov(mod1)[2,2]
covb0b1 <- vcov(mod1)[1,2]
#### Non-linear relationships
# the most non-linear relationships invovles logarithms of the dependent
# or independent variables and polinomial functions
data("br")
View(br)
mod2 <- lm(price ~ I(sqft^2), br)
b0 <- coef(mod2)[[1]]
b1 <- coef(mod2)[[2]]
#
sqftx = c(2000, 4000, 6000)
pricex = b0 + b1 * sqftx^2
#
DpriceDsqft <- 2*b1*sqftx # marginal effect of sqft on price
elasticity = DpriceDsqft * sqftx / pricex
#
b0; b1; DpriceDsqft; elasticity
##
plot(br$sqft, br$price, xlab = "Total square feet",
     ylab = "Sale price, $", col = "grey")
curve(b0+b1*x^2, col = "red", add = TRUE)
## alternative way
ordat <- br[order(br$sqft),]
mod3 <- lm(price ~ I(sqft^2), ordat)
plot(br$sqft, br$price, xlab = "Total square feet",
     ylab = "Sale price, $", col = "grey")
lines(fitted(mod3) ~ ordat$sqft, col = "red") # we need first to ordering
# the dataset in the increasing values of sqft before the regression model
# is evaluated
### log-linear model, regresses the log of the dependent variable on a
# linear expression of the independent variable
# make the distribution closer to the normal distribution
hist(br$price)
hist(log(br$price))
mod4 <- lm(log(price) ~ sqft, br)
#
b0 <- coef(mod4)[[1]]
b1 <- coef(mod4)[[2]] # showing that an increase in the surface area of
# the apartment by one unit, increases the price of the apartment by 
# 0.041 percent
# the marginal effect of an increase in x on y:
# dy / dx = b1 * y
# the elasticity :
# (dy / dx) * (x / y) = b1 * x
# the fitted  value :
# hat(y) = exp(fitted(mod4))
##
ordat <- br[order(br$sqft),]
mod4 <- lm(log(price) ~ sqft, ordat)
plot(br$price ~ br$sqft, col = "grey")  
lines(exp(fitted(mod4))  ~ ordat$sqft,
      col = "blue", main = "Log-linear Mode")
##
pricex <- median(br$price)
sqftx <- (log(pricex) - coef(mod4)[[1]]) / coef(mod4)[[2]]
DyDx <- coef(mod4)[[2]] * pricex
elasticity <- coef(mod4)[[2]] * sqftx
DyDx; elasticity
##
b0 <- coef(mod4)[[1]]
b1 <- coef(mod4)[[2]]  
sqftx <- c(2000, 3000, 4000)  
pricex <- c(100000, exp(b0 + b1*sqftx))
sqftx <- (log(pricex) - b0) / b1
#
DpricexDsqftx <- b1 * pricex
elasticity <- b1 * sqftx
#### Using indicator variables in a regression
data(utown)
View(utown)
sapply(utown, class)
#
aggregate(utown$price, by = list(Location = utown$utown), mean)
by(utown$price, utown$utown, mean)
tapply(utown$price, utown$utown, mean)
doBy::summaryBy(price ~ utown, 
                data = utown,
                FUN = mean)
doBy::summary_by(utown, price ~ utown, FUN = mean)
xtabs(utown$price ~ utown$utown)
psych::describeBy(utown$price, utown$utown)
#
pricebar <- .Last.value
price0bar <- pricebar[[1]]
price1bar <- pricebar[[2]]
#
mod5 <- lm(price ~ utown, utown)
b0 <- coef(mod5)[[1]] # the intercept actually is the average price of 
# non-university houses, because it is 0
b1 <- coef(mod5)[[2]]
#
price0bar; price1bar; b0; b1; (b0 + b1)
#### Monte Carlo Simulation
# a monte carlo simulation generates random variables for the dependent 
# variable when the regression coefficients and the distribution of the 
# random term are given
N <- 40
x1 <- 10
x2 <- 20
b0 <- 100
b1 <- 10
mu <- 0
sig2e <- 2500
sde <- sqrt(sig2e)
yhat1 <- b0+b1*x1
yhat2 <- b0+b1*x2
curve(dnorm(x, mean = yhat1, sd = sde), 0, 500, col = "blue")
curve(dnorm(x, mean = yhat2, sd = sde), 0, 500, add = TRUE, col = "red")
abline(v = yhat1, col = "blue", lty = 2)
abline(v = yhat2, col = "red", lty = 2)
legend("topright", legend = c("f(y|x=10)", "f(y|x=20"), 
       lty = 1,
       xjust = 0.3, yjust = 0.9,
       x.intersp = 0.2, y.intersp = 0.5,
       seg.len = 0.8, xpd = TRUE,
       col = c("blue", "red"))
# theoretical probability distributions of food expenditure, given two
# levels of income
x <- c(rep(x1, N/2), rep(x2, N/2))
xbar <- mean(x)
sumx2 <- sum((x-xbar)^2)
varb1 <- sig2e / sumx2
sdb1 <- sqrt(varb1)
leftlim <- b1-3*sdb1
rightlim <- b1+3*sdb1
curve(dnorm(x, mean = b1, sd = sdb1), leftlim, rightlim)
abline(v = b1, lty = 2)
# calculate the variance of b1 and plot the corresponding 
# theoretical probability density function of b1
##
set.seed(12345)
y <- b0+b1*x+rnorm(N, mean = 0, sd = sde)
mod6 <- lm(y ~ x)
b0hat <- coef(mod6)[[1]]
b1hat <- coef(mod6)[[2]]
mod6summary <- summary(mod6)
seb1hat <- coef(mod6summary)[2,2]
# the strength of a Montre Carlo simulation is, the probability of 
# repeating the estimation of the regression parameters for a large
# number of automatically generated samples
# obtain a large number of values for a parameter, say b1, and then 
# determine its sampling characteristics.
## 
N <- 40
sde <- 50
x <- food$income
nrsim <- 1000
b0 <- 1000
b1 <- 10
vb1 <- numeric(nrsim) # store the estimate of b1
for (i in 1:nrsim){
  set.seed(12345+10*i)
  y <- b0+b1*x+rnorm(N, mean = 0, sd = sde)
  mod7 <- lm(y ~ x)
  vb1[i] <- coef(mod7)[[2]]
}
mb1 <- mean(vb1)
seb1 <- sd(vb1)
#
plot(density(vb1))
curve(dnorm(x, mb1, seb1), col = "red", add = TRUE)
legend("topright", legend = c("true", "stimulate"), lty = 1,
        col = c("red", "black"))
# 
rm(list = ls()) # clear the envir
