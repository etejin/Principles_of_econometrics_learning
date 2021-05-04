###### Chapter 3 Interval estimation and hypothesis testing
pkgs <- c("xtable", "PoEdata", "knitr")
sapply(pkgs, require, character.only = TRUE)
#### 
# interval estimate, confidence interval, includes the true parameter with 
# a given probability
# a coefficient of the linear regression model such as b1 is normally 
# distributed with its mean equal to the population parameter ß1
# a variance that depends on the population variance sig2 and sample size
####
# The quantile is defined as the smallest value x such that F(x) >= p, where 
# F is the distribution function.
####
## pnorm, cdf:culmulative density function
# Suppose widgit weights produced at Acme Widgit Works have weights that are
# normally distributed with mean 17.46 grams and variance 375.67 grams. What
# is the probability that a randomly chosen widgit weighs more then 19 grams?
(1-pnorm(19, 17.46, sqrt(375.67)))
pnorm(19, 17.46, sqrt(375.67), lower.tail = FALSE)
## qnorm: inverse-look up, cdf:culmulative density function
# Suppose IQ scores are normally distributed with mean 100 and standard 
# deviation 15. What is the 95th percentile of the distribution of IQ scores?
qnorm(0.95, 100, 15)
## dnorm, pdf:probability density function
## rnorm
x <- rnorm(1000, 100, 15) # generate 1000 normal random numbers
hist(x, prob = TRUE)
xx <- seq(min(x), max(x), len = 100)
lines(xx, dnorm(xx, 100, 15))
###
## pbinom, cdf
# Suppose widgits produced at Acme Widgit Works have probability 
# 0.005 of being defective. Suppose widgits are shipped in cartons 
# containing 25 widgits. What is the probability that a randomly chosen 
# carton contains no more than one defective widgit?
pbinom(q = 1, size = 25, prob = 0.005)
## qbinom: cdf
## dbinom, pf: probability function, for discrete data
# Suppose widgits produced at Acme Widgit Works have probability 0.005 of
# being defective. Suppose widgits are shipped in cartons containing 25 
# widgits. What is the probability that a randomly chosen carton contains 
# exactly one defective widgit?
dbinom(x = 1, size = 25, prob = 0.005)
####
## the difference between x, q, p:
# x: the exactly this number P(X = 1)
# q: the number of interval, P(X <= 1)
# p: the area, F(x) <= p
#### Get the coefs interval
data("food")
alpha <- 0.05 # the significant level
mod1 <- lm(food_exp ~ income, food)
smod1 <- summary(mod1)
# b2 +- tc * seb2
b2 <- coef(mod1)[[2]] # get b2 coef
seb2 <- coef(smod1)[2,2] # get standard deviation or standard error of b2
df <- df.residual(mod1) # get the df
tc <- qt(1-alpha/2, df) # get the t critical value of the b2
# 
lowb <- b2 - tc*seb2 
upb <- b2 + tc*seb2
lowb; upb
##
ci <- confint(mod1)
#### Confidence intervals in repeated samples
data("table2_2")
View(table2_2)
mod2 <- lm(y1 ~ x, table2_2)
#
tc <- dt(1-alpha/2, df.residual(mod2)) # get the critical t value
#
lowb1 <- rep(0, 10)
upb1 <- rep(0, 10)
lowb2 <- rep(0, 10)
upb2 <- rep(0, 10)
#
for(i in 2:11){
  dat <- data.frame(cbind(table2_2[,1], table2_2[,i]))
  names(dat) <- c("x", "y")
  mod2 <- lm(y ~ x, dat)
  smod1 <- summary(mod2)
  b1 <- coef(mod1)[[1]]
  b2 <- coef(mod1)[[2]]
  seb1 <- coef(smod1)[1, 2]
  seb2 <- coef(smod1)[2, 2]
  lowb1[i-1] <- b1-tc*seb1
  upb1[i-1] <- b1+tc*seb1
  lowb2[i-1] <- b2-tc*seb2
  upb2[i-1] <- b2+tc*seb2
}
table <- data.frame(lowb1, upb1, lowb2, upb2)
kable(table,
      caption = "Confidence interval for $b_{1}$ and $b_{2}$",
      aligh = "c")
?kable
#### Hypothesis testing
mod1 <- lm(food_exp ~ income, food)
smod1 <- summary(mod1)
table <- data.frame(xtable(mod1)) # xtable, convert a R object to an xtable
# object, can then be printed as LaTeX or HTML table
kable(table, caption = "Regression output showing the coefficients")
##
b2 <- coef(mod1)[[2]] # get b2
seb2 <- coef(smod1)[2, 2] # seb2 <- sqrt(vcov(mod1)[2, 2])
t <- b2/seb2
df <- df.residual(mod1)
tc <- qt(1-alpha/2, df)
##
t > tc # true, then in the rejection area
#
curve(dt(x, df), -2.5*seb2, 2.5*seb2, ylab = "", xlab = "t",
      main = "A two-tail hypothesis testing for b2 in the food exmaple") 
# plot density function of respective t statistics
abline(v = c(-tc, tc, t), col = c("red", "red", "blue"),
       lty = c(2, 2, 3)) # show t value and tc 
legend("topleft", legend = c("-tr", "t", "tr"), 
       col = c("red", "red", "blue"),
       lty = c(2, 2, 3), x.intersp = 0.3, y.intersp = 0.3, seg.len = 0.8)
### One-tailed
## right side
t <- (b2-5.5)/seb2
tc <- qt(1-alpha, df) # here, alpha isn't divided by two
#
t > tc # reject H0 
curve(dt(x, df), -2.5*seb2, 2.5*seb2, ylab = "", xlab = "t",
      main = "A right one-tailed hypothesis testing for b2 in the food example")
abline(v = c(tc, t), col = c("red", "blue"), lty = c(2, 3))
legend("topleft", legend = c("tc", "t"), col = c("red", "blue"),
       lty = c(2, 3), x.intersp = 0.3, y.intersp = 0.3, seg.len = 0.8)
## left side
t <- (b2-15)/seb2
tc <- qt(alpha, df)
#
t < tc # reject H0
#
curve(dt(x, df), -2.5*seb2, 2.5*seb2, ylab = "", xlab = "t",
      main = "A left one-tailed hypothesis testing for b2 in the food example")
abline(v = c(t, tc), col = c("blue", "red"), lty = c(3,2))
legend("topright", legend = c("t", "tc"), col = c("blue", "red"),
       lty = c(3, 2), x.intersp = 0.3, y.intersp = 0.3, seg.len = 0.8)
###
table <- data.frame(round(xtable(smod1), 3))
kable(table, caption = "Regression output for the 'food' model")
#### P-value
## two-tailed
t <- b2/seb2
p <- 2*(1-pt(abs(t), df))
## right one-tailed
t <- (b2-5.5)/seb2
p <- 1-pt(t, df)
## left one-tailed
t <- (b2-15)/seb2
p <- pt(t, df)
###
curve(dt(x, df), -2.5*seb2, 2.5*seb2, ylab = "", xlab = "t",
      main = "The p-value in two-tail hypothesis testing")
abline(v = c(-t, t), col = c("red", "red"), lty = c(2, 4))
legend("topright", legend = c("-t", "t"), col = c("red", "red"), lty = c(2,4),
       x.intersp = 0.3, y.intersp = 0.3, seg.len = 0.8)
###
table <- data.frame(round(xtable(smod1),3))
kable(table, caption = "Regression output in showing p-values")
#### Testing linear combinations of parameters
x <- 20 # when income is 20,000s
tc <- qt(1-alpha/2, df) # get the critical t value, create the rejection region
# right of tc
b1 <- coef(mod1)[[1]] # get b1
b2 <- coef(mod1)[[2]] # get b2
varb1 <- vcov(mod1)[1,1] # get the variance of b1
varb2 <- vcov(mod1)[2,2] # get the variance of b2
covb1b2 <- vcov(mod1)[1, 2] # get the covariates of b1 and b2
L <- b1+b2*x # estimated L
varL = varb1 + x^2*varb2 + 2*x*covb1b2 # var(L)
seL <- sqrt(varL) #  get the standard error of L
lowbL <- L-tc*seL
upbL <- L+tc*seL
#
lowbL;upbL
###
# 
shadenorm <-  function(below=NULL, above=NULL, pcts = c(0.025,0.975), mu=0, sig=1, numpts = 500, color = "gray", dens = 40,
                     justabove= FALSE, justbelow = FALSE, lines=FALSE,between=NULL,outside=NULL){
  if(is.null(between)){
    below = ifelse(is.null(below), qnorm(pcts[1],mu,sig), below)
    above = ifelse(is.null(above), qnorm(pcts[2],mu,sig), above)
  }
  if(is.null(outside)==FALSE){
    below = min(outside)
    above = max(outside)
  }
  lowlim = mu - 4*sig
  uplim  = mu + 4*sig
  x.grid = seq(lowlim,uplim, length= numpts)
  dens.all = dnorm(x.grid,mean=mu, sd = sig)
  if(lines==FALSE){
    plot(x.grid, dens.all, type="l", xlab="X", ylab="Density")
  }
  if(lines==TRUE){
    lines(x.grid,dens.all)
  }
  if(justabove==FALSE){
    x.below    = x.grid[x.grid<below]
    dens.below = dens.all[x.grid<below]
    polygon(c(x.below,rev(x.below)),c(rep(0,length(x.below)),rev(dens.below)),col=color,density=dens)
  }
  if(justbelow==FALSE){
    x.above    = x.grid[x.grid>above]
    dens.above = dens.all[x.grid>above]
    polygon(c(x.above,rev(x.above)),c(rep(0,length(x.above)),rev(dens.above)),col=color,density=dens)
  }
  if(is.null(between)==FALSE){
    from = min(between)
    to   = max(between)
    x.between    = x.grid[x.grid>from&x.grid<to]
    dens.between = dens.all[x.grid>from&x.grid<to]
    polygon(c(x.between,rev(x.between)),c(rep(0,length(x.between)),rev(dens.between)),col=color,density=dens)
  }
}
#
curve(dt(x, df), -2.5*seb2, 2.5*seb2)
shadenorm(above = 1.6, justabove = TRUE)
segments(1.6, 0, 1.6, 0.2, col = "blue", lty = 3)
legend("topleft", legend = "t", col = "blue", lty = 3,
       x.intersp = 0.3, y.intersp = 0.3, seg.len = 0.8)
##
curve(dt(x, df), -2.5*seb2, 2.5*seb2)
shadenorm(above = -1.6, justabove = TRUE)
segments(-1.6, 0, -1.6, 0.2, col = "blue", lty = 3)
legend("topright", legend = "t", col = "blue", lty = 3,
       x.intersp = 0.3, y.intersp = 0.3, seg.len = 0.8)
###
t <- (L-250)/seL
tc <- qt(1-alpha/2, df)
#
t > tc # true, reject H0
##
p <- 2*(1-pt(abs(t), df)) 
p < alpha # true, reject H0
####






