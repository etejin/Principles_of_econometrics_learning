###### chapter 8 Heteroskedasticity
pkgs <- c("lmtest", "broom", "PoEdata", "car", "sandwich", "knitr", "stargazer", "nlme")
sapply(pkgs, require, character.only = TRUE)
#### Spotting heteroskedasticity in scatter plots
mod1 <- lm(food_exp ~ income, food)
with(food,
     plot(food_exp ~ income, type = "p",
     xlab = "income", ylab = "food expenditure"))
abline(mod1)
#
res <- resid(mod1)
yhat <- fitted(mod1)
plot(res ~ food$income, xlab = "income", ylab = "residuals")
plot(res ~ yhat, xlab = "fitted variables", ylab = "residuals")
#### Heteroskedasticity tests
## breusch-pagan heteroscedasticity test
alpha <- 0.05
ressq <- resid(mod1)^2
# test equation
modres <- lm(ressq ~ income, food) # h() function, linear relationship between 
# residual squares and regressors
N <- nobs(modres)
gmodres <- glance(modres)
S <- gmodres$df # numbers of betas in model and minus 1
# Chi-square is always a right-tail test
chisqcr <- qchisq(1-alpha, S) # X^2 critical value
#
Rsqres <- gmodres$r.squared # R^2
chisq <- N*Rsqres # X^2 = R^2 * N
pval <- 1-pchisq(chisq, S)
#
chisqcr; chisq; alpha; pval # reject H0
## White version of the residual equation
modres <- lm(ressq ~ income + I(income^2), food)
gmodres <- glance(modres)
#
Rsq <- gmodres$r.squared # R^2
S <- gmodres$df # number of betas in model
chisq <- N*Rsq # X^2 value
pval <- 1-pchisq(chisq, S)
#
chisq; pval # reject the H0
#
kable(tidy(lmtest::bptest(mod1)),
      caption = "Breusch-Pagan heteroskedasticity test")
# more easier way to calculate robust version of breusch-pagan heteroscedasticity
# tests
## goldfeld-quandt heteroskedasticity test
data("cps2")
m <- cps2[which(cps2$metro == 1),]
r <- cps2[which(cps2$metro == 0),]
wg1 <- lm(wage ~ educ+exper, m)
wg0 <- lm(wage ~ educ+exper, r)
#
df1 <- wg1$df.residual
df0 <- wg0$df.residual
sig1squared <- glance(wg1)$sigma^2
sig0squared <- glance(wg0)$sigma^2
#
fstat <- sig1squared/sig0squared
#
Flc <- qf(alpha/2, df1, df0) # lower critical F
Fuc <- qf(1-alpha/2, df1, df0) # upper critical F
#
fstat; Flc; Fuc
#
Fc <- qf(1-alpha, df1, df0) # right-tail test
fstat; Fc
##
li <- food[which(food$income <= median(food$income)),]
hi <- food[which(food$income > median(food$income)),]
#
eqli <- lm(food_exp ~ income, li)
eqhi <- lm(food_exp ~ income, hi)
#
dfli <- eqli$df.residual
dfhi <- eqhi$df.residual
#
sigsqli <- glance(eqli)$sigma^2
sigsqhi <- glance(eqhi)$sigma^2
#
fstat <- sigsqhi/sigsqli
Fc <- qf(1-alpha, dfhi, dfli)
pval <- 1-pf(fstat, dfhi, dfli)
#
fstat; Fc; pval # reject H0
##
gqt <- lmtest::gqtest(mod1, point = 0.5, alternative = "greater",
                      order.by = food$income)
kable(tidy(gqt),
      caption = "R function 'lmtest::gqtest' with the 'food' equation")
#### Heteroskedasticity-consistent standard errors
kable(tidy(mod1), caption = "Regular standard errors in the 'food' equation")
#
cov1 <- car::hccm(mod1, type = "hc1") # white-robust covariance matrix
food.HC1 <- lmtest::coeftest(mod1, vcov.=cov1) # performing z and (quasi-) t Wald 
# tests of estimated coefficients
kable(tidy(food.HC1), 
      caption = "Robust (HC1) standard errors in the 'food' equation")
# lower p-values with robust standard errors is an exception rather than rule
##
mod2 <- lm(sales ~ price + advert, andy)
bp <- lmtest::bptest(mod2)
#
b2 <- coef(mod2)[[2]]
b3 <- coef(mod2)[[3]]
H0 <- "price+advert=0" # null hypothesis
kable(tidy(car::linearHypothesis(mod2, H0,
           vcov = car::hccm(mod2, type = "hc1"))),
      caption = "Linear hypothesis with robust standard errors")
#
kable(tidy(car::linearHypothesis(mod2, H0)),
      caption = "Linear hypothesis with regular standard errors") # f-statistics
##
sandwich::vcovHAC(mod2) # also output an heterocadascity-consistent covariance 
# matrix
car::hccm(mod2, type = "hc1")
##
mod3 <- lm(sales ~ price, andy)
if(require("sandwich"))
  waldtest(mod2, mod3, vcov = vcovHC)
#
if(require("car"))
   waldtest(mod2, mod3, vcov = hccm) # here vcov is only the function for estimating
# the variance matrix of the more general model
# lmtest::encomptest() # compare unested models 
#### GLS:known from variance
w <- 1/food$income # r takes square root of the weight value
food.wls <- lm(food_exp ~ income, weights = w, data = food)
vcvfoodeq <- lmtest::coeftest(mod1, vcov.= cov1) 
kable(tidy(mod1), caption = "OLS estimates for the 'foood' equation")
#
kable(tidy(food.wls), caption = "WLS estimates for the 'food' equation")
#
kable(tidy(vcvfoodeq), 
      caption = "OLS estimates for the 'food' equation with robust standard errors")
#### Grouped data
rural.lm <- lm(wage ~ educ+exper, cps2, subset = (metro == 0))
sigR <- sigma(rural.lm)
#
metro.lm <- lm(wage ~ educ+exper, cps2, subset = (metro == 1))
sigM <- sigma(metro.lm)
# create a vector of weights
cps2$wght <- rep(0, nrow(cps2))
for (i in 1:1000){
  if (cps2$metro[i] == 0) {cps2$wght[i] <- 1/sigR^2}
  else {cps2$wght[i] <- 1/sigM^2}
}
#
wge.fgls <- lm(wage ~ educ+exper+metro, weights=wght, data = cps2)
wge.lm <- lm(wage ~ educ+exper+metro, data = cps2)
#
wge.hce <- lmtest::coeftest(wge.lm, vcov.=car::hccm(wge.lm)) # OLS with HC1 std 
#
stargazer(rural.lm, metro.lm, wge.fgls, wge.hce,
          header = FALSE,
          title = "OLS vs. FGLS estimates for the 'cps2' data",
          type = "text",
          keep.stat = "n", # what statistics to print
          omit.table.layout = "n",
          star.cutoffs = NA,
          digits = 3,
          intercept.bottom = FALSE,
          column.labels = c("Rural", "Metro", "FGLS", "HC1"),
          dep.var.labels.include = FALSE,
          model.numbers = FALSE,
          dep.var.caption = "Dependent variable : wage",
          model.names = FALSE,
          star.char = NULL) # compare coefs and (stds)
#### GLS: unknown form of variance
food.ols <- lm(food_exp ~ income, food)
ehatsq <- resid(food.ols)^2
#
sighatsq.ols <- lm(log(ehatsq) ~ log(income), food)
#
vari <- exp(fitted(sighatsq.ols))
food.fgls <- lm(food_exp ~ income, weights = 1/vari, data = food)
#
stargazer(food.ols, food.HC1, food.wls, food.fgls,
          header = FALSE,
          title = "Comparing various 'food' models",
          type = "text",
          keep.stat = "n",
          omit.table.layout = "n",
          star.cutoffs = NA,
          digits = 3,
          intercept.bottom = FALSE,
          column.labels = c("OLS", "HC1", "WLS", "FGLS"),
          dep.var.labels.include = FALSE,
          model.numbers = FALSE,
          dep.var.caption = "Dependent variable: 'food expenditure'",
          model.names = FALSE,
          star.char = NULL)
#### Heteroscedasticity in the linear probablity model
coke.ols <- lm(coke ~ pratio+disp_coke+disp_pepsi, coke)
coke.hc1 <- lmtest::coeftest(coke.ols, vcov. = car::hccm(coke.ols, type = "hc1"))
p <- fitted(coke.ols)
# truncate negative or >1 values of p
pt <- p
pt[pt<0.01] <- 0.01
pt[pt>0.99] <- 0.99
#
sigsq <- pt*(1-pt) # calculate the variance of response y
wght <- 1/sigsq # get the weight
#
coke.gls.trunc <- lm(coke ~ pratio+disp_coke+disp_pepsi, 
                     weights = wght, data = coke)
# eliminate negative or >1 values of p
p1 <- p
p1[p1<0.01 | p1>0.99] <- NA # it must be or, that means pi<0.01 or p1>0.99 will be
# into NA; if we use &, then, only when condition satisfies both pi<0.01 and p1>0.99
# then it will convert it into NA.
p1[p1<0.01]
p1[p1>0.99]
#
sigsq <- p1*(1-p1)
wght <- 1/sigsq
#
coke.gls.omit <- lm(coke ~ pratio+disp_coke+disp_pepsi, 
                    weights = wght, data = coke)
#
stargazer::stargazer(coke.ols, coke.hc1, coke.gls.trunc, coke.gls.omit,
                     header = FALSE, 
                     title = "Comparing varisous 'coke' models",
                     type = "text",
                     keep.stat = "n",
                     omit.table.layout = "n",
                     star.cutoffs = NA,
                     digits = 4,
                     single.row = TRUE,
                     intercept.bottom = FALSE,
                     column.labels = c("OLS", "HC1", "GLS-trunc", "GLS-omit"),
                     dep.var.labels.include = FALSE,
                     dep.var.caption = "Dependent variable:'choice of coke'",
                     model.names = FALSE,
                     star.char = NULL)
####






