###### chapter 6 further inference in multiple regression
pkgs <- c("PoEdata", "knitr", "xtable", "printr", "effects", "car",
          "AER", "broom", "stats")
sapply(pkgs, require, character.only = TRUE)
#### Testing simultaneous hypotheses
N <- NROW(andy)
K <- 4 # four betas in the unrestricted model
J <- 2 # H0 has two restrictions
#
fc <- qf(1-alpha, J, N-K) # calculate the critical F value
#
mod1 <- lm(sales ~ price + advert + I(advert^2), andy)
anov <- anova(mod1)
kable(anov, caption = "anova table of andy model 1")
#
SSEu <- anov[4,2]
#
mod2 <- lm(sales ~ price, andy) # restricted model
anov <- anova(mod2)
kable(anov, caption = "anova table of andy model 2")
SSEr <- anov[2,2]
#
fval <- ((SSEr-SSEu)/J)/(SSEu/(N-K))
#
fval; fc # reject the H0
#
pval <- 1-pf(fval, J, N-K)
#
pval; alpha # reject H0
##
Hnull <- c("advert=0", "I(advert^2)=0")
car::linearHypothesis(mod1, Hnull)
##
smod1 <- summary(mod1) 
fval <- smod1$fstatistic # r automatically calculate Linear hypotheses, 
# we can retrive it from the output of smod1
#
kable(broom::tidy(smod1), caption = "Tidy's 'summary(mod1)' output")
kable(broom::tidy(mod1), caption = "Tidy(mod1) output")
# broom::tidy(smod1) has the same result of broom::tidy(mod1)
#
broom::glance(mod1)$statistic # retrieves the F-statistic
names(glance(mod1)) # see what's available in 'glance'
kable(broom::glance(mod1), digits = 2, 
      caption = "Function 'glance(mod1)' output",
      col.names = c("Rsq", "AdjRsq", "sig", "F", "pF", "K", "logL",
                    "AIC", "BIC", "dev", "df.residual", "N"))
##
Hnull <- c("advert+3.8*I(advert^2)=1",
           "(Intercept)+6*price+1.9*advert+3.61*I(advert^2)=80")
lhout <- tidy(linearHypothesis(mod1, Hnull))
kable(lhout, 
      caption = "Joint hypotheses with the 'linaerHypothesis' function")
kable(tidy(linearHypothesis(mod1)))
#### Ommited variable bias
data("edu_inc")
View(edu_inc)
mod1 <- lm(faminc ~ he+we, edu_inc)
mod2 <- lm(faminc ~ we, edu_inc)
kable(tidy(mod1), caption = "The true model")
kable(tidy(mod2), caption = "The untrue model ('he' omitted)")
#### irrelevant variables
mod3 <- lm(faminc ~ he + we+ kl6, edu_inc)
mod4 <- lm(faminc ~ he + we + kl6 + xtra_x5 + xtra_x6, edu_inc)
kable(tidy(mod3), caption = "Correct 'famnic' model")
kable(tidy(mod4), caption = "Incorrect 'famnic' with irrelevant variables")
# including irrelevant variables may incorrectly dimish the significance of the
# 'true' regressors
#### Model selection criteria
mod1 <- lm(faminc ~ he, edu_inc)
mod2 <- lm(faminc ~ he + we, edu_inc)
mod3 <- lm(faminc ~ he + we + kl6, edu_inc)
mod4 <- lm(faminc ~ he + we + kl6 + extra_x5 + extra_x6, edu_inc)
#
r1 <- as.numeric(glance(mod1))
r2 <- as.numeric(glance(mod2))
r3 <- as.numeric(glance(mod3))
r4 <- as.numeric(glance(mod4))
#
tab <- data.frame(rbind(r1, r2, r3, r4)[,c(1, 2, 8, 9)])
rownames(tab) <- c("mod1", "mod2", "mod3", "mod4")
#
kable(tab, caption =  "Model comparison, 'faminc'", 
      digits = 4,
      col.names = c("Rsq", "AdjRsq", "AIC", "BIC"))
##
smod1 <- summary(mod1)
Rsq <- smod1$r.squared
AdjRsq <- smod1$adj.r.squared
aic <- stats::AIC(mod1)
bic <- stats::BIC(mod1)
#
Rsq; AdjRsq; aic; bic
##
lmtest::resettest(mod3, power = 2, type = "fitted")
lmtest::resettest(mod3, power = 2:3, type = "fitted")
# the number labeled 'RESET' in the output is the F-statistic of the test under
# H0 followed by two types of df of the F distribution and the p-value. In this
# case, both p-values are less than 0.05, indicating that the model marginally
# fails the specification test and some higher order terms may be necessary
##
lmtest::resettest(mod3, power = 2:3, type = "regressor")
#### Collinearity
data("cars")
mod1 <- lm(mpg ~ cyl, cars)
kable(tidy(mod1), caption = "A simple linear 'mpg' model")
#
mod2 <- lm(mpg ~ cyl + eng + wgt, cars)
kable(tidy(mod2), caption = "Multivariate linear 'mpg' model")
# the high sensitivity of the estimates when other variables are introduced
# is also a sign of collinearity
## vif
tab <- tidy(vif(mod2))
kable(tab, caption = "Variance inflation factors for the 'mpg' regression model",
      col.names = c("regressors", "VIF"))
# cyl and eng fail the collinearity
#### Prediction and Forecasting
newdata <- data.frame(price = 6, advert = 1.9)
mod3 <- lm(sales ~ price + advert + I(advert^2), andy)
tbl <- tidy(predict(mod3, newdata = newdata, interval = "prediction"))
kable(tbl, caption = "Forecasting in the quadratic 'andy' model",
      col.names = c("fitted value", "lower", "upper"))
####






