###### Chapter 5 The multiple regression model
pkgs <- c("PoEdata", "knitr", "xtable", "printr", "effects", "car", "AER", "broom")
sapply(pkgs, require, character.only = TRUE)
#### sample
data("andy")
View(andy)
s = tidy(andy)[,c(1:5, 8, 9)] # turn a data frame into a tidy tibble
kable(s, caption = "Summary statistics for dataset $andy$")
#
mod1 <- lm(sales ~ price + advert, andy)
smod1 <- round(data.frame(xtable(summary(mod1))), 4)
kable(smod1, caption = "The basic multiple regression model",
      col.names = c("coefficients", "Std.Error", "t-value", "p-value"),
      align = "c", digits = 3)
# 
effprice <- effect("price", mod1)
plot(effprice, main = "The partial effect  of price in the basic andy regression")
# show the predicted levels of sales and its 95% band
summary(effprice) # 95% band of predicted level of sales
#
alleffandy <- allEffects(mod1)
plot(alleffandy) # plot all variables in the model
## 
mod2 <- lm(sales ~ price + advert + I(advert^2), andy)
summary(mod2)
plot(effect("I(advert^2)", mod2)) # the interested variable should be indicated specifically
# in the effect function
##
smod1 <- summary(mod1)
#
df <- df.residual(mod1)
# df <- smod1$df[2] 
N <- nobs(mod1)
# N <- nrow(andy)
b1 <- coef(mod1)[[1]]
b2 <- coef(mod1)[[2]]
b3 <- coef(mod1)[[3]]
sighat2 <- smod1$sigma^2
# sighat2 <- sigma(mod1)^2
anov <- anova(mod1)
SSE <- anov[3,2]
SSR <- anov[1, 2] + anov[2, 2]
SST <- sum(anov[,2])
#
kable(data.frame(vcov(mod1)), align = 'c', digits = 3,
      caption = "The coefficient covariance matrix",
      colnames = c("(Intercept)", "price", "advert"))
#### Interval estimation in the multiple regression
varb1 <- vcov(mod1)[1,1]
varb2 <- vcov(mod1)[2,2]
varb3 <- vcov(mod1)[3,3]
covb1b2 <- vcov(mod1)[1,2]
covb2b3 <- vcov(mod1)[2,3]
covb1b3 <- vcov(mod1)[1,3]
#
seb2 <- sqrt(varb2)
seb3 <- sqrt(varb3)
#
tc <- qt(1-alpha/2, df)
lowb2 <- b2 - tc*seb2
upb2 <- b2 + tc*seb2
lowb3 <- b3 - tc*seb3
upb3 <- b3 + tc*seb3
#
lowb2; upb2; lowb3; upb3
#
confint(mod1)
confints <- confint(mod1, parm = c("price", "advert"), level = 0.95)
#
kable(data.frame(confints), caption = "Confidence interval for 'price' and 'advert'",
      align = "c", col.names = c("lowb", "upb"), digits = 4)
##
a1 <- 0
a2 <- -0.4
a3 <- 0.8
L <- a1*b1+a2*b2+a3*b3
varL <- a1^2*varb1+a2^2*varb2+a3^2*varb3+2*a1*a2*covb1b2+2*a1*a3*covb1b3+2*a2*a3*covb2b3
seL <- sqrt(varL)
lowL <- L-seL*tc
upL <- L+seL*tc
#
lowL; upL
##
a <- c(0, -0.4, 0.8) # vector
b <- as.numeric(coef(mod1)) # vector of coefficients
L <- sum(a*b) # sum  of elementwise products
V <- vcov(mod1)
A <- as.vector(a)
varL <- as.numeric(t(A) %*% V %*% A)
#### Hypothesis testing in multiple regression model
### two-tail
## t-tes
tb2 <- b2/seb2
#
tb2; tc # reject H0
## p-value
pval <- 2*(1-pt(abs(tb2), df)) # for two tail test, calculate the p-value of b2
# reject H0, compare it  to the 0.05, less than the area we reject
##
tb3 <- b3/seb3
tb3; tc # reject H0
## 
pvalb3 <- 2*(1-pt(abs(tb3), df))
# reject H0
### one tail 
## left-tail
tc <- -qt(1-alpha, df)
# tc <- qt(alpha, df)
tb2 <- b2/seb2
pb2 <- pt(tb2, df)
# 
tc; tb2 # reject the H0
pb2; alpha # reject the H0
## right-tail
tb3 <- (b3-1)/seb3
pvalb3 <- 1-pt(tb3, df)
#
pvalb3; alpha # fail to reject
### matrix form of testing hypothesis
## right one-tailed
A <- as.vector(c(0, -0.2, -0.5))
V <- vcov(mod1)
L <- sum((A*coef(mod1))) # get the L
# L <- as.numeric(t(A) %*% coef(mod1))
varL <- as.numeric((t(A) %*% V %*% A)) # get the variance
seL <- as.numeric(sqrt(L))
tval <- L/seL
pvalm <- 1-pt(tval, df)
#
tc <- qt(1-alpha, df)
tval; tc # fail to reject
#
pvalm; alpha # fail to reject H0
## two tail linear hypothesis
hypothesis <- "-0.2*price = 0.5*advert"
test <- car::linearHypothesis(mod1, hypothesis)  
Fstats <- test$F[2] # it uses F statistics, rather than t statistics
pval <- 1-pf(Fstats, 1, df) # F-statistics has two df, the first df is the number of
# simultaneous hypothesis to be tested, the second df is the df in the model, (N-K)
# pval <- test$`Pr(>F)`[2]
pval; alpha # fail to reject the H0
# 
kable(test, caption = "The 'linearHypothesis(' object")
## two tail multiple linear hypothese
hypothesis <- c("0.4*price = -0.8*advert",
                "-0.6*price = 0.3*advert")
test <- car::linearHypothesis(mod1, hypothesis)
kable(test, 
      caption = "Two simutaneous hypothesis performed by 'linearHypothesis()' function")
#### Polynomial regression models
mod2 <- lm(sales ~ price + advert + I(advert^2), andy)
smod2 <- summary(mod2)
tbl <- data.frame(xtable(smod2))
kable(tbl, caption = "The quadratic version of the $andy$ model",
      digits = 3, align = "c", 
      col.names = c("Estimate", "Std.Error", "t", "p-Value"))
#
adlevels <- c(0.5, 2)
b3 <- coef(mod2)[[3]]
b4 <- coef(mod2)[[4]]
DsDa <- b3+b4*adlevels
#
DsDa # diminishing returns to advertising at any given price
## calculate the optimal advertising level
df <- mod2$df.residual
tc <- qt(1-alpha/2, df)
#
g <- (1-b3)/(2*b4)
g3 <- -1/(2*b4)
g4 <- -(1-b3)/(2*b4^2)
#
varb3 <- vcov(mod2)[3,3]
varb4 <- vcov(mod2)[4,4]
covb3b4 <- vcov(mod2)[3,4]
varg <- g3^2*varb3 + g4^2*varb4 + 2*g3*g4*covb3b4
seg <- sqrt(varg)
#
lowbg <- g-tc*seg
upbg <- g+tc*seg
g; lowbg; upbg
#### Interaction terms in linear regression
data("pizza4")
View(pizza4)
#
mod3 <- lm(pizza ~ age*income, pizza4)
summary(mod3)
#
inc <- c(25, 90)
b2 <- coef(mod3)[[2]]
b4 <- coef(mod3)[[4]]
DpDa <- b2+b4*inc
# 
DpDa
##
meduc <- mean(cps4_small$educ)
mexper <- mean(cps4_small$exper)
#
mod4 <- lm(log(wage) ~ educ*exper+I(exper^2), cps4_small)
smod4 <- data.frame(xtable(summary(mod4)))
#
b3 <- coef(mod4)[[3]]
b4 <- coef(mod4)[[4]]
b5 <- coef(mod4)[[5]]
#
pDwDe <- (b3+b4*meduc+2*b5*mexper)*100
pDwDe
#
kable(smod4, caption = "Wage equation with equation with interaction and quadratic terms")
# the interaction term is in the last
#
pDwDe <- (b3+2*b4*mexper+b5*meduc)*100 
pDwDe
#### Goodness-of-fit in multiple regression
mod1
smod1
Rsq <- smod1$r.squared
anov <- anova(mod1)
kable(data.frame(anov), caption = "Anova table for the basic *andy* mode")
#
SST <- sum(anov[,2])
SSR <- sum(anov[1:2, 2])
SSE <- anov[3,2]
#
SSR/SST # equal to Rsq
####











