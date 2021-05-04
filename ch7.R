###### chapter 7 using indicator variables
pkgs <- c("bookdown", "PoEdata", "knitr", "xtable", "printr", "effects", 
          "car", "AER", "broom", "stats", "lmtest", "stargazer", "ggplot2")
sapply(pkgs, require, character.only = TRUE)
#### Factor variables
some(utown)
utown$utown <- as.factor(utown$utown)
utown$pool <- as.factor(utown$pool)
utown$fplace <- as.factor(utown$fplace)
# transform()
# mutate()
# cut()
# car::recode()
# doBy::recodeVar()
kable(summary.data.frame(utown), caption = "Summary for 'utown' dataset")
#### Examples
mod4 <- lm(price ~ utown*sqft+age+pool+fplace, utown)
kable(tidy(mod4), caption = "The 'house prices' model")
#
bsqft <- 1000*coef(mod4)[["sqft"]] # that means an extra hundred square feet 
# increases the price by 7612.177 if the house is not near university
bsqft1 <- 1000*(coef(mod4)[[3]] + coef(mod4)[[7]]) # that means an extra hundred 
# square feet increases the price by 8911.581, if the house is  near the 
# university
# that's how interaction terms allow to  distinguishing the marginal effect of  
# a continuous variable for one category form the marginal effect of the same
# continuous variable within another category
## 
names(cps4_small)
mod5 <- lm(wage ~ educ + black*female, cps4_small)
beta1 <- coef(mod5)[[1]]
beta2 <- coef(mod5)[[2]]
delta1 <- coef(mod5)[[3]]
delta2 <- coef(mod5)[[4]]
gamma <- coef(mod5)[[5]]
#
wm <- beta2 # baseline category
blm <- beta2+delta1
wf <- beta2+delata2
blfm <- delta1+delta2+gamma
#
kable(tidy(mod5), caption = "A wage-discrimination model")
# joint hypothesis
hyp <- c("black=0", "female=0", "black:female=0")
tab <- tidy(linearHypothesis(mod5, hyp))
kable(tab,
      caption = "Testing  a joint hypothesis for 'wage' equation")
#### Comparing two regressions: the chow test
dnosouth <- cps4_small[which(cps4_small$south == 0),]
dsouth <- cps4_small[which(cps4_small$south == 1),]
#
mod5ns <- lm(wage ~ educ + black*female, dnosouth)
mod5s <- lm(wage ~ educ + black*female, dsouth)
mod6 <- lm(wage ~ educ + black*female + south/(educ + black*female),
           cps4_small) # full wage model with all terms interacted with variable
# south   
#
stargazer::stargazer(mod6, mod5s, mod5ns, header = FALSE, 
                     type = "text",  style = "qje",
                     title = "Model comparison, 'wage' equation",
                     keep.stat = "n", digits = 2, single.row = TRUE,
                     intercept.bottom = FALSE)
# the number in the parenthese is the standard error of the coefs
#
kable(anova(mod5, mod6),
      caption = "Chow test for the 'wage' equation")
#### Indicator variable in log-linear model
mod1 <- lm(log(wage) ~ educ + female, cps4_small)
approx <- 100*coef(mod1)[["female"]]
exact <- 100*(exp(coef(mod1)[["female"]])-1)
#
approx; exact
#### The linear probability model
data("coke")
View(coke)
#
mod2 <- lm(coke ~ pratio+disp_coke+disp_pepsi, coke)
kable(tidy(mod2), caption = "Linear probability model, the 'coke' example")
#
range(fitted(mod2)) # fitted value can be negative
## graph
b00 <- coef(mod2)[[1]]
b10 <- b00+coef(mod2)[[3]]
b11 <- b10+coef(mod2)[[4]]
b01 <- b11-coef(mod2)[[3]]
b2 <- coef(mod2)[[2]]
#
plot(coke$pratio, coke$coke,
     ylab = "Pr[coke]", xlab = "price ratio")
abline(b00, b2, lty = 2, col = 2)
abline(b10, b2, lty = 3, col = 3)
abline(b11, b2, lty = 4, col = 4)
abline(b01, b2, lty = 5, col = 5)
#
legend("topright", c("00", "10", "11", "01"),
       lty = c(2, 3, 4, 5), col = c(2, 3, 4, 5),
       x.intersp = 0.3, y.intersp = 0.5, seg.len = 0.8)
#### Treatment effects
data("star")
View(star)
#
attach(star)
vars <- c("totalscore", "small", "tchexper", "boy", "freelunch", "white_asian",
          "tchwhite", "tchmasters", "schurban", "schrural")
starregular <- star[which(small == 0),vars]
starsmall <- star[which(small == 1), vars]
detach(star)
# 
stargazer(starregular, type = "text", header = FALSE,
          title = "Dataset 'star' for regular classes")
stargazer(starsmall, type = "text", header = FALSE, 
          title = "Dataset 'star' for small classes")
#
diff <- aggregate(star$totalscore, by = list(Class = star$small), mean)
diffval <- diff[[2]][2] - diff[[2]][1]
#
mod3 <- lm(totalscore ~ small, star)
b2 <- coef(mod3)[[2]] # exactly equal to the mean difference about mean scores
#  between regular and small class
school <- as.factor(star$schid)
mod4 <- lm(totalscore ~ small + tchexper, star)
mod5 <- lm(totalscore ~ small + tchexper + school, star)
b2n <- coef(mod4)[[2]]
b2s <- coef(mod5)[[2]]
# 
b2n; b2s
#
kable(anova(mod4, mod5)) # have significant differences
## check collinearity
# check if the assignments to be treated and control groups are random
mod6 <- lm(small ~ boy + white_asian + tchexper + freelunch, star)
kable(tidy(mod6), caption = "Checking random assignment in the 'star' dataset")
# 
fstat <- glance(mod6)$statistic
pf <- glance(mod6)$p.value
#
fstat; pf # the model is overall insignificant at the 5% level
# finally proves that students' assignment to small or regular classes was random
#### The difference-in-difference estimator
data("njmin3")
View(njmin3)
#
mod1 <- lm(fte ~ nj*d, njmin3)
mod2 <- lm(fte ~ nj*d+
             kfc+roys+wendys+co_owned, njmin3)
mo3 <- lm(fte ~ nj*d+
            southj+centralj+pa1, njmin3)
#
stargazer(mod1, mod2, mod3, 
          type = "text", header = FALSE, keep.stat = "n", digits = 2)
#
tdelta <- summary(mod1)$coefficients[4,3]
##
b1 <- coef(mod1)[[1]]
b2 <- coef(mod1)[[2]]
b3 <- coef(mod1)[[3]]
delta <- coef(mod1)[[4]]
#
C <- b1+b2+b3+delta
E <- b1+b3 # control after
B <- b1+b2 # treatment before
A <- b1
D <- E+(B-A)
#
plot(1, type = "n", xlab = "period", ylab = "fte", xaxt = "n",
     xlim = c(-0.01, 1.01), ylim = c(18, 24))
segments(x0=0, y0=A, x1=1, y1=E, lty=2, col=2) # control
segments(x0=0, y0=B, x1=1, y1=C, lty=3, col=3) # treated
segments(x0=0, y0=B, x1=1, y1=D, lty=4, col=4) # counterfactual
#
legend("topright", legend = c("control", "treated", "counterfactual"),
       lty = c(2, 3, 4), col = c(2, 3, 4),
       x.intersp = 0.3, y.intersp = 0.3, seg.len = 0.8)
axis(side=1, at=c(0,1), labels = NULL)
#### Using panel data
mod3 <- lm(demp ~ nj, njmin3)
kable(tidy(summary(mod3)), caption = "Difference in differences with panel data")
(smod3 <- summary(mod3))
#### R practicum
mod5 <- lm(wage ~ educ+black*female, cps4_small)
mod1
attributes(mod1)
attributes(mod5)
#
smod5 <- summary(mod5)
gmod5 <- summary(mod5)
#
names(mod5)
names(smod5)
names(gmod5)
#
head(mod5$fitted.values)
tail(mod5$residuals)
# 
smod5$r.squared
gmod5$fstatistic
mod5$df.residual
#
nobs(mod5)
fitted(mod5)
resid(mod5)
coef(mod5)
#
lmtest::coeftest(mod5) # coefficients and their statistics
#
tbl <- broom::tidy(mod5) # give the same result of coeftest()
kable(tbl, caption = "Example of using the function 'tidy'")
## ggplot2
fpool <- as.factor(utown$pool)
futown <- as.factor(utown$utown)
#
ggplot(data = utown) + 
  geom_point(mapping = aes(x = sqft, y = price, color = fpool, shape = fpool))
#
ggplot(dat = utown) +
  geom_point(mapping = aes(x = sqft, y = price, color = futown, shape = fpool))
####









