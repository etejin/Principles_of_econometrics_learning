###### chapter 10 Random regressors
pkgs <- c("AER", "lmtest", "broom", "PoEdata", "car", "sandwich", "knitr", "stargazer")
sapply(pkgs, require, character.only = TRUE)
#### The instrumental variable (IV) method
data("mroz")
View(mroz)
#
mroz1 <- mroz[mroz$lfp==1,]
educ.ols <- lm(educ ~ exper + I(exper^2) + mothereduc, mroz1)
kable(tidy(educ.ols), digits = 4, align = 'c', 
      caption = "First stage in the 2SLS model for the 'wage' equation")
#
educHat <- fitted(educ.ols)
wage.2sls <- lm(log(wage) ~ educHat + exper + I(exper^2), mroz1)
kable(tidy(wage.2sls), digits = 4, align = "c",
      caption = "Second stage in the 2SLS model for the 'wage' equation")
# however, in order to get the correct standard errors, we need directly use the R 
# functions, AER::ivreg()
mroz1.ols <- lm(log(wage) ~ educ + exper + I(exper^2), mroz1)
mroz1.iv <- ivreg(log(wage) ~ educ + exper + I(exper^2)|
                         exper + I(exper^2) + mothereduc, 
                       data = mroz1)
# | separates the proper regressors and instrumental lists
# in this model, educ used is equal to the previous wage.2sls, all the fitted(educ), hwr, $
# their variance differs, which the ivreg model calculates the correct one
mroz1.iv1 <- ivreg(log(wage) ~ educ + exper + I(exper^2)|
                          exper + I(exper^2) + mothereduc + fathereduc,
                        data = mroz1)
#
stargazer::stargazer(mroz1.ols, wage.2sls, mroz1.iv, mroz1.iv1,
                     title = "Wage equation: OLS, 2SLS, and IV models Compared",
                     header = FALSE,
                     type = 'text',
                     keep.stat = "n",
                     omit.table.layout = 'n',
                     star.cutoffs = NA,
                     digits = 4,
                    # single.row = TRUE,
                     intercept.bottom = FALSE,
                     column.labels = c("OLS", "explicit 2SLS", "IV mothereduc",
                                       "IV mothereduc and fathereduc"),
                     dep.var.labels.include = FALSE,
                     model.numbers = FALSE,
                     dep.var.caption = "Dependent variable: wage",
                     model.names = FALSE,
                     star.char = NULL)
## test whether the intruments are weak or strong
educ.ols <- lm(educ ~ exper + I(exper^2) + mothereduc + fathereduc, mroz1)
tab <- tidy(educ.ols)
kable(tab, digits = 4,
      caption = "The 'educ' first-stage model")
#
car::linearHypothesis(educ.ols, c("mothereduc = 0", "fathereduc = 0"))
# indicating at least one instrument is strong
# a rule of thumb  require to soundly reject the H0 at a value of F greater 10,
# or for only one instrument,  t-statistic greater than 3.16 to make sure the instrument is 
# strong
#### Specification tests
summary(mroz1.iv1, diagnostics = TRUE)
# automatically calculate the weak instruments test, Hansman test for endogeneity, Sargen test
# weak instruments test: at least one instrument is strong
# Hansman test: barely reject, educ is marginally endogenous
# Sargen test: the extra instruments are valid
## Cragg-Donald F-statistic
mroz1 <- mroz[which(mroz$wage>0),]
nwifeinc <- (mroz1$faminc-mroz1$wage*mroz1$hours)/1000
#
G <- 2; L <- 2; B <- 2; N <- nrow(mroz1)
#
x1 <- resid(lm(mtr ~ kidsl6+nwifeinc, mroz1))
x2 <- resid(lm(educ ~  kidsl6+nwifeinc, mroz1))
z1 <- resid(lm(mothereduc ~ kidsl6+nwifeinc, mroz1))
z2 <- resid(lm(fathereduc ~ kidsl6+nwifeinc, mroz1))
#
X <- cbind(x1, x2)
Y <- cbind(z1, z2)
#
rB <- min(cancor(X,Y)$cor)
#
CraggDonaldF <- ((N-G-B)/L)/((1-rB^2)/(rB^2))
CraggDonaldF
# smaller than critical value 4.58
# reject H0
#
devtools::install_github("beniaminogreen/cragg")
require("cragg")
#
cragg_donald(~ kidsl6+nwifeinc, # exogeneous
             ~ mtr + educ, # endogenours
             ~ mothereduc + fathereduc, # instruments
             data = mroz1)
#
# stock_yogo_test(~ kidsl6+nwifeinc,
#                 ~ mtr + educ,
#                 ~ mothereduc + fathereduc,
#                 B = 0.05,
#                 size_bias = 'bias',
#                 data = mroz1)
####