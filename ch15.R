###### chapter 15 Panel data models
pkgs <- c("plm", "tseries", "dynlm", "vars", "nlWaldTest", "lmtest",
          "broom", "PoEdata", "car", "sandwich", "knitr", "forecast",
          "systemfit", "AER", "xtable")
sapply(pkgs, require, character.only = TRUE)
#### Organizing the data as panel
data("nls_panel")
View(nls_panel) # long-form
#
nlspd <- plm::pdata.frame(nls_panel, 
                          index = c("id", "year")) # two indexes
car::some(nlspd)
#
smpl <- nlspd[nlspd$id %in% c(1,2), c(1:6, 14:15)]
#
tbl <- xtable(smpl)
kable(tbl, digits = 4, align = "c",
      caption = "A data sample")
#
pdim(nlspd)
#### The pooled model
wage.pooled <- plm(lwage ~ educ + exper + I(exper^2) + 
                     tenure + I(tenure^2) + black + south + union,
                   model = "pooling", data = nlspd)
#
kable(tidy(wage.pooled), digits = 3,
      caption = "Pooled model")
#
tbl <- tidy(coeftest(wage.pooled, vcov = vcovHC(wage.pooled,
                                                type = "HC0", 
                                                cluster = "group")))
kable(tbl, digits = 5, 
      caption = "Pooled 'wage' model with ckyster robust standar errors")
#### The fixed effects model
nls10 <- plm::pdata.frame(nls_panel[nls_panel$id %in% 1:10,])
some(nls10)
#
wage.fixed <- lm(lwage ~ exper + I(exper^2) + tenure + I(tenure^2) + 
                   union + factor(id) -1,
                 data = nls10) # at this model, other information as 
# south, black and so on as intercept, and then remove it
# factor() generates dummy variables for all categories of the 
# variable, taking the first category as reference
# 
kable(tidy(wage.fixed), digits = 3,
      caption = "Fixed effects in a subsample")
names(nls10)
##
wage.within <- plm::plm(lwage ~ exper + I(exper^2) + tenure + I(tenure) +
                          south + union, 
                        data = nlspd,
                        model = "within")
kable(tidy(wage.within), digits = 5,
      caption = "Fixed effects using 'within' with full sample")
##
wage.10.within <- plm::plm(lwage ~ exper + I(exper^2) + tenure + 
                             I(tenure^2) + south + union, 
                           data = nls10,
                           model = "within")
kable(tidy(wage.10.within), digits = 5,
      cpation = "Fixed effects using 'within' model option for n = 10")
## test the necessity of the fixed effects
kable(tidy(plm::pFtest(wage.within, wage.pooled)),
      caption = "Fixed effects test: H0: 'No fixed effects'")
# reject H0
# including individual heterogeneity will significantly lower the 
# marginal effects of the variables.
#### Random effects model
wageReTest <- plm::plmtest(wage.pooled, effect = "individual")
kable(tidy(wageReTest), 
      caption = "A random effects test for the wage equation")
# reject H0, heterogeneity among individuals may be significant
# Hausman test for endogeneity
wage.random <- plm::plm(lwage ~ educ + exper + I(exper^2) + 
                          tenure + I(tenure^2) + south + union, 
                        data = nlspd,
                        random.method = "swar",
                        model = "random")
kable(tidy(wage.random), digits = 4, 
      caption = "The random effects results for the wage equation")
#
kable(tidy(plm::phtest(wage.within, wage.random)), 
      caption = "Hausman endogeneity test for the random effects wage model")
# model is consistent, that means individual random effects are
# exogeneous;
# if model is not consistent, then fixed effects model
# is correct solution, 
## Hausman-taylor estimator
wage.HT <- plm(lwage ~ educ + exper + I(exper^2) +
                      tenure + I(tenure^2) + black + 
                      south + union | exper + I(exper^2) + 
                      tenure + I(tenure^2) + union + black,
                    data = nlspd,
                    model = "ht")
kable(tidy(wage.HT), digits = 5, 
      caption = "Hausman-Taylor estimates for the wage equation")
#### Grunfeld's investment example
data("grunfeld2")
View(grunfeld2)
#
grun <- pdata.frame(grunfeld2, index = c("firm", "year"))
kable(some(grun), align = "c", 
      caption = "The head of the grunfeld2 dataset organized as a panel")
## pooling model
grun.pool <- plm::plm(inv ~ v + k, 
                      model = "pooling", data = grun)
kable(tidy(grun.pool), digits = 5,
      caption = "pooling model results for grunfeld dataset")
#
SSE.pool <- sum(resid(grun.pool)^2)
sigma2.pool <- SSE.pool / (grun.pool$df.residual) # variance
# 
SSE.pool; sigma2.pool
## fixed model, allow individual heterogeneity, but has the same error
# structure
grun.fe <- plm::plm(inv ~ v*grun$firm + k*grun$firm, 
                    model = "pooling", data = grun)
#
kable(tidy(grun.fe), digits = 5,
      caption = "fixed effects model results for grunfeld dataset")
#
SSE.fe <- sum(resid(grun.fe)^2)
sigma2.fe <- SSE.fe/grun.fe$df.residual
#
SSE.fe; sigma2.fe
## comparison between fixed effects model and pooling model
grun.pvcm <- plm::pvcm(inv ~ v + k,
                       model = "within", 
                       data = grun) 
#
coef(grun.pvcm) # pvcm function is not equal to the fixed effects model
# actually it is useful for testing the poolibility of a dataset
# test
pooltest(grun.pool, grun.pvcm) # test requires fixed effects model 
# using pvcm function
# fail to reject H0:individual dummy terms are 0
## random effects model: different coefs and different error variances
grun1.pool <- plm::plm(inv ~ v + k, model = "pooling",
                       subset = grun$firm == 1, 
                       data = grun)
grun2.pool <- plm::plm(inv ~ v + k, model = "pooling",
                       subset = grun$firm == 2,
                       data = grun)
#
SSE.pool1 <- sum(resid(grun1.pool)^2)
SSE.pool2 <- sum(resid(grun2.pool)^2)
#
sigma2.pool1 <- SSE.pool1 / grun1.pool$df.residual
sigma2.pool2 <- SSE.pool2 / grun2.pool$df.residual
#
SSE.pool1; sigma2.pool1
SSE.pool2; sigma2.pool2
#
kable(tidy(grun1.pool), digits = 5, align = "c",
      caption = "Pooling astimates for the GE firm (firm = 1)")
kable(tidy(grun2.pool), digits = 5, align = "c",
      caption = "Pooling astimates for the GE firm (firm = 2)")
## test errors between two firms
lmtest::gqtest(grun.pool, point = 0.5, alternative = "two.sided",
               order.by = grun$firm)
# variances are not equal
# test whether two firms have significant differences on the variances
## generalized least squares method, for seemingly unrelated regressions
# the only link between two firms is correlation of their 
# contemporaneous error terms
grunf <- grunfeld2
grunf$Firm <- "WE"
for (i in 1:40){
  if(grunf$firm[i] ==1) {grunf$Firm[i] <- "GE"}
}
#
grunf$firm <- NULL # delete the column
#
names(grunf) <- c("inv", "val", "cap", "year", "firm")
#
some(grunf)
#
grunfpd <- plm::plm.data(grunf, c("firm", "year"))
# SUR: seemingly unrelated regression
grunf.SUR <- systemfit(inv ~ val + cap, method = "SUR", 
                       data = grunfpd) # systemfit function requires 
# the dataset created by the plm.data, rather than pdata.frame
# in order to make the function work, we need to change the names more
# than one letter
summary(grunf.SUR, resdCov = FALSE, # hide the covariance matrix
        equations = FALSE)
####












