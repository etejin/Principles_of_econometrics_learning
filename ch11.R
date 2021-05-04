###### chapter 11 Simultanous equation tools
rm(list = ls())
#
pkgs <- c("systemfit", "broom", "PoEdata", "knitr")
sapply(pkgs, require, character.only = TRUE)
####
data("truffles")
View(truffles)
##
D <- q ~ p+ps+di # demand
S <- q ~ p+pf # supply 
##
sys <- list(D,S)
instr <- ~ ps+di+pf # instruments
truff.sys <- systemfit(sys, 
                       inst = instr,
                       method = "2SLS",
                       data = truffles)
summary(truff.sys)
##
Q.red <- lm(q ~ ps+di+pf, truffles)
P.red <- lm(p ~ ps+di+pf, truffles)
kable(tidy(Q.red), digits = 4,
      caption = "Reduced form for quantity")
#
kable(tidy(P.red), digits = 4,
      caption = "Reduced form for price")
#### Another example
data("fultonfish")
View(fultonfish)
##
fishQ.ols <- lm(lquan ~ mon + tue + wed + thu + stormy, data = fultonfish)
fishP.ols <- lm(lprice ~ mon + tue + wed + thu + stormy, data = fultonfish)
#
kable(tidy(fishQ.ols), digits = 4,
      caption = "Reduced 'Q' equation for the fultonfish example") 
# stormy is significant, which means the 2SLS estimation of the demand equation is 
# reliable
kable(tidy(fishP.ols), digits = 4,
      caption = "Reduced 'P' equation for the fultonfish example")
# weekdays indicator are not reliable, which means the 2SLS estimation of the supply 
# equation is not reliable
##
fish.D <- lquan ~ lprice + mon + tue + wed + thu
fish.S <- lquan ~ lprice + stormy
sys <- list(fish.D, fish.S)
iv <- ~ mon + tue + wed + thu + stormy
#
fish.sys <- systemfit::systemfit(sys, method = "2SLS",
                                 inst = iv, data = fultonfish)
summary(fish.sys)
# solution is to find out the significant instruments
#### 



