###### chapter 16 qualitative and LDV models
rm(list = ls())
pkgs <- c("nlWaldTest", "lmtest", "broom", "PoEdata", "car", "sandwich",
          "knitr", "forecast", "AER", "xtable")
sapply(pkgs, require, character.only = TRUE)
#### The linear probability model
x <- seq(-3, 3, 0.2)
plot(x, pnorm(x), type = "l", xlab = "b1+b2*x", ylab = "P[y = 1]")
plot(x, dnorm(x), type = "l")
#### The transportation example
data("transport")
View(transport)
##
auto.probit <- glm(auto ~ dtime, family = binomial(link = "probit"),
                   data = transport)
kable(tidy(auto.probit), dnigits = 4, align = "c",
      caption = "Transport example, estimated by probit")
## partial effects
xdtime <- data.frame(dtime = 2)
preLinear <- predict(auto.probit, xdtime,
                     data = transport, type = "link")
#
DpDdtime <- coef(auto.probit)[[2]]*dnorm(preLinear)
DpDdtime
## predict the probability of choosing auto
preProbit <- predict(auto.probit, xdtime,
                     data = transport, type = "response")
preProbit
# The argument type. type = "link" returns ^η (the log-odds in the 
# logistic model), 
# type = "response" returns  g−1(^η) (the probabilities in the logistic
# model). 
## marginal effects at average predicted value
avgPredlinear <- predict(auto.probit, type = "link")
AME <- mean(dnorm(avgPredlinear)) * coef(auto.probit)
AME
#### The logit model for binary choice
data("coke")
#
coke.logit <- glm(coke ~ pratio + disp_coke +disp_pepsi,
                  data = coke, family = binomial(link = "logit"))
kable(tidy(coke.logit), digits = 5, align = "c",
      caption = "Logit estimates for the 'coke' dataset")
#
coke.LPM <- lm(coke ~ pratio + disp_coke + disp_pepsi, data = coke)
coke.probit <- glm(coke ~ pratio + disp_coke + disp_pepsi,
                   data = coke, family = binomial(link = "probit"))
#
stargazer::stargazer(coke.LPM, coke.probit, coke.logit,
                     header = FALSE,
                     title = "Three binary choice models for the 'coke' dataset",
                     type = "text",
                     keep.stat = "n", digits = 4, single.row = FALSE,
                     intercept.bottom = FALSE,
                     model.names = FALSE,
                     column.labels = c("LPM", "probit", "logit"))
## marginal effects
newdata <- data.frame(disp_coke = sample(coke$disp_coke,5),
                      disp_pepsi = sample(coke$disp_pepsi, 5),
                      pratio = sample(coke$pratio, 5))
PredLinear <- predict(coke.logit, newdata, type = "link")
#
avgPredLinear <- mean(PredLinear)
lamda <- exp(avgPredLinear) / (1 + exp(avgPredLinear))
#
AME <- (lamda*(1-lamda)) * coef(coke.logit)
# 
AME
## prediction results
tble <- data.frame(table(true = coke$coke,
                         predicted = round(fitted(coke.logit))))
kable(tble, align = "c", caption = "Logit prediction results")
## linearHypothesis
Hnull <- "disp_coke + disp_pepsi = 0"
car::linearHypothesis(coke.logit, Hnull)
#
Hnull <- c("disp_coke = 0", "disp_pepsi =0")
linearHypothesis(coke.logit, Hnull)
#### Multinomial logit
data("nels_small")
View(nels_small)
#
nels.multinom <- nnet::multinom(psechoice ~ grades, data = nels_small)
summary(nels.multinom)
##
medGrades <- median(nels_small$grades)
fifthPercentileGrades <- quantile(nels_small$grades, 0.05)
#
newdata <- data.frame(grades = c(medGrades, fifthPercentileGrades))
rownames(newdata) <- c("mean", "5%")
colnames(newdata) <- c("grades")
#
pred <- predict(nels.multinom, newdata)
#
predProb <- predict(nels.multinom, newdata, "prob")
#
pred; predProb
#### The Conditional logit model
data("cola")
View(cola)
#
psych::describeBy(cola)
#
N <- nrow(cola)
N3 <- N/3
#
price1 <- cola$price[seq(1, N, by = 3)]
price2 <- cola$price[seq(2, N, by = 3)]  
price3 <- cola$price[seq(3, N, by = 3)]
#
bchoice <- rep("1", N3)
for (j in 1:N3){
  if(cola$choice[3*j-1]==1) bchoice[j] <- "2"
  if(cola$choice[3*j]==1) bchoice[j] <- "3"
}
#
cola.clogit <- MCMCmnl(bchoice ~ choicevar(price1, "b2", "1") +
                         choicevar(price2, "b2", "2") + 
                         choicevar(price3, "b2", "3"),
                       baseline = "3", mcmc.method = "IndMH")
sclogit <- summary(cola.clogit)
tabMCMC <- as.data.frame(sclogit$statistics)[,1:2]
row.names(tabMCMC) <- c("b2", "b11", "b12")
kable(tabMCMC, digits = 4, align = "c",
      caption = "conditional logit estimates for the 'cola' problem")
##
pPepsi <- 1
pSevenup <- 1.25
pCoke <- 1.10
#
b13 <- 0
b2 <- tabMCMC$Mean[1]
b11 <- tabMCMC$Mean[2]
b12 <- tabMCMC$Mean[3]
#
PiPepsi <- exp(b11+b2*pPepsi) / (exp(b11+b2*pPepsi)+
                                    exp(b12+b2*pSevenup)+
                                    exp(b13+b2*pCoke))
#
PiSevenup <- exp(b12+b2*pSevenup) / (exp(b11+b2*pPepsi)+
                                       exp(b12+b2*pSevenup)+
                                       exp(b13+b2*pCoke))
#
PiCoke <- 1-PiPepsi - PiSevenup
#
PiPepsi; PiSevenup; PiCoke
##
# the three probabilities are different for different individuals because
# different individuals faces different price
#### Ordered choice model
nels.oprobit <- MCMCoprobit(psechoice ~ grades, data = nels_small,
                            mcmc = 10000)
sOprobit <- summary(nels.oprobit)
tabOprobit <- sOprobit$statistics[,1:2]
#
kable(tabOprobit, digits = 4, align = "c",
      caption = "Ordered probit estimates for the 'nels' problem")
## probabilities for each choice
mu1 <- -tabOprobit[1]
b <- tabOprobit[2]
mu2 <- tabOprobit[3] - tabOprobit[1]
#
xGrade <- c(mean(nels_small$grades), 
            quantile(nels_small$grades, 0.05))
# probabilities
prob1 <- pnorm(mu1-b*xGrade)
prob2 <- pnorm(mu2-b*xGrade) - pnorm(mu1-b*xGrade)
prob3 <- 1-pnorm(mu2-b*xGrade)
#
prob1; prob2; prob3
# marginal effects
Dp1DGrades <- -pnorm(mu1-b*xGrade)*b
Dp2DGrades <- (pnorm(mu1-b*xGrade)-pnorm(mu2-b*xGrade))*b
Dp3DGrades <- pnorm(mu2-b*xGrade)*b
#
Dp1DGrades; Dp2DGrades; Dp3DGrades
#### Models for count data
data("olympics")
View(olympics)
#
olympics.count <- glm(medaltot ~ log(pop) + log(gdp),
                      family = "poisson",
                      na.action = na.omit,
                      data = olympics)
#
kable(tidy(olympics.count), digits = 4, align = "c",
      caption = "poisson model for the 'olympics' problem")
#
AER::dispersiontest(olympics.count) # test the validity of the Possion
# distrobution based on this distribution's characteristic that its mean
# is equal to its variance
#### The tobit, or censored data model
data("mroz")
View(mroz)
#
hist(mroz$hours, breaks = 10, col = 3)
#
mroz.tobit <- AER::tobit(hours ~ educ + exper + age + kidsl6, data = mroz)
smroz <- summary(mroz.tobit)
smroz
# marginal effects
xEduc <- 12.29
xExper <- 10.63
xAge <- 42.54
xKids <- 1
#
bInt <- coef(mroz.tobit)[[1]]
bEduc <- coef(mroz.tobit)[[2]]
bExper <- coef(mroz.tobit)[[3]]
bAge <- coef(mroz.tobit)[[4]]
bKids <- coef(mroz.tobit)[[5]]
#
bSigma <- mroz.tobit$scale
#
Phactor <- pnorm((bInt + bEduc*xEduc + bExper*xExper + bAge*xAge +
                    bKids*xKids)/bSigma)
DhoursDeduc <- Phactor*bEduc
#
DhoursDeduc
# margEff()
#### The heckit, or sample selection model
wage.heckit <- sampleSelection::selection(lfp ~ age + educ + 
                                            I(kids618 + kidsl6) + mtr,
                                          log(wage) ~ educ + exper,
                                          data = mroz, method = "ml")
summary(wage.heckit)
####




