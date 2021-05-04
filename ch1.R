###### chapter 1 introduction
require("bookdown")
require("PoEdata")
require("knitr")
require("xtable")
require("printr")
require("stargazer")
require("rmarkdown")
require("car")
####
data(andy)
?andy
#
View(andy)
head(andy)
car::some(andy) # randomly show a few rows in dataset, by default 
# is 10
car::some(andy[1:20,], n = 5)
####
plot(price ~ advert, data = andy)
#
curve(x ^ 2, from = 0, to = 20,
      xlab = 'X', ylab = "Y")
curve(x ^ 3, from = 0, to = 20, add = TRUE)
curve(x ^ 4, from = 0, to = 20, add = TRUE)
#
abline(a = 300, b = -1, h = 200, v = 10) # will graph 3 lines
####
fit <- lm(sales ~ price + advert, data = andy)
nobs(fit) # shows the number of obs used in the model
nrow(andy) # show the number of obs
####








