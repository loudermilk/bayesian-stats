## sr-ch5.R
## Chapter 5 - Multivariate Linear Models

library(rethinking)
data(WaffleDivorce)
d <- WaffleDivorce
head(d)
names(d)

# standardize
d$MedianAgeMarriage.s <- (d$MedianAgeMarriage - mean(d$MedianAgeMarriage))/sd(d$MedianAgeMarriage)

# fit
m5.1 <- map(alist(
  Divorce ~ dnorm(mu, sigma),
  mu <- a + bA * MedianAgeMarriage.s,
  a ~ dnorm(10,10),
  bA ~ dnorm(0, 1),
  sigma ~ dunif(0, 10)
), data = d)

MAM.seq <- seq(from = -3, to = 3.5, length.out = 30)
mu <- link(m5.1, data=data.frame(MedianAgeMarriage.s = MAM.seq))
mu.PI <- apply(mu, 2, PI)
plot(Divorce~MedianAgeMarriage.s, data = d, col = rangi2)
abline(m5.1)
shade(mu.PI, MAM.seq)

precis(m5.1)

View(head(d))
d$Marriage.s <- (d$Marriage - mean(d$Marriage))/sd(d$Marriage)
m5.2 <- map(
  alist(
    Divorce ~ dnorm(mu, sigma),
    mu <- a + bR * Marriage.s,
    a ~ dnorm(10, 10),
    bR ~ dnorm(0,1),
    sigma ~ dunif(0,10)
  ),
  data = d
)

## What is the predictive value of a variable once I already know all the
## other predictor variables?

## (1) after I already know marriage rate, what additional value is there in
##     also knowing age at marriage?
## (2) after I already know age at marriage, what additional value is there in
##     also knowing marriage rate?

## 5.1.1 Multivariate notation
## strategy:
## (1) nominate the predcitor variables you want in the linear model of the mean.
## (2) for each predictor make a parameter that will measure its association
## with the outcome.
## (3) multiply the parameter by the variable and add that term to the linear model.


## 5.1.2 Fitting the model
##
## D_i ~ Normal(mu_i, sigma)            ## Divorce ~ dnorm(mu, sigma)
## mu_i = a + b_R * R_i + b_A * A_i     ## mu <- a + bR*Marriage.s + bA*MedianAgeMarriage.s
## a ~ Normal(10,10)                    ## a ~ dnorm(10,10)
## b_R ~ Normal(0,1)                    ## bR ~ dnorm(0,1)
## b_A ~ Normal(0,1)                    ## bA ~ dnorm(0,1)
## sigma ~ Uniform(0,10)                ## sigma ~ dunif(0,10)

m5.3 <- map(
  alist(
    Divorce ~ dnorm(mu, sigma),
    mu <- a + bR*Marriage.s + bA*MedianAgeMarriage.s,
    a ~ dnorm(10,10),
    bR ~ dnorm(0,1),
    bA ~ dnorm(0,1),
    sigma ~ dunif(0,10)
  ),
  data = d
)
precis(m5.3)

## once we know median age at marriage for a state there is no additional
## predictive power in also knowing marriate rate for that state.

## 5.1.3 Plotting multivariate posteriors
## (1) predictor residual plots - show outcome against residual predictor values
## (2) counterfactual plots - show implied predictions for immaginary experiments
## in which the different predictor variables can be changed independently.
## (3) posterior prediction plots - show model-based predictions against raw
## data or otherwise display the error in the prediction.

## 5.1.3.1 Predictor residual plots
## A predictor variable residual is the average prediction error when we use all 
## the other predictor variables to model a predictor of interest.

## in our multivariate model of divorce rate we have two predictors:
## Marriage.s and MedianAgeMarriage.s. To compute predictor residuals for either
## we just use the other predictor to model it.

## R_i ~ Normal(mu_i, sigma)
## mu_i = a +b*A_i
## a ~ Normal(0,10)
## b ~ Normal(0,1)
## sigma ~ Uniform(0,10)

m5.4 <- map(
  alist(
    Marriage.s ~ dnorm(mu, sigma),
    mu <- a + b*MedianAgeMarriage.s,
    a ~ dnorm(0,10),
    b ~ dnorm(0,1),
    sigma ~ dunif(0,10)
  ), data = d
)

## compute the residuals by subtracting the observed marriage rate in each
## state from the predicted rate based on age of marriage

## compute expected value at MAP for each state:
mu <- coef(m5.4)['a'] + coef(m5.4)['b']*d$MedianAgeMarriage.s
## compute residuals for each state
m.resid <- d$Marriage.s - mu

plot(Marriage.s ~ MedianAgeMarriage.s, d, col = rangi2)
abline(m5.4)
for (i in 1:length(m.resid)) {
  x <- d$MedianAgeMarriage.s[i]
  y <- d$Marriage.s[i]
  lines(c(x,x),c(mu[i],y),lwd = 0.5, col=col.alpha("black",0.7))
}

## 5.1.3.2 Counterfactual plots
## simplest use is to see how the predictions change as you change only
## one predictor at a time. Will help you understand the implications of
## your model.

## prepare new counterfactual data
A.avg <- mean(d$MedianAgeMarriage.s)
R.seq <- seq(from = -3, to = 3, length.out = 30)
pred.data <- data.frame(
  Marriage.s = R.seq,
  MedianAgeMarriate.s = A.avg
)

# compute counterfacual mean divorce
## DOESN'T WORK


## 5.1.3.3 Posterior prediction plots
## check the model fit against the observed data
## (1) did the model fit correctly?
## (2) how does the model fail?









