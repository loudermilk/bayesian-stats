## sr-ch6.R
## 
library(rethinking)


sppnames <- c("afarensis", "africanus", "habilis", "boisei", "rudlfensis", "ergaster", "sapiens")
brainvolcc <- c(438, 452, 612, 521, 752, 871, 1350)
masskg <- c(37, 35.5, 34.5, 41.5, 55.5, 61, 53.5)
d <- data.frame(species = sppnames, brain = brainvolcc, mass = masskg)
d

m6.1 <- lm(brain ~ mass, data = d)

1 - var(resid(m6.1))/var(d$brain)
summary(m6.1)

m6.2 <- lm(brain ~ mass + I(mass^2), data = d)
m6.3 <- lm(brain ~ mass + I(mass^2) + I(mass^3), data = d)
m6.4 <- lm(brain ~ mass + I(mass^2) + I(mass^3) + I(mass^4), data = d)
m6.5 <- lm(brain ~ mass + I(mass^2) + I(mass^3) + I(mass^4) + I(mass^5), data = d)
m6.6 <- lm(brain ~ mass + I(mass^2) + I(mass^3) + I(mass^4) + I(mass^5) + I(mass^6), data = d)
summary(m6.1)

## model fitting is a form of data compression> parameters summarize relationship
## among the data compressing data into a simpler form although with loss of
## information about the sample.

## 6.1.2 Too few parameters hurts too
## underfitting produces models that are inaccurate both withon and out of sample
## underfit models are insensitive to the sample

## 6.2 Information theory and model performance
## out-of-sample deviance
## (1) establish that join probability is the right way to judge accuracy
## (2) establish a measurement scale for distamce fro perfect accuracy
## (3) establish deviance as an approximation of relative distance from perfect accuracy.
## (4) establish that it is only deviance out-of-sample that is of interest


## 6.2.1 Firing the weatherperson
## cost-benefit analysis - how much does it cost when we are wrong? How much do
## we win when we are right.
## accuracy in context - ignoring cost/benefit we need a way to judge accuracy
## that account for how much a model could possibly improve prediction.

## 6.2.1.1 Costs and benefits
## 6.2.1.2 Measurng accuracy
## 6.2.2 Information and uncertainty

## How much is our uncertainty reduced by learning an outcome?
## Information - the reduction in uncertainty derived from learning an outcome.
## (1) the measure of uncertainty should be continuous
## (2) the measure of uncertainty should increase as the number of possible events
## increases.
## (3) the measure of uncertainty should be additive

## INFORMATION ENTROPY - the uncertainty contained in a probability distribution
## is the average log-probability of an event.

## to compute information entropy for weather example. Suppose true probabilities
## of rain and shine are p1 = 0.3 and p2 = 0.7

p <- c(0.3, 0.7)
-sum(p*log(p))

## in Abu Dhabi p1 = 0.01, p2 = 0.99
p <- c(0.01, 0.99)
-sum(p*log(p))

p <- c(0.5, 0.5)
-sum(p*log(p))


## rain, sun, snow
p <- c(0.7, 0.15, 0.15)
-sum(p*log(p))

## 6.2.3 From entropy to accuracy
## DIVERGENCE - the additional uncertainty induced by using probabilities from
## one distribution to describe another distribution

m6.1 <- lm(brain ~ mass, data = d)
# compute deviance by cheating
(-2)*logLik(m6.1)

## 6.2.5 From deviance to out-of-sample
## Deviance is a principled way to measure distance from the target. Deviance
## has the same problem at R^2... it always improves as the model gets more complex.

## (a) given tarining sample of size N
## (b) fit model to training sample and compute deviance on the training sample (D_train)
## (c)Suppose another sample of size N from the same process. This is the test sample.
## (d) compute the deviance on the test sample. Use the MAP estimayes from (b) to compute
## the devaince for the data in the test sample called D_test.

## 6.3 Regularization




