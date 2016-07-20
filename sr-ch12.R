## sr-ch12.R
## Chapter 12 - Multilevel Models
## Remember features of each clusteer in the data as they learn about all
## the clusters.

## (1) Improved estimates for repeat sampling - when more than one observation
## arises from the same indiv, loc, or time, then traditional single-level
## models either maximally underfit or underfit the data.
## (2) Improved estimates for imbalance in sampling - when some indiv, loc, or
## time are sampled more than others, multilevel models cope with differing
## uncertainty across these clusters. This prevents over-sampled clusters from
## unfairly dominating inference.
## (3) Estimates of variation. If RQ include variation among indiv or other grps
## in data , then multilevel models help bc the model variation explicitly.
## (4) Avoid averaging, retain variation - pre-averaging data to construct
## variables can be dangerous bc it removes variation.

## 12.1 Multilevel tadpoles
library(rethinking)
data(reedfrogs)
d <- reedfrogs
str(d)
head(d)

## VARYING INTERCEPTS MODEL -  multilevel model in which we simultaneously estimate both an intercept for
## each tank and the variation among tanks
## learn the prior that is common to all the modeled intercepts

## make the tank cluster variable
d$tank <- 1:nrow(d)
## fit
m12.1 <- map(
  alist(
    surv ~ dbinom(density, p),
    logit(p) <- a_tank[tank],
    a_tank[tank] ~ dnorm(0,5)
  ), data = d
)
precis(m12.1, depth = 2)

## HYPERPARAMETERS - parameters for parameters

m12.2 <- map2stan(
  alist(
    surv ~ dbinom(density, p),
    logit(p) <- a_tank[tank],
    a_tank[tank] ~ dnorm(a, sigma),
    a ~ dnorm(0,1),
    sigma ~ dcauchy(0,1)
  ), data = d, iter = 4000, chains = 4
)
  
compare(m12.1, m12.2)
  
## POOLING - each tank provides information that can be used to improve the estimates
## for all of the other tanks

post <- extract.samples(m12.2)
plot(NULL, xlim=c(-3,4), ylim=c(0,0.35), 
     xlab="log-odds survive", ylab="density")
for (i in 1:100) {
  curve(dnorm(x, post$a[i], post$sigma[i]), add = T, col=col.alpha("black", 0.2))
}

# sample imaginary tank from post dist
sim_tanks <- rnorm(8000, post$a, post$sigma)
dens(logistic(sim_tanks), xlab = "probability survive")

## 12.2 Varying effects and the underfitting/overfitting trade-off
## Varying intercepts are just regularized estimates but adaptively regularized by
## estimating how diverse the clusters are while estimating the features of each
## cluster

## (1) complete pooling - assume that the population of the ponds is invariant,
## the same as estimating a common intercept for all ponds.
## (2) no pooling - assume each pond tells us nothing about any other pond
## (3) partial pooling - using an adaptive regularizing prior

## 12.2.1 The model
## multilevel binomial model with ponds instead of tanks

## 12.2.2 Assign values to the parameters
a <- 1.4
sigma <- 1.5
nponds <- 60
ni <- as.integer(rep(c(5,10,25,35),each=15))

a_pond <- rnorm(nponds, mean=a, sd=sigma)
dsim <- data.frame(pond=1:nponds, ni=ni, true_a=a_pond)
head(dsim)
tail(dsim)

## simulate survival process
dsim$si <- rbinom(nponds,prob=logistic(dsim$true_a), size = dsim$ni)

## 12.2.4 Compute the no-pooling estimates
dsim$p_nopool <- dsim$si/dsim$ni

## 12.2.5 Compute the partial-pooling estimates
m12.3 <- map2stan(
  alist(
    si ~ dbinom(ni, p),
    logit(p) <- a_pond[pond],
    a_pond[pond] ~ dnorm(a, sigma),
    a ~ dnorm(0,1),
    sigma ~ dcauchy(0,1)
  ), data = dsim, iter=1e4, warmup = 1000
)
precis(m12.3, depth = 2)
## 60 estimated intercept parameters

estimated.a_pond <- as.numeric(coef(m12.3)[1:60])
dsim$p_partpool <- logistic(estimated.a_pond)
dsim$p_true <- logistic(dsim$true_a)

nopool_error <- abs(dsim$p_nopool - dsim$p_true)
partpool_error <- abs(dsim$p_partpool - dsim$p_true)

plot(1:60, nopool_error, xlab="pond", ylab="abs error", col=rangi2, pch=16)
points(1:60, partpool_error)



