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

## 12.3 More than one type of cluster
library(rethinking)
data(chimpanzees)
d <- chimpanzees
str(d)
head(d)
d$recipient <- NULL #get rid of NAs

m12.4 <- map2stan(
  alist(
    pulled_left ~ dbinom(1,p),
    logit(p) <- a + a_actor[actor] + (bp + bpC*condition)*prosoc_left,
    a_actor[actor] ~ dnorm(0,sigma_actor),
    a ~ dnorm(0,10),
    bp ~ dnorm(0,10), 
    bpC ~ dnorm(0,10),
    sigma_actor ~ dcauchy(0,1)
  ), data = d, chains = 2, cores = 3, iter = 5000, warmup = 1000
)
plot(m12.4)
precis(m12.4)

post <- extract.samples(m12.4)
total_a_actor <- sapply(1:7, function(actor) post$a + post$a_actor[,actor])
round(apply(total_a_actor,2,mean),2)

## 12.3.2 Two types of cluster (add block)
## fit model that uses both actor and block
d$block_id < d$block # name `block` is reserved by stan
m12.5 <- map2stan(
  alist(
    pulled_left ~ dbinom(1, p),
    logit(p) <- a + a_actor[actor] + a_block[block_id] + (bp + bpc*condition)*prosoc_left,
    a_actor[actor] ~ dnorm(0, sigma_actor),
    a_block[block_id] ~ dnorm(0, sigma_block),
    c(a,bp, bpc) ~ dnorm(0,10),
    sigma_actor ~ dcauchy(0,1),
    sigma_block ~ dcauchy(0,1)
  ), data = d, warmup = 1000, iter = 6000, chains = 1, cores = 1
)

## 12.4 Multilevel posterior predictions
## MODEL CHECKING - compare the sample to the posterior predictions of the fit model
## producing implied predictions from a fit model is helpful for understanding
## what the model means. INFORMATION CRITERIA (like DIC & WAIC) provide simple estimates
## of out-of-sample model accuracy, like the KL divergence. IC provide rough measure of
## a model's flexibility and therefore overfitting risk.

## in chimpanzees there are 7 unique actors - these are clusters.
precis(m12.4, depth=2)
## the whole pt of partial pooling is to shrink estimates towards the grand mean

chimp <- 2
d.pred <- list(
  prosoc_left = c(0,1,0,1),
  condition = c(0,0,1,1),
  actor = rep(chimp, 4)
)
d.pred
link.m12.4 <- link(m12.4, data = d.pred)
pred.p <- apply(link.m12.4, 2, mean)
pred.p.PI <- apply(link.m12.4, 2, PI)

par(mfrow=c(1,1))

plot(0,0,type='n', xlab="prosoc_left/condition", ylab="proportion pulled left", ylim=c(0,1),xaxt="n", xlim=c(1,4))
axis(1,at=1:4, labels = c("0/0","1/0", "0/1","1/1"))

p <- by(d$pulled_left, list(d$prosoc_left, d$condition, d$actor), mean)
for (chimp in 1:7) {
  lines(1:4, as.vector(p[,,chimp]), col=rangi2, lwd = 1.5)
}
lines(1:4, pred.p)
shade(pred.p.PI, 1:4)
post <- extract.samples(m12.4)
str(post)
dens(post$a_actor[,5])

p.link <- function(prosoc_left, condition, actor) {
  logodds <- with(post, 
                  a + a_actor[,actor] + (bp +bpC * condition)*prosoc_left
                  )
  return(logistic(logodds))
}

## compute predictions
prosoc_left <- c(0,1,0,1)
condition <- c(0,0,1,1)
pred.raw <- sapply(1:4, function(i) p.link(prosoc_left[i], condition[i], 2))
pred.p <- apply(pred.raw, 2, mean)
pred.p.PI <- apply(pred.raw, 2, PI)

## 12.4.2 Posterior prediction for new clusters
## often the particular clusters in the sample are not of any enduring interest - in the
## chimpanzee data, for examoke, we'd like to make inferences about the population, so
## the actor intercepts are not of interest.

## imagine leaving out one of the clusters when you fit the data. Use the a and sigma_actor
## parameters because they descrive the population of actors.
## how to construct posterior predictions for a now, previously unobserved average actor.
## by average, I mean a chimp with an intercept exactly at the mean a.

library(rethinking)
data(chimpanzees)
d <- chimpanzees
str(d)
head(d)
d$recipient <- NULL #get rid of NAs

m12.4 <- map2stan(
  alist(
    pulled_left ~ dbinom(1,p),
    logit(p) <- a + a_actor[actor] + (bp + bpC*condition)*prosoc_left,
    a_actor[actor] ~ dnorm(0,sigma_actor),
    a ~ dnorm(0,10),
    bp ~ dnorm(0,10), 
    bpC ~ dnorm(0,10),
    sigma_actor ~ dcauchy(0,1)
  ), data = d, chains = 2, cores = 3, iter = 5000, warmup = 1000
)
plot(m12.4)
precis(m12.4)


d.pred <- list(
  prosoc_left = c(0,1,0,1),
  condition = c(0,0,1,1),
  actor = rep(2,4)
)

## replace varying interceot samples w zeros
## 1000 samples by 7 actors
a_actor_zeros <- matrix(0,1000,7)

## fire up link
link.m12.4 <- link(m12.4, n = 1000, data=d.pred, replace = list(a_actor=a_actor_zeros))

## summarize & plot
pred.p.mean <- apply(link.m12.4, 2, mean)
pred.p.PI <- apply(link.m12.4, 2, PI, prob=0.8)
par(mfrow=c(1,1))
plot(0,0,type="n",xlab="prosoc_left/condition", 
     ylab="proportion pulled left", ylim=c(0,1),xaxt="n",xlim=c(1,4))
axis(1, at = 1:4, labels=c("0/0","1/0", "0/1", "1/1"))
lines(1:4, pred.p.mean)
shade(pred.p.PI, 1:4)

## to show variation among actors use sigma_alpha in calculation
## replace varying intercept samples with simulations
post <- extract.samples(m12.4)
a_actor_sims <- rnorm(7000, 0, post$sigma_actor)
a_actor_sims <- matrix(a_actor_sims, 1000, 7)

## pass simulated intercepts into link
link.m12.4 <- link(m12.4, n = 1000, data = d.pred, replace = list(a_actor=a_actor_sims))

## summarize & plot
pred.p.mean <- apply(link.m12.4, 2, mean)
pred.p.PI <- apply(link.m12.4, 2, PI, prob=0.8)
par(mfrow=c(1,1))
plot(0,0,type="n",xlab="prosoc_left/condition", 
     ylab="proportion pulled left", ylim=c(0,1),xaxt="n",xlim=c(1,4))
axis(1, at = 1:4, labels=c("0/0","1/0", "0/1", "1/1"))
lines(1:4, pred.p.mean)
shade(pred.p.PI, 1:4)

## simulate a new actor from the estimated population of actors and then
## computes probabilities of pulling the left lever for each of the 4 treatments

post <- extract.samples(m12.4)
sim.actor <- function(i) {
  sim_a_actor <- rnorm(1,0,post$sigma_actor[i])
  P <- c(0,1,0,1)
  C <- c(0,0,1,1)
  p <- logistic(
    post$a[i] +
      sim_a_actor +
      (post$bp[i] + post$bpC[i]*C)*P
  )
  return(p)
}

plot(0,0,type="n",xlab="prosoc_left/condition", 
     ylab="proportion pulled left", ylim=c(0,1),xaxt="n",xlim=c(1,4))
axis(1, at = 1:4, labels=c("0/0","1/0", "0/1", "1/1"))
# plot 50 simulated actors
for (i in 1:50) lines(1:4,sim.actor(i), col=col.alpha("black", 0.5))

## 12.4.3 Focus and multilevel prediction
## multilevel models contain parameters with different FOCUS - i.e. which
## level of the model the parameter makes direct predictions for.
## (1) when retrodicting the sample, the parameters that describe the population
## of clusters do not influence prediction directly. These population parameters
## are called HYPERPARAMETERS, as they are parameters for parameters and they have
## their effects during estimation by shrinking the varying effect parameters
## towards a common mean.
## (2) the same is true when forecasting a new observation for a cluster that was
## present in the sample.
## (3) when we wish to forecast for some new (unseen) cluster, we need the 
## hyperparameters as they tell us how to forecast a new cluster by generating
## a distribution of new per-cluster intercepts.

## over-dispersed Poisson model
library(rethinking)
data(Kline)
d <- Kline
str(d)
d$logpop <- log(d$population)
d$society <- 1:10
m12.6 <- map2stan(
  alist(
    total_tools ~ dpois(mu),
    log(mu) <- a + a_society[society] + bp*logpop,
    a ~ dnorm(0,10),
    bp ~ dnorm(0,1),
    a_society[society] ~ dnorm(0,sigma_society),
    sigma_society ~ dcauchy(0,1)
  ), data = d, iter=4000, chains=3
)

## to see the general trend that the model expects we need to simulate
## counterfactual societies using hyperparameters alpha and sigma_society

post <- extract.samples(m12.6)
d.pred <- list(
  logpop = seq(from=6, to=14, length.out=30),
  society = rep(1,30)
)
a_society_sims <- rnorm(20000,0,post$sigma_society)
a_society_sims <- matrix(a_society_sims, 2000, 10)
link.m12.6 <- link(m12.6, n=2000, data = d.pred, replace = list(a_society=a_society_sims))

plot(d$logpop, d$total_tools, col=rangi2, pch=16, xlab="log population", ylab="total tools")
mu.median <- apply(link.m12.6, 2, median)
lines(d.pred$logpop, mu.median)

mu.PI <- apply(link.m12.6, 2, PI, prob=.97)
shade(mu.PI, d.pred$logpop)
mu.PI <- apply(link.m12.6, 2, PI, prob=.89)
shade(mu.PI, d.pred$logpop)
mu.PI <- apply(link.m12.6, 2, PI, prob=.67)
shade(mu.PI, d.pred$logpop)
