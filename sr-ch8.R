## sr-ch8.R
## Chapter 8 - Markov Chain Monte Carlo

## 8.1 Good King Markov and His Island Kingdom

num_weeks <- 1e5
current <- 10
positions <- rep(0, num_weeks)
for (i in 1:num_weeks) {
  positions[i] <- current
  proposal <- current + sample(c(-1,1),size=1)
  if (proposal < 1) proposal <- 10
  if (proposal > 10) proposal <- 1
  prob_move <- proposal/current
  current <- ifelse(runif(1) < prob_move,proposal, current)
}


## 8.2.1 Gibbs Sampling

library(rethinking)
data(rugged)
d <- rugged
d$log_gdp <- log(d$rgdppc_2000)
dd <- d[complete.cases(d$rgdppc_2000),]
names(dd)
m8.1 <- map(
  alist(
    log_gdp ~ dnorm(mu, sigma),
    mu <- a + bR*rugged + bA*cont_africa + bAR*rugged*cont_africa,
    a ~ dnorm(0,100),
    c(bR, bA, bAR) ~ dnorm(0,10),
    sigma ~ dunif(0,10)
  ), data = dd
)
precis(m8.1)

## 8.3.1 Preparation.
## Fit the model using Hamiltonian Monte Carlo
## (1) preprocess all variable transformations (incl in df)
## (2) reduce df to only contain variables of interest

names(dd)
dd.trim <- dd[,c("rugged", "cont_africa", "log_gdp")]
str(dd.trim)

## 8.3.2 Estimation
m8.1stan <- map2stan(
  alist(
    log_gdp <- dnorm(mu, sigma),
    mu <- a + bR*rugged + bA*cont_africa + bAR*rugged*cont_africa,
    a ~ dnorm(0,100),
    c(bR, bA, bAR) ~ dnorm(0,10),
    sigma ~ dcauchy(0,2)
  ), data = dd.trim
)
precis(m8.1stan)
## n_eff - crude estimate of the number of independent samples you got
## Rhat - estimate of the convergence of the Markov chains to the target dist
## Rhat should approach 1.00 from above if all is well

## 8.3.3 Sampling again in parallel
## Using an existing stan model you can draw more samples from it, running
## as many independent Markov chains as you want.
m8.1stan_4chains <- map2stan(m8.1stan, chains = 4, cores = 4)
precis(m8.1stan_4chains)

## 8.3.4 Visualization
post <- extract.samples(m8.1stan)
class(post)
str(post)
length(post)
pairs(post)
pairs(m8.1stan)

## 8.3.5 Using the samples
show(m8.1stan)

## 8.3.6 Checking the chain
## Causes and solutions for malfunctions
## TRACE PLOT - plots the samples in sequential order joined by a line
plot(m8.1stan)
## Diagnosis - interpret the plot for a good chain:
## (1) staionarity - does the path stay within the posterior distribution?
## (2) well-mixed - no correlation with previous event -- zigzag is good

## Grey marks the adaption phase - learning to more efficiently sample
## from the postereior distribution (thus not necessarily reliable for inference)
## extract.samples only returns those in the white region

## 8.4 Care and feeding of your Markov chain

## 8.4.1 How many samples do you need?
## iter = 2000; warmup = iter/2

## 8.4.2 How many chains do you need?
## (1) when debugging use a single chain
## (2) when deciding whether chains are valid you need more than one chain
## (3) when you begin final run to make ineferences from you need one chain

## motto: four short chains to check, one long chain for inference

## 8.4.3 Taming a wild chain
## one problem w some models is that there are broad flat regions of the 
## posterior density (typically caused by using flat priors). This can generate
## a wild wandering markov chain that erratically samples extremely positive and
## extremely negative parameter values.

y <- c(-1,1)
map8.2 <- map2stan(
  alist(
    y ~ dnorm(mu, sigma),
    mu <- alpha
  ), data=list(y=y), start=list(alpha=0, sigma = 1), chains = 2, iter = 4000, warmup = 1000)

precis(map8.2)
plot(map8.2)

## Tame the chain by using weakly informative priors
## Flat priors say that every possible value of the parameter is equally
## plausible apriori.

y <- c(-1,1)
m8.3 <- map2stan(
  alist(
    y ~ dnorm(mu, sigma),
    mu <- alpha,
    alpha ~ dnorm(0,10),
    sigma ~ dcauchy(0,1)
  ), data=list(y=y), start=list(alpha=0, sigma = 1), chains = 2, iter = 4000, warmup = 1000)
precis(m8.3)
plot(m8.3)

## 8.4.4 Non-identifiable parameters
## construct a non-identifiable model
y <- rnorm(100, mean = 0, sd = 1)

m8.4 <- map2stan(alist(
  y ~ dnorm(mu, sigma),
  mu <- a1 + a2,
  sigma ~ dcauchy(0,1)
), data=list(y=y), start=list(a1=0,a2=0,sigma=1), chains=2, iter=4000, warmup=1000
)
## ^^ contains two parameters a1 & a2 that cannot be identified
precis(m8.4)
plot(m8.4)

## weak priors to the rescue!!!

m8.5 <- map2stan(
  alist(
    y ~ dnorm(mu, sigma),
    mu <- a1 + a2,
    c(a1,a2) ~ dnorm(0,10),
    sigma ~ dcauchy(0,1)
  ), data=list(y=y), start=list(a1=0,a2=0,sigma=1), chains=2, iter=4000, warmup=1000
)
precis(m8.5)
plot(m8.5)

## 8H1. Run the model below and then inspect the posterior distribution and
## explain what it is accomplishing.
mp <- map2stan(
  alist(
    a ~ dnorm(0,1),
    b ~ dcauchy(0,1)
  ), 
  data = list(y=1),
  start = list(a=0,b=0),
  iter = 1e4, warmup = 1000, WAIC=FALSE
)
precis(mp)
plot(mp)

