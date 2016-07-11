## sr-ch4.R

## Simulate random walk on football field
sum(runif(16, -1, 1)) # 16 random steps between -1 and 1, sum is total dist

pos <- replicate(1000, sum(runif(16, -1, 1)))
hist(pos) ## approx normal
plot(density(pos)) ## approx normal

## 4.1.2 Normal by multiplication
prod(1 + runif(12, 0, 0.1))
growth <- replicate(1000, prod(1 + runif(12, 0, 0.1)))
dens(growth, norm.comp = T)
hist(growth)
plot(density(growth))

big <- replicate(1000, prod(1 + runif(12, 0, 0.5)))
small <- replicate(10000, prod(1 + runif(12, 0, 0.1)))
dens(big, norm.comp = T)
dens(small, norm.comp = T)


## 4.1.3 Normal by log-multiplication
## large deviates that are multiplied together 

log.big <- replicate(10000, log(prod(1 + runif(12, 0, 0.5))))

dens(log.big, norm.comp = T)

## 4.2 A language for describing models
## (1) recognize a set of measurements that we hope to predict or understand, 
##     the outcome variable or variables.
## (2) for each of tese outcome variables, we define a likelihood distribution
##     that defines the plausability of individual observations. 
##     In linear regression this distribution is always Gaussian.
## (3) then we recognize a set of other measurements that we hope to use to
##     predict or understand the outcome. Call these predictor variables.
## (4) we relate the exact shape of the likelihood distribution - its precise
##     location and variance and other aspects of its shape, if it has them -
##     to the predictor variables. In choosing a way to relate the predictors 
##     to the outcomes we are forced to name and define all the parameters 
##     of the model.
## (5) we choose priors for all the parameters in the model. These priors define
##     the initial information state of the model before seeing the data.

## 4.2.1 Re-describing the globe tossing model
## w ~ Binomial(n, p) <- defines likelihood function used in Bayes Theorem
## p ~ Uniform(0,1) <- other lines define priors
## The count w is distributed binomially with sample size n and probability p.
## The prior p is assumed to be uniform from 0 to 1.
## Both of the lines in this model are stochastic (~) - it maps variables or
## parameters into a distribution. It is stoachastic bc no single instance
## of the variable on the left is known with certainty, rather it is probabilistic

w <- 6; n <- 9
p_grid <- seq(from=0, to=1, length.out=100)
posterior <- dbinom(w,n,p_grid) * dunif(p_grid,0,1)
posterior <- posterior/sum(posterior)
plot(posterior)

## 4.3 A Gaussian model of height
## 4.3.1 The data
library(rethinking)
data(Howell1)
d <- Howell1
head(d)
str(d)

# only work with adult data
d2 <- d[d$age >= 18,]
dim(d2)
dens(d2$height, main = "Adult !Kung Height")
summary(d2)

## Model this data using a Gaussian distribution
## subscript i => each individual element of this list
## h_i ~ Normal(mu,sig) <= likelihood
## mu ~ Normal(178, 20) <= mu prior (based on authors height)
## sigma ~ Uniform(0, 50) <= sigma prior

## plot priors to understand model assumptions
curve(dnorm(x, 178, 20), from = 100, to=250)
curve(dunif(x,0,50),from=10, to=60)

## simulate heights by sampling the prior
sample_mu <- rnorm(1e4, 178, 20)
sample_sigma <- runif(1e4, 0, 50)
prior_h <- rnorm(1e4, sample_mu, sample_sigma)
dens(prior_h)

## 4.3.3 Grid approximation of the posterior distribution

mu.list <- seq(from = 140, to = 160, length.out = 200)
sigma.list <- seq(from = 4, to = 9, length.out = 200)
post <- expand.grid(mu = mu.list, sigma = sigma.list)
post$LL <- sapply(1:nrow(post), function(i) sum(dnorm(
  d2$height,
  mean = post$mu[i],
  sd = post$sigma[i],
  log = TRUE
)))
post$prod <- post$LL + dnorm(post$mu, 178, 20, TRUE) +
  dunif(post$sigma, 0 , 50, TRUE)
post$prob <- exp(post$prod - max(post$prod))



contour_xyz(post$mu, post$sigma, post$prob)
image_xyz(post$mu, post$sigma, post$prob)

## 4.3.4 Sampling from the posterior

sample.rows <- sample(1:nrow(post), size=1e4, replace = TRUE, prob = post$prob)
sample.mu <- post$mu[sample.rows]
sample.sigma <- post$sigma[sample.rows]

plot(sample.mu, sample.sigma, cex = 0.5, pch = 16, col=col.alpha(rangi2,0.1))
dens(sample.mu)
dens(sample.sigma)
HPDI(sample.mu)
HPDI(sample.sigma)

d3 <- sample(d2$height, size = 20)


## 4.3.5 Fitting the model with map (quadratic approximation)

library(rethinking)
data(Howell1)
d <- Howell1
d2 <- d[d$age >= 18, ]

## Model definition + Corresponding R
## h_i ~ Normal(mu, sigma)   height ~ dnorm(mu, sigma)
## mu ~ Normal(178, 20)      mu ~ dnorm(178, 20)
## sigma ~ Uniform(0, 50)    sigma ~ dunif(0,50)

## Put R-code equalivalents into an alist
flist <- alist(height ~ dnorm(mu, sigma), 
               mu ~ dnorm(178, 20), 
               sigma ~ dunif(0,50))

## fit the model
m4.1 <- map(flist, data = d2)
precis(m4.1)
## these numbers provide Gaussian approximations for each parameter's
## marginal distribution. This means the plausibility of each value mu, 
## after averaging over the plausabilities of each value of sigma is given
## by a dist with mean 154.6 and SD 0.4
precis(m4.1, prob = 0.95)

## map estimates posterior by climbing it like a hill - it starts by random
## sampling of the prior, but you can explicitly set start values

start <- list(mu=mean(d2$height),
              sigma=sd(d2$height))

m4.2 <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu ~ dnorm(178, 0.1),
    sigma ~ dunif(0,50)
  ),
  data = d2
)
precis(m4.2)

## Sampling from a map fit
## a quadratic approximation to a posterior distribution with more than one
## parameter dimension is just a multi-dimensional gaussian distribution

## to view list of variance and covariances for multi-dim more do:
vcov(m4.1)

## variance-covariance matrix can be factored into two elements:
## (1) a vector of variances for the parameters
## (2) a correlation matrix that tells use how changes in any parameter
## lead to correlated changes in the others

diag(vcov(m4.1))
cov2cor(vcov(m4.1))

## how to sample

library(rethinking)
post <- extract.samples(m4.1, n = 1e4)
head(post)
precis(post)

## 4.4 Adding a predictor
plot(d2$height ~ d2$weight)

## 4.4.1 Linear model strategy
## add weight to gaussian model of height

## Model definition + Corresponding R
## h_i ~ Normal(mu_i, sigma)  ## height ~ dnorm(mu, sigma)
## mu_i = a + b * x_i         ## mu <- a + b*weight
## a ~ Normal(178, 100)       ## a ~ dnorm(178, 100)
## b ~ Normal(0,10)           ## b ~ dnorm(0,10)
## sigma ~ Uniform(0, 50)     ## sigma ~ dunif(0, 50)

## mu_i = a + b*x_i
## (1) what is the expected height, when x_i = 0
## (2) what is the change in expected height when x_i changes by 1 unit.

## 4.4.1.3 Priors
library(rethinking)
data(Howell1)
d <- Howell1
d2 <- d[d$age >= 18,]
m4.3 <- map(alist(
  height ~ dnorm(mu, sigma),
  mu <- a + b*weight,
  a ~ dnorm(156, 100),
  b ~ dnorm(0,10),
  sigma ~ dunif(0,50)
), data = d2)

## 4.4.3 Interpreting the model fit

## PLOT THE STUFF
## (1) whether or not model fitting procedure worked correctly
## (2) the absolute magnitude of a relationship between outcome & predictor
## (3) the uncretainty surrounding an average relationship
## (4) the uncertainty surrounding the implied predictions of the model, as these
##     are distinct from mere parameter uncertainty.

## posterior probabilities of parameter values describe the relative comparability
## of different states of the world with the data, according to the model.

## 4.4.3.1 Table of estimates

precis(m4.3)
precis(m4.3, corr = T)


## CENTERING - subtracting the mean of a variable from each value

d2$weight.c <- d2$weight - mean(d2$weight)

## refit model using centered weight
m4.4 <- map(alist(
  height ~ dnorm(mu, sigma),
  mu <- a + b*weight.c,
  a ~ dnorm(178, 100),
  b ~ dnorm(0,10),
  sigma ~ dunif(0,50)
), data = d2)


precis(m4.4, corr = T)

## 4.4.3.2 Plotting posterior inference against the data

plot(height ~ weight, data = d2, xlab = "Weight (kg)", ylab = "Height (cm)")
abline(a = coef(m4.3)["a"], b = coef(m4.3)["b"])

## Adding uncertainty around the mean
## sample
post <- extract.samples(m4.3)
post[1:5,]


par(mfrow=c(2,2))

data_sizes <- c(10,50,100,200)
for (N in data_sizes) {
  dN <- d2[1:N,]
  dN
  mN <- map(
    alist(
      height ~ dnorm(mu, sigma),
      mu <- a + b * weight,
      a ~ dnorm(178, 100),
      b ~ dnorm(0, 10),
      sigma ~ dunif(0, 50)
    ),
    data = dN
  )
  
  ## extract 20 samples from posterior
  post <- extract.samples(mN, n = 20)
  
  ## display raw data and sample size
  plot(dN$weight, dN$height, xlim = range(d2$weight), ylim = range(d2$height), col = rangi2)
  mtext(paste0("N = ", N))
  
  for (i in 1:20) {
    abline(a=post$a[i], b = post$b[i], col=col.alpha("black", 0.3))
  }
}

## Plotting regression intervals and contours
## for pedagogy focus on a single weight value, e.g., 50 kg
mu_at_50 <- post$a + post$b * 50
mu_at_50
dens(mu_at_50, col=rangi2, lwd=2, xlab = "mu|weight=50")
HPDI(mu_at_50, prob = .89)

## link function - takes map model fit, samples from the posterior distribution,
## computes mu for each case

mu <- link(m4.3)
str(mu)

## define sequences of weights to compute predictions for these values will be
## on the horiz axis
weight.seq <- seq(from=25, to = 70, by = 1)
## use link to compute mu for each sample from posterior and 
## for each weight in weght.seq

mu <- link(m4.3, data = data.frame(weight=weight.seq))
str(mu)

mu.mean <- apply(mu, 2, mean)
mu.HPDI <- apply(mu, 2, HPDI, prob = .89)
plot(height ~ weight, data = d2, col = col.alpha(rangi2, 0.5))
lines(weight.seq, mu.mean)
shade(mu.HPDI, weight.seq)

## How to generate predictions and intervals from the posterior of a fit model
## (1) Use link to generate distributions of posterior values of mu. The 
## default behavior of link is to use the original data, so yo have to pass it a 
## list of new horizontal axis values you want to plot posterior predictions across.
## (2) Use summary functions like mean, HPDI, PI to find avergaes and lower and 
## upper bounds of mu for each value of the predcitor variable.
## (3) Use plot funcs lines() and shade() to draw intervals
