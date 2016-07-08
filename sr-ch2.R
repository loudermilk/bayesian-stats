## sr-ch2.R
library(rethinking)

## Bayesian model design loop
## (1) Data Story - motivate the model by narrating how the data might arise
## (2) Update - educate your model by feeding it data
## (3) Evaluate - all models require supervision and possible revision

## Given observation: W L W W W L W L W


## LIKELIHOOD - mathematical formula that specifies the plausability of data.
##              the likelihood maps each conjecture into the relative number
##              of ways the data could occur, given that possbility.
## 

## Likelihood that the globe is 50% water
dbinom(6, size = 9, prob = 0.5)

## Given the observation, what is the most likely conjecture?
## Globe is 67% water
conjectures <- seq(from = 0, to = 1, by = 0.01)
probs <- sapply(conjectures, function(x) {dbinom(6, size = 9, prob = x)})
conjectures[which(probs == max(probs))]


## PARAMETERS - likelihood functions take adjustable inputs. For the binomial
##              likelihood the inputs are p (probability of seeing W), n (the
##              sample size), and w (the number of Ws). We want to estimate 
##              these parameters from the data which represent different
##              conjectures.

## PRIORS - for every parameter you must also provide its prior, an initial
##              plausability setting. Priors are useful for constraining 
##              parameters to a reasonable range as well as for expressing
##              any knowledge we have about the parameter before any data
##              are observed.

## POSTERIOR - for every unique combination of data, likelihood, parameters, 
##             and priors there is a unique set of estimates. The resulting
##             estimates - the relative plausability of different parameter
##             values conditional on the data -- are known as the posterior
##             distribution. The posterior distribution takes the form of the
##             probability of the parameters conditional on the data: Pr(p|n, w)

## Pr(w,p) = Pr(w|p)Pr(p)
## Pr(w,p) = Pr(p|w)Pr(w)
## Pr(w|p)Pr(p) = Pr(p|w)Pr(w)
## Pr(p|w) = Pr(w|p)Pr(p)/Pr(w)
## Posterior = Likelihood * Prior/Average Likelihood

## 2.4.1 Grid Approximation
## (1) Define the grid - Decide how many points to use in estimating the posterior
##    and then make a list of the parameter values on the grid.
## (2) Compute the value of the prior at each parameter value on the grid.
## (3) Compute the likelihood at each parameter value
## (4) Compute the unstandardized posterior at each parameter value, by
##    multiplying the prior by the likelihood.
## (5) Standardize the posterior by dividing each val by sum(val)

## define grid
(p_grid <- seq(from = 0, to = 1, length.out = 20))

## define prior
prior <- rep(1, 20)

## compute likelihood
likelihood <- dbinom(6, size = 9, prob = p_grid)

## compute prod of likelihood and prior
unstd.posterior <- likelihood * prior

## standardize posterior
posterior <- unstd.posterior/sum(unstd.posterior)

plot(p_grid, posterior, type="b", xlab="probability of water", 
     ylab = "posterior probability")
mtext("20 pts")

## 2.4.2 Quadratic Vectorization
## Grid approximation doesn't scale as the number of parameters increases.
## The region near the peak of the posterior distribution is approximately
## gaussian/normal  and can be approximated by a gaussian distribution.
## (1) Find the posterior mode. Accomplished by an optimization algo that 
##     "climbs" the posterior distribution and tries to find the peak.
## (2) Estimate the curvature near the peak.

globe.qa <- map(
  alist(
    w ~ dbinom(9, p), # binomial likelihood
    p ~ dunif(0, 1)
  ),
  data = list(w=6)
)
globe.qa
precis(globe.qa)

## 2.4.3 Markov Chain Monte Carlo
## MCMC - family of conditioning engines capable of handling highly complex
## models. Instead of trying to compute or approximate the posterior distribution
## directly, MCMC merely samples from the posterior distribution.
