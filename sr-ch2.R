## sr-ch2.R
library(rethinking)

## Bayesian model design loop
## (1) Data Story - motivate the model by narrating how the data might arise
## (2) Update - educate your model by feeding it data
## (3) Evaluate - all models require supervision and possible revision

## Given observation: W L W W W L W L W


## LIKELIHOOD - mathematical formula that specifies the plausability of data.
##              the likelihood maps each conjecture onto the relative number
##              of ways the data could occur, given that possbility.
## 

## Likelihood that the globe is 50% water
dbinom(6, size = 9, prob = 0.5)
## [1] 0.1640625
## ^^ relative number of ways of getting 6 W in 9 tosses with
## the conjecture that the globe is covered in 50% water.


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
## use a flat prior
(prior <- rep(1, 20))

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

## MAP - MAXIMUM A POSTERIORI (mode of the posterior distribution)

globe.qa <- map(
  alist(
    w ~ dbinom(9, p), # binomial likelihood
    p ~ dunif(0, 1)
  ),
  data = list(w=6)
)
globe.qa
precis(globe.qa)
##   Mean StdDev 5.5% 94.5%
## p 0.67   0.16 0.42  0.92
## Assuming the posterior is gaussian, it is maximized at 0.67 and its
## standard deviation is 0.16


## 2.4.3 Markov Chain Monte Carlo
## MCMC - family of conditioning engines capable of handling highly complex
## models. Instead of trying to compute or approximate the posterior distribution
## directly, MCMC merely samples from the posterior distribution.

## 2.6 Practice

## 2E1. Which of the expressions below correspond to the statement: 
## the probability of rain on Monday?
## (2) Pr(rain|Monday)

## 2E2. Which of the following statements corresponds to the expression:
## Pr(Monday|rain) ?
## (3) The probability it is Monday given it is raining

## 2E3. Which of the expressions below correspond to the statement:
## the probability that it is Monday given that it is raining.
## (1) Pr(Monday|rain)

## 2E4. What does it mean to say the probability of water is 0.7?
## Probability resides with the observer bc we do not have complete knowledge.
## If we did know (exact trajectory of toss, rotation of globe, etc.) then we
## would be able to precisely predict whether the toss will result in W or L.

## 2M1. Compute and plot the grid approximate posterior distribution for
## each of the following sets of observatioins. Assume uniform prior for p.

plotPosteriorDistribution <- function(x, size) {
  ## define grid
  (p_grid <- seq(from = 0, to = 1, length.out = 20))
  
  ## define prior
  ## use a flat prior
  (prior <- rep(1, 20))
  
  ## compute likelihood
  likelihood <- dbinom(x, size = size, prob = p_grid)
  
  ## compute prod of likelihood and prior
  unstd.posterior <- likelihood * prior
  
  ## standardize posterior
  posterior <- unstd.posterior/sum(unstd.posterior)
  
  plot(p_grid, posterior, type="b", xlab="probability of water", 
       ylab = "posterior probability")
  mtext("20 pts")
}

## (1) W, W, W
plotPosteriorDistribution(x = 3, size = 3)
## (2) W, W, W, L
plotPosteriorDistribution(x = 3, size = 4)
## (3) L, W, W, L, W, W, W
plotPosteriorDistribution(x = 5, size = 7)

## 2M2. Assume a prior for p that is equal to zero when p < 0.5 and is a
## positive constant when p >= 0.5 - compute and plot the grid approximate
## posterior distribution for each of the sets of observation in problem 2M1.

plotPD <- function(x, size) {
  ## define grid
  (p_grid <- seq(from = 0, to = 1, length.out = 20))
  
  ## define prior
  ## use a flat prior
  (prior <- rep(1, 20))
  ## but us zero for these conjectures
  prior[p_grid < 0.5] <- 0
  
  prior## compute likelihood
  likelihood <- dbinom(x, size = size, prob = p_grid)
  
  ## compute prod of likelihood and prior
  unstd.posterior <- likelihood * prior
  
  ## standardize posterior
  posterior <- unstd.posterior/sum(unstd.posterior)
  
  plot(p_grid, posterior, type="b", xlab="probability of water", 
       ylab = "posterior probability")
  mtext("20 pts")
}

## (1) W, W, W
plotPD(x = 3, size = 3)
## (2) W, W, W, L
plotPD(x = 3, size = 4)
## (3) L, W, W, L, W, W, W
plotPD(x = 5, size = 7)

## 2M3. Suppose globe for earth and mars. Earth globe covered in 70% water,
## mars is 100% land. One globe (you don't know which) was tossed in the air and
## produced a land observation. Each globe was equally likely to be tossed.
## Show that the posterior probability that the globe was the earth, conditional
## on seeing land (Pr(earth|land)) is 0.23.

## Pr(earth) = 0.5
## Pr(mars) = 0.5
## Pr(land|earth) = 0.3
## Pr(land|mars) = 1.0
## Pr(earth|land) = Pr(land|earth)*Pr(earth)/Pr(land)
0.3 * 0.5/.65

## 2M4. Suppose a deck with three cards. One card has two black sides. Second
## card has one black and one white side. Third card has two white sides. All cards
## placed in bag and shuffled. Someone pulls out a card and places it flat on the table.
## a black side is facing up. Show that the probability the other side is also black
## is 2/3. Using the counting method to approach the problem.

## Conjecture OBS(B) Plausability
## B/B        2      2/3
## B/W        1      1/3
## W/W        0      0/3
ways <- c(2,1,0)
ways/sum(ways)

## 2M5. Suppose 4 cards: B/B, B/B, B/W, & W/W. Suppose a single card showing a black
## face. Compute probability other face is black.
## Conjecture OBS(B) Plausability
## B/B        4      4/5
## B/W        1      1/5
## W/W        0      0/5
ways <- c(4,1,0)
ways/sum(ways)

## 2M6. Imagine that the black ink is heavy, so that it is less likely that
## a card with black sides is pulled from the bag. Assume 3 cards: B/B, B/W, W/W.
## For every way to pull the B/B there are 2 ways to pull B/W and ways for W/W.
## Suppose a pull resulting in a faceup black. Show that the probability the other side
## is black is 0.5

## Conjecture OBS(B) Prior   Ways  Plausability
## B/B        2      1       2   
## B/W        1      2       2   
## W/W        0      3       0
ways <- c(2,2,0)
ways/sum(ways)

## 2M7. Assume original problem w single card blackside up. Draw a second which
## is whiteside up. Show that the probability that the first card has black on its
## hidden side os 0.75.

## Conj       Obs(B)   Obs(W)  Ways
## (B/B)(W/W)    2        2       4 2 8
## (W/W)(B/B)    0        0       0
## (B/B)(B/W)    2        1       2 2 4
## (B/W)(B/B)    1        0       0
## (W/W)(B/W)    0        1       0
## (B,W)(W,W)    1        2       2 1 2
ways <- c(8,4,2)
ways/sum(ways)

## 2H1. Suppose two species bear that are perfectly identical except in their
## family sizes. Species A give birth to twins 10% of the time, otherwise single
## infants. Species B births twins 20% of the time, otherise single infants. Suppose
## you are managing a bear breeding program, and you have a female of an unknown species
## who just gave birth to twins. What is the probability that her next birth will be
## twins?


## 2H2.
## Conjecture   Obs(twin) Plaus
## A            .1         1/3
## B            .2         2/3
## Answer: .33

ways <- c(.1,.2)
ways/(sum(ways))

## 2H3.
## Conjecture   Obs(twin) Obs(sing) ways Plaus
## A            .1         .9       .09
## B            .2         .8       .16
## Answer: .36

ways <- c(.1*.9,.2*.8)
ways
ways/(sum(ways))

## 2H4. genetic test administered first correct at species A identification 0.8
## and at species B identification 0.65
ways <- c(.8,.65)
ways
ways/(sum(ways))

ways <- c(.1*.9*.8,.2*.8*.65)
ways
ways/(sum(ways))
