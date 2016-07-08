## sr-ch3.R

## Chapter 3 - Sampling the Imaginary
## Bayes Theorem
## Pr(A|B) = Pr(B|A)Pr(A)/Pr(B)
## Pr(B) = Pr(B|A)*Pr(A) + Pr(B|~A)Pr(~A)

## Pr(pos|vampirism) = 0.95
## Pr(pos|mortal) = 0.01
## Pr(vampirism) = 0.001
## Pr(vampirism|pos) = ???

## Pr(vamp|pos) = Pr(pos|vamp)Pr(vamp)/Pr(pos)
## = 0.95 * 0.001 / Pr(pos)
## Pr(pos) = Pr(pos|vamp)Pr(vamp) + Pr(pos|mort)(1 - Pr(vamp))
## = 0.95 * 0.001 + 0.01 * (1-0.001)
## = 0.01094
## Pr(vamp|pos) (0.95 * 0.001)/0.01094
## 0.0868 8.7% chance of being vamp

PrPV <- 0.95
PrPM <- 0.01
PrV <- 0.001
PrM <- 1 - PrV
(PrVP <- PrPV * PrV/(PrPV * PrV + PrPM * PrM) )

## 3.1 Sampling from a grid-approximate posterior
## define grid
(p_grid <- seq(from = 0, to = 1, length.out = 1000))

## define prior
prior <- rep(1, 1000)

## compute likelihood
likelihood <- dbinom(6, size = 9, prob = p_grid)

## compute prod of likelihood and prior
unstd.posterior <- likelihood * prior

## standardize posterior
posterior <- unstd.posterior/sum(unstd.posterior)


samples <- sample(p_grid, prob=posterior, size=1e4, replace=TRUE)
plot(samples)

library(rethinking)
dens(samples)

## 3.2 Sampling to summarize
## Common questions:
## (1) - intervals of defined boundaries
## (2) - questions abt defined probability mass
## (3) - questions abt point estimates


## 3.2.1 Intervals of defined boundaries
## (Q) what is the posterior probability that the percentage of water is 50%?
p <- 0.5
obs <- 6
size <- 9
dbinom(obs, size = size, prob = p)

sum(posterior[p_grid < 0.5]) ## 0.1718
sum(samples < 0.5)/1e4


## (Q) how much posterior prob lies bw 0.5 & 0.75?
sum(samples > 0.5 & samples < 0.75)/1e4

## 3.2.2 Intervals of defined mass
## Credible Interval reports two parameter values that contain between them
## a specified amount of posterior probability, a probability mass.

## (Q) What are the boundaries of the lower 80% posterior probability?
## it starts at p=0 and stops where the 80 percentile lies
quantile(samples, 0.8)

## (Q) what are the boundaries of the middle 80%?
quantile(samples, 0.9) - quantile(samples, 0.1)
quantile(samples, c(0.1, 0.9))

## Problems with percentile intervals

(p_grid <- seq(from = 0, to = 1, length.out = 1000))
prior <- rep(1, 1000)
likelihood <- dbinom(3, size = 3, prob = p_grid)
unstd.posterior <- likelihood * prior
posterior <- unstd.posterior/sum(unstd.posterior)
samples <- sample(p_grid, prob = posterior, size = 1e4, replace = T)
## Standard PI missed
PI(samples, prob=0.5)
## use Highest Posterior Density Interval instead 
HPDI(samples, prob = 0.5)

## Maximum a posteriori (MAP)
## Report parameter value with the highest posterior probabilty
p_grid[which.max(posterior)] ## if you have entire post dist

# how to choose between mode, mean, median
chainmode(samples, adj = 0.01)
mean(samples)
median(samples)

## LOSS FUNCTION - rule that tells you the cost associated with using any
## particular point estimate. Different loss functions imply different
## point estimates. The parameter that maximizes expected winnings (minimizes
## expected loss) is the median of the posterior distribution.

## suppose of point estimate is p = 0.5
sum(posterior*abs(0.5 - p_grid)) # <- weighted average loss
## compute for all decisions
loss <- sapply(p_grid, function(d) {sum(posterior*abs(d-p_grid))})
p_grid[which.min(loss)] # same values as median(samples)

## 3.3 Sampling to simulate prediction
## Generating implied observations from a model is useful bc:
## (1) Model checking 
## (2) Software validation
## (3) Research design
## (4) Forecasting

## DUMMY DATA - simulated data that is a stand-in for actual data
## with the globe toss, dummy data arises from the binomial likelihood
## suppose 2 tosses of the globe - there are three possible observations:
## W = 0, W = 1, & W = 2.

dbinom(0:2, size = 2, prob = 0.7)
rbinom(10, size = 2, prob = 0.7)
dummy_w <- rbinom(1e5, size = 2, prob = 0.7)
table(dummy_w)/1e5

# simulate the real sample size
dummy_w <- rbinom(1e5, size = 9, prob = 0.7)
simplehist(dummy_w, xlab = "dummy water count")

## 3.3.2 Model checking
## model checking means (1) ensuring the model fitting worked correctly and
## (2) evaluating the adequacy of a model of some purpose.

w <- rbinom(1e4, size = 9, prob = 0.6)
simplehist(w)

(p_grid <- seq(from = 0, to = 1, length.out = 1000))
prior <- rep(1, 1000)
likelihood <- dbinom(6, size = 9, prob = p_grid)
unstd.posterior <- likelihood * prior
posterior <- unstd.posterior/sum(unstd.posterior)
samples <- sample(p_grid, prob = posterior, size = 1e4, replace = T)

w <- rbinom(1e4, size = 9, prob = samples)
simplehist(w)

## seek out aspects in prediction in which the model fails
## (1) longest contiguous run (data = 3)
## (2) number of switches (data = 6)
