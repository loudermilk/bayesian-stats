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
library(rethinking)
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

s1 <- sample(p_grid, prob=posterior, size=1e3, replace=TRUE)
s2 <- sample(p_grid, prob=posterior, size=1e4, replace=TRUE)
s3 <- sample(p_grid, prob=posterior, size=1e5, replace=TRUE)
s4 <- sample(p_grid, prob=posterior, size=1e6, replace=TRUE)
plot(samples)
dens(s1, col = "red")
dens(s2, col = "orange", add = T)
dens(s3, col = "green", add = T)
dens(s4, col = "black", add = T)

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
## observed 3 W in 3 tosses
(p_grid <- seq(from = 0, to = 1, length.out = 1000))
prior <- rep(1, 1000)
likelihood <- dbinom(3, size = 3, prob = p_grid)
unstd.posterior <- likelihood * prior
posterior <- unstd.posterior/sum(unstd.posterior)
samples <- sample(p_grid, prob = posterior, size = 1e4, replace = T)

## Standard PI missed the mode
(res <- PI(samples, prob=0.5))
dens(samples)
abline(v = res[1])
abline(v = res[2])

## use Highest Posterior Density Interval instead 
(res <- HPDI(samples, prob = 0.5))
dens(samples, xlim = c(0,1))
abline(v = res[1])
abline(v = res[2])

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

## 3.5 Practice

p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior <- rep(1, 1000)
likelihood <- dbinom(6, size = 9, prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior/sum(posterior)
set.seed(100)
samples <- sample(p_grid, prob=posterior, size = 1e4, replace = T)

## 3E1. How much posterior probability lies below p = 0.2
sum(samples < 0.2)/1e4
hist(samples)

## 3E2. How much posterior probability lies above p =0.8?
1 - sum(samples <= 0.8)/1e4

## 3E3. How much posterior prob lies between 0.2 and 0.8?
sum(samples > 0.2 & samples < 0.8)/1e4

## 3E4. 20% of the posterior probability lies below what point
quantile(samples,c(.2))

## 3E5. 20% of the prob lies above what point?
quantile(samples,c(.8))

## 3E6. Which values of p contain the narrowest interval equal to 66% of prob
(res <- HPDI(samples, prob = 0.66))

## 3E7. Which values of p contain 66% of the posterior probability assuming
## equal posterior probability both below and above the interval?
(res <- PI(samples, prob = 0.66))

## 3M1. Suppose the globe tossing data had turned out to be 8 W in 15 tosses. 
## Construct the posterior distribution using grid approximation. Use flat prior.

p_grid <- seq(from = 0, to = 1, by = .01)
prior <- rep(1, length(p_grid))
likelihood <- dbinom(x = 8, size = 15, prob = p_grid)
unstd.posterior <- likelihood *prior
posterior <- unstd.posterior/sum(unstd.posterior)

## 3M2. Draw 10k samples and compute 90% HPDI
samples <- sample(p_grid, size = 1e4, prob = posterior, replace = T)
dens(samples)
HPDI(samples = samples, prob = 0.9)

## 3M3. Construct a posterior predictive check for this model and data. This
## means simulate the distribution of samples, averaging over the posterior
## uncertainty in p. What is the probability of observing 8 W in 15 tosses.

## Posterior predictive checks are, in simple words, "simulating replicated data 
## under the fitted model and then comparing these to the observed data" 
## So, you use posterior predictive to "look for systematic discrepancies between real and simulated data" 
## (Gelman et al. 2004, p. 169).

## The main idea behind posterior predictive checking is the notion that, if the model fits, then 
## replicated data generated under the model should look similar to observed data.
w <- rbinom(1e4, size = 15, prob = samples)
hist(w)
