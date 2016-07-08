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

