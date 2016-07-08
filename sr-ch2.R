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
##              sample size), and w (the number of Ws).