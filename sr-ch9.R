## sr-ch9.R
## Chapter 9 - Big Entropy and the Generalized Linear Model
## Bet on the distribution with the biggest entropy b/c
## (a) dist w biggest entropy is the widest and has least informative distribution
## (b) nature tends to produce distributions w high entropy
## (c) regardless of why it works, it tends to work well

## Generalized Linear Model - a model that replaces a parameter of a likelihood model
## with a linear model
## Principle of maximum likelihood - helps us choose a likelihood function by providing
## a way to use state assumptions about constraints on the outcome variable to choose the 
## likelihood function that is the mst conservative distribution compatible with known constraints

## 9.1 Maximum Entropy
## The distribution that can happen the most ways is also the distribution with the
## biggest information entropy. The distribution with the biggest entropy is the most
## conservative distribution that obeys its constraints.

## 10 pebbles into 5 buckets so that each arrangement of pebbles in buckets is equally likely
p <- list()
p$A <- c(0,0,10,0,0)
p$B <- c(0,1,8,1,0)
p$C <- c(0,2,6,2,0)
p$D <- c(1,2,4,2,1)
p$E <- c(2,2,2,2,2)

p_norm <- lapply(p, function(x){x/sum(x)})
p_norm
# since these are prob dist we can compute the information entropy of each
H <- sapply(p_norm, function(q){-sum(ifelse(q==0,0,q*log(q)))})
H

ways <- c(1,90,1260,37800,113400)
logwayspp <- log(ways)/10
logwayspp

## information entropy is a way of counting how many unique arrangements correspond
## to a distribution
## maximum entropy distribution - the dist that can happen the greatest number of ways
## max ent is the center of gravity for the highly plausible distributions

## 9.1.1 Gaussian
## 9.1.2 Binomial

## build list of the candidate distributions
p <- list()
p[[1]] <- c(1/4,1/4,1/4,1/4)
p[[2]] <- c(2/6,1/6,1/6,2/6)
p[[3]] <- c(1/6,2/6,2/6,1/6)
p[[4]] <- c(1/8,4/8,2/8,1/8)
## compute expected value of each
sapply(p, function(p) sum(p*c(0,1,1,2)))
## compute entropy of each dist
sapply(p, function(p) -sum(p*log(p)))

p <- 0.7
(A <- c((1-p)^2, p*(1-p), (1-p)*p, p^2))
-sum(A*log(A))

sim.p <- function(G=1.4) {
  x123 <- runif(3)
  x4 <- ((G)*sum(x123)-x123[2]-x123[3])/(2-G)
  z <- sum(x123,x4)
  p <- c(x123, x4)/z
  list(H=-sum(p*log(p)),p=p)
}

H <- replicate(1e5, sim.p(1.4))
dens(as.numeric(H[1,]), adj=0.1)

entropies <- as.numeric(H[1,])
distributions <- H[2,]
max(entropies)
distributions[which.max(entropies)]

## 9.2 Generalized Linear Models
## when an outcome variable is discrete or bounded a Gaussian likelihood is not the
## most powerful choice
## by using prior knowledge about outcome variable is the form on constraints on the 
## possible values it can take we can appeal to maximum entropy for the choice of dist.

## Link Function

## 9.2.1 Meet the family
## EXPONENTIAL FAMILY

## (1) EXPONENTIAL DISTRIBUTION - constrained to be zereo or positive. A fundamental distribution
## of distance and duration. If the probability of an event is constant across time
## or space then the distribution of events tends toward exponential. Core of survival and
## event history analysis.

## (2) GAMMA DISTRIBUTION - constrained to be zero or pos.
## (3) POISSON DISTRIBUTION - count distribition like binomial

## 9.2.2 Linking linear models to distributions
## LOGIT LINK - maps a parameter that is defined as a probability mass and therefore
## constrained to lie between 0 and 1, onto a linear model that can take on any real value
## LOG LINK - maps a parameter that is defined over only positive real values onto a 
## linear model.
