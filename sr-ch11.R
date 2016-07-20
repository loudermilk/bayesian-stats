## sr-ch11.R
## Chapter 11 - Monster and Mixtures

## 11.1 Ordered categorical outcomes
## Built by merging a categorical likelihood function with a special kind of
## link function, usually a CUMULATIVE LINK.

## 11.1.1 Moral intuition

library(rethinking)
data(Trolley)
d <- Trolley
dim(d)
head(d)

## 11.1.2 Describing an ordered distribution with intercepts
simplehist(d$response, xlim=c(1,7), xlab="response")

## discrete proportion of each response variable
(pr_k <- table(d$response)/nrow(d))

(cum_pr_k <- cumsum(pr_k))
plot(1:7, cum_pr_k, type = "b", xlab="response", ylab="cumulative proportion", ylim=c(0,1))

logit <- function(x) log(x/(1-x))
(lco <- logit(cum_pr_k))
plot(1:7, lco, type = "b", xlab="response", ylab="log-cumulative odds", ylim=c(-2,2))


