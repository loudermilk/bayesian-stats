## sr-ch10.R
## Chapter 10 - Counting and Classification

## (1) Binomial Regression - model a binary classsification for which the total
## of both categories are known.
## (2) Poisson Regression - GLM that models a count outcome without a known
## maximum. The Poisson model can be conceived of as a binomial model
## with a very large maximum but a very sml probability per trial.

## 10.1 Binomial regression
## y ~ Binomial(n, p) # n- num trials; p - probability of trial success
## Binomial dist has max entropy when each trial must result in 1 of 2
## events and the expected value is constant.

## (1) Logistic regression - data are organized into single-trial cases, such
## that the outcome variable can only take values of 0 or 1.
## (2) Aggregated Binomial regression - when individual trials with the same
## covariate values are aggregated. The outcome can take the value zereo or any
## positive integer up to n, the number of trials.

## 10.1.1 Logistic regression: Prosocial chimpanzees
devtools::install_github("rmcelreath/rethinking", force = TRUE)
# install.packages("BiocInstaller")
# source("https://bioconductor.org/biocLite.R")
# biocLite("BiocInstaller")
library(rethinking)
data("chimpanzees")
d <- chimpanzees

## pulled_left - outcome
## prosoc_left - prosoc option associated with left
## condition - 1 = parter, 0 = control
names(d)
## intercept-only model
m10.1 <- map(
  alist(
   pulled_left ~ dbinom(1, p),
   logit(p) <- a,
   a ~ dnorm(0,10)
  ), data = d
)
precis(m10.1)
##   Mean StdDev 5.5% 94.5%
## a 0.32   0.09 0.18  0.46

## need to use inverse of logit to put on probability scale
logistic(c(0.18, 0.46))
names(d)

m10.2 <- map(
  alist(
    pulled_left ~ dbinom(1, p),
    logit(p) <- a + bp*prosoc_left,
    a ~ dnorm(0,10),
    bp ~ dnorm(0,10)
  ), data = d
)
precis(m10.2)
logistic(c(0.27, 0.85))

m10.3 <- map(
  alist(
    pulled_left ~ dbinom(1,p),
    logit(p) <- a + (bp + bpC*condition)*prosoc_left,
    c(a, bp, bpC) ~ dnorm(0,10)
  ), data = d
)

compare(m10.1, m10.2, m10.3)

## 
precis(m10.3)
exp(0.61)

## dummy data for predictions across treatments
d.pred <- data.frame(
  prosoc_left = c(0,1,0,1),
  condition = c(0,0,1,1)
)
chimp.ensemble <- ensemble(m10.1, m10.2, m10.3, data = d.pred)
pred.p <- apply(chimp.ensemble$link, 2, mean)
pred.p.PI <- apply(chimp.ensemble$link, 2, PI)

plot(0,0,type='n', xlab="prosoc_left/condition", ylab="proportion pulled left", ylim=c(0,1),xaxt="n", xlim=c(1,4))
axis(1,at=1:4, labels = c("0/0","1/0", "0/1","1/1"))

p <- by(d$pulled_left, list(d$prosoc_left, d$condition, d$actor), mean)
for (chimp in 1:7) {
  lines(1:4, as.vector(p[,,chimp]), col=rangi2, lwd = 1.5)
}
lines(1:4, pred.p)
shade(pred.p.PI, 1:4)
methods(plot)

## clean NAs from data
d2 <- d
d2$recipient <- NULL
m10.3stan <- map2stan(m10.3, data = d2, iter = 1e4, warmup=1000) 
precis(m10.3stan)
pairs(m10.3stan)

## evidence shows a preference for handedness in the data, let's add it as a distinct
## intercept for each individual, each actor.
names(d2)
m10.4 <- map2stan(
  alist(
    pulled_left ~ dbinom(1,p),
    logit(p) <- a[actor] + (bp + bpC*condition)*prosoc_left,
    a[actor] ~ dnorm(0,10), 
    bp ~ dnorm(0,10), 
    bpC ~ dnorm(0,10) 
  ), data = d2, chains = 2, iter = 2500, warmup = 500
)

unique(d$actor)
precis(m10.4, depth = 2)

post <- extract.samples(m10.4)
str(post)
dens(post$a[,2])

chimp <- 3
d.pred <- list(
  pulled_left = rep(0,4),
  prosoc_left = c(0,1,0,1),
  condition = c(0,0,1,1),
  actor = rep(chimp, 4)
)

link.m10.4 <- link(m10.4, data = d.pred)
pred.p <- apply(link.m10.4, 2, mean)
pred.p.PI <- apply(link.m10.4, 2, PI)

plot(0,0,type="n", xlab="prosoc_left/condition", ylab="proportion pulled left",
     ylim=c(0,1), xaxt="n", xlim=c(1,4), yaxp=c(0,1,2))
axis(1,at=1:4, labels = c("0/0","1/0", "0/1","1/1"))
mtext(paste("actor", chimp))
p <- by(d$pulled_left, list(d$prosoc_left, d$condition, d$actor), mean)
lines(1:4, as.vector(p[,,chimp]), col=rangi2, lwd = 2)
lines(1:4, pred.p)
shade(pred.p.PI, 1:4)

## 10.1.2 Aggregated binomial: Chimpanzees again, condensed
data(chimpanzees)
d <- chimpanzees
d.aggregated <- aggregate(d$pulled_left, list(prosoc_left=d$prosoc_left, 
                                              condition=d$condition, actor=d$actor), sum)
head(d.aggregated)
m10.5 <- map(
  alist(
    x ~ dbinom(18, p),
    logit(p) <- a + (bp + bpC*condition)*prosoc_left,
    c(a, bp, bpC) ~ dnorm(0,10)
  ), data = d.aggregated
)


## 10.1.3 Aggregated binomial: graduate school admissions
library(rethinking)
data(UCBadmit)
d <- UCBadmit
d

d$male <- ifelse(d$applicant.gender == "male",1,0)
m10.6 <- map(
  alist(
    admit ~ dbinom(applications, p),
    logit(p) <- a + bM*male,
    a ~ dnorm(0,10),
    bM ~ dnorm(0,10)
  ), data = d
)

m10.7 <- map(
  alist(
    admit ~ dbinom(applications, p),
    logit(p) <- a,
    a ~ dnorm(0,10)
  ), data = d
)
compare(m10.6, m10.7)
precis(m10.6)

post <- extract.samples(m10.6)
p.admit.male <- logistic(post$a + post$bM)
p.admit.female <- logistic(post$a)
diff.admit <- p.admit.male - p.admit.female
quantile(diff.admit, c(.025, .5, .975))
dens(diff.admit)

postcheck(m10.6, n=1e4)

# make index
d$dept_id <- coerce_index(d$dept)
m10.8 <- map(
  alist(
    admit ~ dbinom(applications, p),
    logit(p) <- a[dept_id],
    a[dept_id] ~ dnorm(0,10)
  ), data = d
)

m10.9 <- map(
  alist(
    admit ~ dbinom(applications, p),
    logit(p) <- a[dept_id] + bM*male,
    a[dept_id] ~ dnorm(0,10),
    bM ~ dnorm(0,10)
  ), data = d
)
compare(m10.6, m10.7, m10.8, m10.9)
precis(m10.9, depth = 2)

## 10.1.4 Fitting binomial regressions
m10.7glm <- glm(cbind(admit, reject)~1, data = d, family = binomial)
m10.6glm <- glm(cbind(admit, reject)~male, data = d, family = binomial)
m10.8glm <- glm(cbind(admit, reject)~dept, data = d, family = binomial)
m10.9glm <- glm(cbind(admit, reject)~male +dept, data = d, family = binomial)

## when outcome coded as 0/1 input looks like linear regression
names(chimpanzees)
## note us of `-` to remove as main effect
m10.4glm <- glm(
  pulled_left ~ as.factor(actor) + prosoc_left * condition - condition,
  data = chimpanzees, family = "binomial"
)

## use glimmer to extract MAP style from GLM
glimmer(pulled_left ~ prosoc_left * condition - condition,
        data = chimpanzees, family = binomial)



y <- c(rep(0,10), rep(1,10))
x <- c(rep(-1,9), rep(1,11))
cor(x,y) ## strong correlation
## 0.9045

df <- data.frame(y,x)
fit <- glm(y ~ x, data = df, family=binomial)
rethinking::precis(fit)
##              Mean  StdDev     5.5%   94.5%
## (Intercept) -9.13 2955.06 -4731.89 4713.63
## x           11.43 2955.06 -4711.33 4734.19

## What's going on with those confidence intervals? ^^

## From "Rethinking Statistics" - the outcome is so strongly associated
## with the predictor that the slope on x tried to grow very large. At large
## log-odds almost any value is just as good as any other. So the uncertainty
## is asymmetric and the flat prior does nothing to cakn inference down on the
## high end of it. Glm does nothing to warn us of this behavior.

## using weakly informative priors fixes the problem
new.fit <- map(
  alist(
    y ~ dbinom(1, p),
    logit(p) <- a + b*x,
    c(a,b) ~ dnorm(0,10)
  ), data = df
)
precis(new.fit)
##   Mean StdDev  5.5% 94.5%
## a -1.73   2.78 -6.16  2.71
## b  4.02   2.78 -0.42  8.45

## 10.2 Poisson regression
## when a binomial distribution has a very small probability of an event p and a very
## large number of trials n, then it takes on a special shape. The expected value
## of a binomial dist is np and its variance is np(1-p). But when n is very large and p
## is very small these are approximately the same.


## 10.2.1 Oceanic tool complexity
library(rethinking)
data(Kline)
d <- Kline
head(d)

d$log_pop <- log(d$population)
d$high_contact <- ifelse(d$contact == "high",1,0)
names(d)
m10.10 <- map(
  alist(
    total_tools ~ dpois(lambda),
    log(lambda) <- a + bp*log_pop + bc*high_contact + bpc*log_pop*high_contact,
    a ~ dnorm(0,100),
    c(bp, bc, bpc) ~ dnorm(0,1)
  ), data = d
)

precis(m10.10, corr = T)
## initial analysis suggests a main effect of population, but no
## main or interaction effect with high_contact... but this is wrong!!
## compute some counterfactual predictions: consider two islands woth with
## log_pop == 8, but one with high_contact and one with low_contact. Compute
## lambda (the expected tool count for each)

post <- extract.samples(m10.10)
## log(lambda) <- a + bp*log_pop + bc*high_contact + bpc*log_pop*high_contact
lambda_high <- exp(post$a + post$bc + (post$bp + post$bpc)*8)
lambda_low <- exp(post$a + post$bp*8)

diff <- lambda_high - lambda_low
sum(diff > 0)/length(diff)
## ^^ there is a 95% plausability that high_contact islands have more tools

## no interaction
m10.11 <- map(
  alist(
    total_tools ~ dpois(lambda),
    log(lambda) <- a + bp*log_pop + bc*high_contact,
    a ~ dnorm(0,100),
    c(bp, bc) ~ dnorm(0,1)
  ), data = d
)

## no contact rate
m10.12 <- map(
  alist(
    total_tools ~ dpois(lambda),
    log(lambda) <- a + bp*log_pop,
    a ~ dnorm(0,100),
    c(bp) ~ dnorm(0,1)
  ), data = d
)

## no log-population
m10.13 <- map(
  alist(
    total_tools ~ dpois(lambda),
    log(lambda) <- a + bc*high_contact,
    a ~ dnorm(0,100),
    c(bc) ~ dnorm(0,1)
  ), data = d
)

## null model
## intecept only model

m10.14 <- map(
  alist(
    total_tools ~ dpois(lambda),
    log(lambda) <- a,
    a ~ dnorm(0,100)
  ), data = d
)

(islands.compare <- compare(m10.10, m10.11, m10.12, m10.13, m10.14, n = 1e4))
plot(islands.compare)
methods(plot)

## to get a better sense of what these models are doing, plot couonterfactual predictions

pch <- ifelse(d$high_contact == 1, 'H', 'L')
plot(d$log_pop, d$total_tools, col=rangi2, pch=pch, xlab="log-population", ylab="total tools")
## sequence of log-pop sizes to compute over
log_pop.s <- seq(from = 6, to = 13, length.out = 30)
## compute trend from high contact islands
d.pred <- data.frame(log_pop = log_pop.s, high_contact = 1)

lambda.pred.h <- ensemble(m10.10, m10.11, m10.12, data = d.pred)
lambda.med <- apply(lambda.pred.h$link, 2, median)
lambda.PI <- apply(lambda.pred.h$link, 2, PI)

lines(log_pop.s, lambda.med, col=rangi2)
shade(lambda.PI, log_pop.s, col=col.alpha(rangi2,0.2))


## compute trend from low contact islands
d.pred <- data.frame(log_pop = log_pop.s, high_contact = 0)

lambda.pred.l <- ensemble(m10.10, m10.11, m10.12, data = d.pred)
lambda.med <- apply(lambda.pred.l$link, 2, median)
lambda.PI <- apply(lambda.pred.l$link, 2, PI)

lines(log_pop.s, lambda.med, col=rangi2, lty=2)
shade(lambda.PI, log_pop.s, col=col.alpha("black",0.2))

## 10.2.2 MCMC islands
m10.10stan <- map2stan(m10.10, iter = 3000, warmup = 1000, chains = 4)
precis(m10.10stan)
pairs(m10.10stan)
## HMC is going to be less effective when there are strong correlations
## in the posterior dist

## centering predictors aids in inference
names(d)
d$log_pop.c <- d$log_pop - mean(d$log_pop)
m10.10.c <- map2stan(
  alist(
    total_tools ~ dpois(lambda),
    log(lambda) <- a + bp*log_pop.c + bc*high_contact + bcp*log_pop.c*high_contact,
    a ~ dnorm(0,10),
    c(bp, bc, bcp) ~ dnorm(0,1)
  ), data = d, iter = 3000, warmup = 1000, chains = 4
)
precis(m10.10.c)
pairs(m10.10.c)

## 10.2.3 Exposure and offset
## true lambda = 1.5 manuscripts/day
true_lambda <- 1.5
num_days <- 30
y <- rpois(num_days, true_lambda)
y

## true rate of new monastery lambda = 0.5
## data only reported at weekly intervals
new_true_lambda <- 0.5
num_weeks <- 4
y_new <- rpois(num_weeks, new_true_lambda*7)
y_new

y_all <- c(y,y_new)
exposure <- c(rep(1,30), rep(7,4))
monastery <- c(rep(0,30),rep(1,4))
d <- data.frame(y=y_all, days = exposure, monastery = monastery)
head(d)
tail(d)

d$log_days <- log(d$days)

m10.15 <- map(
  alist(
    y ~ dpois(lambda),
    log(lambda) <- log_days + a + b*monastery,
    a ~ dnorm(0,100),
    b ~ dnorm(0,1)
  ), data = d
) 
precis(m10.15)

post <- extract.samples(m10.15)
lambda_old <- exp(post$a)
lambda_new <- exp(post$a + post$b)
precis(data.frame(lambda_old, lambda_new))

## 10.3 Other count regressions
## 10.3.1 Multinomial
## when more than two types of events are possible and the probability of
## each event is constant acrosss trials, then the maximum entropy distribution
## is the MULTINOMIAL DISTRIBUTION (aka a model build on multinomial dist is also
## called categorical regression or a MAX ENT CLASSIFIER.

## 10.3.1.1 Explicit multinomial models
## modeling career choice
## simulate fake career choices

N <- 500
income <- 1:3
score <- 0.5*income
## convert scores to probabilities
(p <- softmax(score[1], score[2], score[3]))
score

career <- rep(NA, N)
for (i in 1:N) {
  career[i] <- sample(1:3, size = 1, prob = p)
}
table(career)

# fit model w dcategorical and softmax link
m10.16 <- map(
  alist(
    career ~ dcategorical(softmax(0,s2,s3)),
    s2 <- b*2,
    s3 <- b*3,
    b ~ dnorm(0,5)
  ), data = list(career = career)
)

## BEWARE - estimates from these models are difficult to interpret - you
## must convert them to a vector of probabilities b/c the estimates swing
## abouot dependeing upon which event type you assign a constant score

N <- 100
## simulate family incomes for each indiv
family_income <- runif(N)
## assign unique coef for each event type
b <- (-1:1)
career <- rep(NA, N)
for (i in 1:N) {
  score <- 0.5*(1:3) + b*family_income[i]
  p <- softmax(score[1], score[2], score[3])
  career[i] <- sample(1:3, size = 1, prob = p)
}

m10.17 <- map(
  alist(
    career ~ dcategorical(softmax(0,s2,s3)),
    s2 <- a2 + b2*family_income,
    s3 <- a3 + b3*family_income,
    c(a2,a3,b2,b3) ~ dnorm(0,5)
  ), data = list(career=career, family_income=family_income)
)
precis(m10.17)

## 10.3.1.2 Multinomial in disguise as Poisson
## refactor a multinomial likelihood into series of Poisson likelihoods


## 10.3.2 Geometric - when a count variable is a number of events up until
## something happened (i.e. the terminating event) - what's the probability
## of that event? Use EVENT HISTORY ANALYSIS or SURVIVAL ANALYSIS.




