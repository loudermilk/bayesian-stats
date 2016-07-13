## sr-ch5.R
## Chapter 5 - Multivariate Linear Models

library(rethinking)
data(WaffleDivorce)
d <- WaffleDivorce
head(d)
names(d)

# standardize
d$MedianAgeMarriage.s <- (d$MedianAgeMarriage - mean(d$MedianAgeMarriage))/sd(d$MedianAgeMarriage)

# fit
m5.1 <- map(alist(
  Divorce ~ dnorm(mu, sigma),
  mu <- a + bA * MedianAgeMarriage.s,
  a ~ dnorm(10,10),
  bA ~ dnorm(0, 1),
  sigma ~ dunif(0, 10)
), data = d)

MAM.seq <- seq(from = -3, to = 3.5, length.out = 30)
mu <- link(m5.1, data=data.frame(MedianAgeMarriage.s = MAM.seq))
mu.PI <- apply(mu, 2, PI)
plot(Divorce~MedianAgeMarriage.s, data = d, col = rangi2)
abline(m5.1)
shade(mu.PI, MAM.seq)

precis(m5.1)

View(head(d))
d$Marriage.s <- (d$Marriage - mean(d$Marriage))/sd(d$Marriage)
m5.2 <- map(
  alist(
    Divorce ~ dnorm(mu, sigma),
    mu <- a + bR * Marriage.s,
    a ~ dnorm(10, 10),
    bR ~ dnorm(0,1),
    sigma ~ dunif(0,10)
  ),
  data = d
)

## What is the predictive value of a variable once I already know all the
## other predictor variables?

## (1) after I already know marriage rate, what additional value is there in
##     also knowing age at marriage?
## (2) after I already know age at marriage, what additional value is there in
##     also knowing marriage rate?

## 5.1.1 Multivariate notation
## strategy:
## (1) nominate the predcitor variables you want in the linear model of the mean.
## (2) for each predictor make a parameter that will measure its association
## with the outcome.
## (3) multiply the parameter by the variable and add that term to the linear model.


## 5.1.2 Fitting the model
##
## D_i ~ Normal(mu_i, sigma)            ## Divorce ~ dnorm(mu, sigma)
## mu_i = a + b_R * R_i + b_A * A_i     ## mu <- a + bR*Marriage.s + bA*MedianAgeMarriage.s
## a ~ Normal(10,10)                    ## a ~ dnorm(10,10)
## b_R ~ Normal(0,1)                    ## bR ~ dnorm(0,1)
## b_A ~ Normal(0,1)                    ## bA ~ dnorm(0,1)
## sigma ~ Uniform(0,10)                ## sigma ~ dunif(0,10)

m5.3 <- map(
  alist(
    Divorce ~ dnorm(mu, sigma),
    mu <- a + bR*Marriage.s + bA*MedianAgeMarriage.s,
    a ~ dnorm(10,10),
    bR ~ dnorm(0,1),
    bA ~ dnorm(0,1),
    sigma ~ dunif(0,10)
  ),
  data = d
)
precis(m5.3)

## once we know median age at marriage for a state there is no additional
## predictive power in also knowing marriate rate for that state.

## 5.1.3 Plotting multivariate posteriors
## (1) predictor residual plots - show outcome against residual predictor values
## (2) counterfactual plots - show implied predictions for immaginary experiments
## in which the different predictor variables can be changed independently.
## (3) posterior prediction plots - show model-based predictions against raw
## data or otherwise display the error in the prediction.

## 5.1.3.1 Predictor residual plots
## A predictor variable residual is the average prediction error when we use all 
## the other predictor variables to model a predictor of interest.

## in our multivariate model of divorce rate we have two predictors:
## Marriage.s and MedianAgeMarriage.s. To compute predictor residuals for either
## we just use the other predictor to model it.

## R_i ~ Normal(mu_i, sigma)
## mu_i = a +b*A_i
## a ~ Normal(0,10)
## b ~ Normal(0,1)
## sigma ~ Uniform(0,10)

m5.4 <- map(
  alist(
    Marriage.s ~ dnorm(mu, sigma),
    mu <- a + b*MedianAgeMarriage.s,
    a ~ dnorm(0,10),
    b ~ dnorm(0,1),
    sigma ~ dunif(0,10)
  ), data = d
)

## compute the residuals by subtracting the observed marriage rate in each
## state from the predicted rate based on age of marriage

## compute expected value at MAP for each state:
mu <- coef(m5.4)['a'] + coef(m5.4)['b']*d$MedianAgeMarriage.s
## compute residuals for each state
m.resid <- d$Marriage.s - mu

plot(Marriage.s ~ MedianAgeMarriage.s, d, col = rangi2)
abline(m5.4)
for (i in 1:length(m.resid)) {
  x <- d$MedianAgeMarriage.s[i]
  y <- d$Marriage.s[i]
  lines(c(x,x),c(mu[i],y),lwd = 0.5, col=col.alpha("black",0.7))
}

## 5.1.3.2 Counterfactual plots
## simplest use is to see how the predictions change as you change only
## one predictor at a time. Will help you understand the implications of
## your model.

## prepare new counterfactual data
A.avg <- mean(d$MedianAgeMarriage.s)
R.seq <- seq(from = -3, to = 3, length.out = 30)
pred.data <- data.frame(
  Marriage.s = R.seq,
  MedianAgeMarriate.s = A.avg
)

# compute counterfacual mean divorce
## DOESN'T WORK


## 5.1.3.3 Posterior prediction plots
## check the model fit against the observed data
## (1) did the model fit correctly?
## (2) how does the model fail?

## call link w/out specifying new data so it uses orig data
mu <- link(m5.3)
## summarize samples across cases
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)
## simulate observations
## again no new data so it uses orig data
divorce.sim <- sim(m5.3, n=1e4)
divorce.PI <- apply(divorce.sim, 2, PI)

plot(mu.mean ~ d$Divorce, col= rangi2, ylim = range(mu.PI),
     xlab="Observed Divorce", ylab="Predicted Divorce")
abline(a=0, b=1, lty = 2)
for (i in 1:nrow(d)) {
  lines(rep(d$Divorce[i],2),c(mu.PI[1,i],mu.PI[2,i]),col=rangi2)
}
# label a few select pts
identify(x= d$Divorce, y = mu.mean, labels=d$Loc, cex=0.8)

## compute residuals
divorce.resid <- d$Divorce - mu.mean
## get ordering by divorce rate
o <- order(divorce.resid)
## make plot
dotchart(divorce.resid[o], labels=d$Loc[o], xlim=c(-6,5), cex=0.6)
abline(v=0, col=col.alpha("red",0.5))
for (i in 1:nrow(d)) {
  j <- o[i]
  lines(d$Divorce[j] - c(mu.PI[1,j], mu.PI[2,j]), rep(i,2))
  points(d$Divorce[j] - c(divorce.PI[1,j], divorce.PI[2,j]), rep(i,2), pch=3, cex=0.6,col="grey")
}


plot(divorce.resid, d$WaffleHouses/d$Population)

## 5.2 Masked Relationship
library(rethinking)
data(milk)
d <- milk
str(d)

## to what extent is energy content of milk related to percent neocortex brain mass
m5.5 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bn*neocortex.perc,
    a ~ dnorm(0,100),
    bn ~ dnorm(0,1),
    sigma ~ dunif(0,1)
  ), data = d
)

## strange error msg need to investigate (discusses in book)
d$neocortex.perc

dcc <- d[complete.cases(d),]
m5.5 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bn*neocortex.perc,
    a ~ dnorm(0,100),
    bn ~ dnorm(0,1),
    sigma ~ dunif(0,1)
  ), data = dcc
)

precis(m5.5, digits = 3)
coef(m5.5)["bn"]*(76-55)

np.seq <- 0:100
pred.data <- data.frame(neocortex.perc=np.seq)
mu <- link(m5.5, data = pred.data, n= 1e4)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

plot(kcal.per.g ~ neocortex.perc, data = dcc, col = rangi2, ylim=c(0,1))
lines(np.seq, mu.mean)
lines(np.seq, mu.PI[1,], lty = 2)
lines(np.seq, mu.PI[2,], lty = 2)

## try log(mass of mother)
dcc$log.mass <- log(dcc$mass)

m5.6 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bm*log.mass,
    a ~ dnorm(0,100),
    bm ~ dnorm(0,1),
    sigma ~ dunif(0,1)
  ), data = dcc
)
precis(m5.6)


np.seq <- 0:100
pred.data <- data.frame(log.mass=np.seq)
mu <- link(m5.6, data = pred.data, n= 1e4)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

plot(kcal.per.g ~ log.mass, data = dcc, col = rangi2)
lines(np.seq, mu.mean)
lines(np.seq, mu.PI[1,], lty = 2)
lines(np.seq, mu.PI[2,], lty = 2)


## Add both as predictors
m5.7 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bn*neocortex.perc + bm*log.mass,
    a ~ dnorm(0, 100),
    bn ~ dnorm(0, 1), 
    bm ~ dnorm(0,1),
    sigma ~ dunif(0,1)
  ), data = dcc
)

precis(m5.7)

mean.log.mass <- mean(log(dcc$mass))
np.seq <- 0:100
pred.data <- data.frame(
  neocortex.perc = np.seq,
  log.mass = mean.log.mass
)

mu <- link(m5.7, data = pred.data, n = 1e4)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

plot(kcal.per.g ~ neocortex.perc, data = dcc, type = "n")
lines(np.seq, mu.mean)
lines(np.seq, mu.PI[1,], lty = 2)
lines(np.seq, mu.PI[2,], lty = 2)

## try it for log(mass)
mean.neocortex.perc <- mean(dcc$neocortex.perc)
np.seq <- 0:100
pred.data <- data.frame(
  neocortex.perc = mean.neocortex.perc,
  log.mass = np.seq
)

mu <- link(m5.7, data = pred.data, n = 1e4)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

plot(kcal.per.g ~ log.mass, data = dcc, type = "n")
lines(np.seq, mu.mean)
lines(np.seq, mu.PI[1,], lty = 2)
lines(np.seq, mu.PI[2,], lty = 2)

## 5.3 When adding variables hurts
## (1) multicolinearity - very strong correlation bw 2+ predictor variables - 
## posterior distribution will say that a very large range of parameter values
## are plausible 
## (2) post-treatment bias
## (3) over-fitting

## 5.3.1 Mulicollinear legs (simulated data: predict height from leg lengths)
N <- 100
height <- rnorm(N, 10, 2)
leg_prop <- runif(N, 0.4, 0.5)
leg_left <- leg_prop * height + rnorm(N, 0, 0.02)
leg_right <- leg_prop * height + rnorm(N, 0, 0.02)
d <- data.frame(height, leg_left, leg_right)


m5.8 <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + bl * leg_left + br * leg_right,
    a ~ dnorm(10,100),
    bl ~ dnorm(2,10),
    br ~ dnorm(2,10),
    sigma ~ dunif(0, 10)
  ), data = d
)
precis(m5.8)

post <- extract.samples(m5.8)
plot(bl ~ br, post, col=col.alpha(rangi2, 0.2), pch=16)

sum_blbr <- post$bl + post$br
dens(sum_blbr, col=rangi2, lwd=2, xlab="sum of br and bl")

m5.9 <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + bl*leg_left,
    a ~ dnorm(10,100),
    bl ~ dnorm(2, 10),
    sigma ~ dunif(0, 10)
  ), data = d
)
precis(m5.9)

## when two predictor variables are strongly correlated, including both in a 
## model may lead to confusion.

## 5.3.2 Multicollinear milk
library(rethinking)
data(milk)
d <- milk

## model kcal as funcion of perc.fat and perc.lactose
## start with two bivariate models
names(d)
m5.10 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bF * perc.fat,
    a ~ dnorm(0.6, 10),
    bF ~ dnorm(0,1),
    sigma ~ dunif(0, 10)
  ), data = d
)
precis(m5.10)

m5.11 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bL * perc.lactose,
    a ~ dnorm(0.6, 10),
    bL ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ), data = d
)
precis(m5.11)

cor(d$perc.fat, d$perc.lactose)


m5.12 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bF * perc.fat + bL * perc.lactose,
    a ~ dnorm(0.6, 10),
    bF ~ dnorm(0,1),
    bL ~ dnorm(0,1),
    sigma ~ dunif(0, 10)
  ), data = d
)
precis(m5.12)
pairs(~kcal.per.g + perc.fat + perc.lactose, data = d, col = rangi2)

## 5.3.3 Post-treatment bias
## mistaken inferences that arise from omitting predictor variables. Called
## omitted variable bias (post-treatment bias is including variables)

## number of plants
N <- 100
## simulate intial heights
h0 <- rnorm(N, 10, 2)
## assign 50% to treatment
treatment <- rep(0:1, each = N/2)
## treatment groups have less fungus
fungus <- rbinom(N, size = 1, prob = 0.5 - treatment*0.4)
## fungus has less growth
h1 <- h0 + rnorm(N, 5 - 3*fungus)
d <- data.frame(h0=h0, h1=h1, treatment=treatment, fungus=fungus)
head(d)


m5.13 <- map(
  alist(
    h1 ~ dnorm(mu, sigma),
    mu <- a + bH*h0 + bT*treatment + bF*fungus,
    a ~ dnorm(0, 100),
    c(bH, bT, bF) ~ dnorm(0,10),
    sigma ~ dunif(0,10)
  ), data = d
)
precis(m5.13)

## problem is that fungus is a consequence of treatment
## need to remove treatment from stats

m5.14 <- map(
  alist(
    h1 ~ dnorm(mu, sigma),
    mu <- a + bH*h0 + bT*treatment,
    a ~ dnorm(0, 100),
    c(bH, bT) ~ dnorm(0,10),
    sigma ~ dunif(0,10)
  ), data = d
)
precis(m5.14)

## including post-treatment variables can causal influence of treatment

## 5.4 Categorical variables
## 5.4.1 Binary categories
## !Kung data incl sex as predictor

data(Howell1)
d <- Howell1
str(d)

## h_i ~ Normal(mu_i, sigma)
## mu_i = a +bM*m_i
## a ~ Normal(178, 100)
## bM ~ Normal(0, 10)
## sigma ~ Uniform(0, 50)

m5.15 <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + bM*male,
    a ~ dnorm(178, 100),
    bM ~ dnorm(0, 10),
    sigma ~ dunif(0, 50)
  ), data = d
)
precis(m5.15)

## a - average height among females b/c when m_i = 0 predicted height is a
## bM is the average difference between males and females

post <- extract.samples(m5.15)
mu.male <- post$a + post$bM
PI(mu.male)


## 5.4.2 Many categories
## to incl k categories in a linear model you need k-1 dummy variables

data(milk)
d <- milk
str(d)
unique(d$clade)

## create dummy for new world monkey
(d$clade.NWM <- ifelse(d$clade == "New World Monkey",1,0))
d$clade.OWM <- ifelse(d$clade == "Old World Monkey",1,0)
d$clade.S <- ifelse(d$clade == "Strepsirrhine",1,0)
## default is Ape -- "intercept" category

m5.16 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bNWM*clade.NWM + bOWM*clade.OWM + bS*clade.S,
    a ~ dnorm(0.6, 10),
    c(bNWM, bOWM, bS) ~ dnorm(0,1),
    sigma ~ dunif(0,10)
  ), data = d
)
precis(m5.16)

## get posterior distributions of the avg milk energy in each category
post <- extract.samples(m5.16)
mu.ape <- post$a
mu.nwm <- post$a + post$bNWM
mu.owm <- post$a + post$bOWM
mu.s <- post$a + post$bS

precis(data.frame(mu.ape, mu.nwm, mu.owm, mu.s))

diff.NWM.OWM <- mu.nwm - mu.owm
quantile(diff.NWM.OWM, probs = c(0.025, 0.5, 0.975))

