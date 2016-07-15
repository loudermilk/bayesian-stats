## sr-ch7.R

library(rethinking)
data(rugged)
d <- rugged
head(d)

# make log of outome
d$log_gdp <- log(d$rgdppc_2000)
dd <- d[complete.cases(d$rgdppc_2000),]

# split to africa and non
d.A1 <- dd[dd$cont_africa==1,]
d.A0 <- dd[dd$cont_africa==0,]

# african nations
m7.1 <- map(
  alist(
    log_gdp ~ dnorm(mu, sigma),
    mu <- a + bR*rugged,
    a ~ dnorm(8,100),
    bR ~ dnorm(0,1),
    sigma ~ dunif(0,10)
  ), data = d.A1
)

# non-african nations
m7.2 <- map(
  alist(
    log_gdp ~ dnorm(mu, sigma),
    mu <- a + bR*rugged,
    a ~ dnorm(8,100),
    bR ~ dnorm(0,1),
    sigma ~ dunif(0,10)
  ), data = d.A0
)

range(d.A1$rugged)



plotData <- function(df, map_model, x){
  r <- range(df[[x]])

  x.seq <- seq(from=r[1], to=r[2], length.out = 30)

  pred_dat <- list()  
  pred_dat[[x]] <- x.seq

  mu <- link(map_model, data = pred_dat)
  mu.mean <- apply(mu, 2, mean)
  mu.PI <- apply(mu, 2, PI, prob = 0.89)

  plot(log_gdp ~ rugged, d, col=col.alpha(rangi2, .5))
  lines(x.seq, mu.mean)
  shade(mu.PI, x.seq)
}

x <- "rugged"
plotData(d.A0, m7.2, x)


m7.3 <- map(
  alist(
    log_gdp ~ dnorm(mu, sigma),
    mu <- a + bR*rugged,
    a ~ dnorm(8,100),
    bR ~ dnorm(0,1),
    sigma ~ dunif(0,10)
  ), data = dd
)

plotData(df = dd, map_model = m7.3, x = "rugged")

m7.4 <- map(
  alist(
    log_gdp ~ dnorm(mu, sigma),
    mu <- a + bR*rugged + bA*cont_africa,
    a ~ dnorm(8,100),
    bR ~ dnorm(0,1),
    bA ~ dnorm(0,1),
    sigma ~ dunif(0,10)
  ), data = dd
)

compare(m7.3, m7.4)


rugged.seq <- seq(from=-1, to=8, by=0.25)
mu.NotAfrica <- link(m7.4, data = data.frame(cont_africa=0,rugged=rugged.seq))
mu.Africa <- link(m7.4, data = data.frame(cont_africa=1,rugged=rugged.seq))

mu.NotAfrica.mean <- apply(mu.NotAfrica, 2, mean)
mu.NotAfrica.PI <- apply(mu.NotAfrica, 2, PI)
mu.Africa.mean <- apply(mu.Africa, 2, mean)
mu.Africa.PI <- apply(mu.Africa, 2, PI)