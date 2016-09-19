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
  x.seq <- seq(from=floor(r[1]), to=ceiling(r[2]), length.out = 30)

  pred_dat <- list()  
  pred_dat[[x]] <- x.seq

  ## create predictions for new data
  mu <- link(map_model, data = pred_dat)
  mu.mean <- apply(mu, 2, mean)
  mu.PI <- apply(mu, 2, PI, prob = 0.89)

  ## plot(log_gdp ~ rugged, d, col=col.alpha(rangi2, .5))
  plot(log_gdp ~ rugged, df, col="red")
  lines(x.seq, mu.mean)
  shade(mu.PI, x.seq)
}

x <- "rugged"
df <- d.A0
map_model <- m7.2
plotData(d.A0, m7.2, x)

x <- "rugged"
df <- d.A1
map_model <- m7.1
plotData(d.A1, m7.1, x)

m7.3 <- map(
  alist(
    log_gdp ~ dnorm(mu, sigma),
    mu <- a + bR*rugged,
    a ~ dnorm(8,100),
    bR ~ dnorm(0,1),
    sigma ~ dunif(0,10)
  ), data = dd)

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

rugged_range <- range(dd$rugged)
rug_start <- floor(rugged_range[1])-1
rug_stop <- ceiling(rugged_range[2])+1

rugged.seq <- seq(from=rug_start, to=rug_stop, by=0.25)
mu.NotAfrica <- link(m7.4, data = data.frame(cont_africa=0,rugged=rugged.seq))
mu.Africa <- link(m7.4, data = data.frame(cont_africa=1,rugged=rugged.seq))

mu.NotAfrica.mean <- apply(mu.NotAfrica, 2, mean)
mu.NotAfrica.PI <- apply(mu.NotAfrica, 2, PI)
mu.Africa.mean <- apply(mu.Africa, 2, mean)
mu.Africa.PI <- apply(mu.Africa, 2, PI)


TCSevaluationModel <- rethinking::map(
  alist(
    perf_eval ~ dnorm(mu, sigma),
    mu <- a + bYrs*yearsAtTCS + bM*hasMustache + bQ*threatenQuit + 
      bB*benchMonths * bSH*squareHead *bW*actuallyDoesWork,
    a ~ dnorm(50, 10),
    c(bYrs, bM, bQ, bB, bSH) ~ dnorm(5,1),
    bW ~ dnorm(-10,1),
    sigma ~ dcauchy(0,2)
  ), data = read.csv(someShittySpreadsheet.csv)
)

n <- 1e4
lambda <- 1000

.16*.21/.36


data(Howell1)
df <- Howell1
head(df)


