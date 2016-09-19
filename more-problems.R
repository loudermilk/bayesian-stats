W <- 1
L <- 0
obs <- list()
obs[[1]] <- c(W, W, W)
obs[[2]] <- c(W, W, W, L)
obs[[3]] <- c(L, W, W, L, W, W, W)

plotObs <- function(x, 
                    size, 
                    p_grid = seq(from = 0, to = 1, length.out = 100), 
                    prior = rep(1,length(p_grid))){
  likelihood <- dbinom(x, size = size, prob = p_grid)
  unstd.posterior <- likelihood * prior
  posterior <- unstd.posterior/sum(unstd.posterior)
  max.pt <- which.max(posterior)
  plot(p_grid, posterior)
  abline(v = p_grid[max.pt], col = "red")
}


plotObs(3,3 )

p_grid <- seq(from = 0, to = 1, length.out = 100)
prior_2 <- prior_flat <- rep(1,length(p_grid))
prior_2[p_grid < 0.5] <- 0
plotObs(3,3,p_grid,prior_2 )



