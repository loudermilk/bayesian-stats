## sr-ch8.R
## Chapter 8 - Markov Chain Monte Carlo

## 8.1 Good King Markov and His Island Kingdom

num_weeks <- 1e5
current <- 10
positions <- rep(0, num_weeks)
for (i in 1:num_weeks) {
  positions[i] <- current
  proposal <- current + sample(c(-1,1),size=1)
  if (proposal < 1) proposal <- 10
  if (proposal > 10) proposal <- 1
  prob_move <- proposal/current
  current <- ifelse(runif(1) < prob_move,proposal, current)
}
