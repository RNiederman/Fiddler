# https://thefiddler.substack.com/p/can-you-tip-the-dominoes

library(pbapply)

Domino_Fiddler <- function() {
  prob <- 1/100
  d <- 0
  tipped <- FALSE
  while (!tipped) {
    d <- d + 1
    rando <- runif(1)
    tipped <- rando <= prob
  }
  return(d)
}


trials <- 10^7

mc <- pbreplicate(trials, Domino_Fiddler())

soln <- median(mc)
print(soln)
# 69

hist(mc)
mean(mc)
# ~99.9

# https://thefiddler.substack.com/p/a-pi-day-puzzle