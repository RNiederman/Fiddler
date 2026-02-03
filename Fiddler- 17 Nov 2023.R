# https://thefiddler.substack.com/p/can-you-flip-the-coins-exactly-as

library(pbapply)

expected <- c(1, 3, 3, 1) # Expected Numbers of Heads- 0, 1, 2, 3
bins <- 0:3

Fiddler.Coin.Toss <- function(coins = 3, trials = 8) {

  flips <- rbinom(trials, coins, 1/2)
  factored_flips <- factor(flips, bins)
  tab <- tabulate(factored_flips, nbins = 4)
  tester <- all(tab == expected)

  return(tester)
}

n <- 10^7
mc <- pbreplicate(n, Fiddler.Coin.Toss(3, 8))
soln <- mean(mc)
sprintf("%1.3f%%", soln*100)

rm(mc)


# 4.8666 percent
# https://thefiddler.substack.com/p/can-you-race-around-the-monopoly