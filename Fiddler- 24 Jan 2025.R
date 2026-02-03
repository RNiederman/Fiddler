# https://thefiddler.substack.com/p/can-you-hop-to-the-lily-pad

library(pbapply)


starting_pad <- 2
moves <- c(-1, 1)


Froggy_Fiddler <- function(pads=4) {
  k <- starting_pad
  enders <- c(1, pads)  
  
  
  while (all(k != enders) ) {
    odds <- c(1/k, (k - 1)/k )
    hop <- sample(x = moves, size = 1, prob = odds)
    k <- k + hop
  }

  win <- k == 1
  return(win)
}


trials <- 10^7

mc_4 <- pbreplicate(trials, Froggy_Fiddler(4) )
win_perc_4 <- sum(mc_4)/trials

# plotter <- cumsum(mc_4) / 1:trials
# plot(plotter)
# rm(plotter)

rm(mc_4, trials)
rm(starting_pad, moves)


sprintf("%1.1f%%", win_perc_4 * 100)
# 60%

# https://thefiddler.substack.com/p/can-you-spin-the-graph

