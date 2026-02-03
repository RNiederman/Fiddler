# https://thefiddler.substack.com/p/can-you-sweep-the-series

library(dplyr)
library(scales)

# series_len <- 7
# p <- 63 / 100


BB_Series <- function(p, series_len=7) {
  
  mc <- rbinom(n = series_len, size = 1, prob = p)
  win <- sum(mc) >= 4
  
  if (!win) {mc <- 1 - mc}
  runner <- cumsum(mc)
  games <- which(runner == 4)[1]

  return(c(games, win) )
}


GAMES_2 <- 7

A <- 0.625
B <- 0.75

TRIALS_2 <- 10^7
BURPER_2 <- 10^5
 
win_register <- game_register <- rep(0, 7)
key <- 1:7

for (j in 1:TRIALS_2) {

  p_2 <- runif(1, A, B)
  mc_2 <- BB_Series(p_2, GAMES_2)

  k <- mc_2[1] # The number of games in the series
  w <- mc_2[2] # Did we win the series? 1 = win, 0 = loss
  
  game_register[k] <- game_register[k] + 1
  win_register[k] <- win_register[k] + w

  if (j %% BURPER_2 == 0) {
    paste(comma(j), comma(TRIALS_2), sep = " / ") %>% 
      print
  }
}

rm(p_2, mc_2)
rm(j, k, w)

rm(GAMES_2)
rm(A, B)
rm(BURPER_2)

#### ####
# We Sweep
p4 <- win_register[4] / TRIALS_2
# A seven-game series, regardlesss of winner
p7 <- game_register[7] / TRIALS_2

sprintf("%1.1f%%", p4 * 100)
sprintf("%1.1f%%", p7 * 100)
# 22.7% -> p4
# 19.7% -> p7
#### ####

barplot(game_register, names.arg = key)
# barplot(win_register, names.arg = key)
rm(key)

win_perc <- sum(win_register) / TRIALS_2
win_4_perc <- win_register[4] / game_register[4]

# sprintf("%1.1f%%", win_perc * 100)
# sprintf("%1.1f%%", win_4_perc * 100)
