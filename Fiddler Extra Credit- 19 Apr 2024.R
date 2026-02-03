# https://thefiddler.substack.com/p/can-you-win-the-collaborative-card

# options(scipen = 999)
library(dplyr)
library(scales)


Fiddler_Card_Matching_Game_XC <- function(n_cards=52, n_decks=2) {
  require(dplyr)

  deck <- 1:n_cards %>% rep(n_decks) %>% sample
  L <- length(deck)

  odd_index  <- seq(from = 1, to = L, by = 2)
  even_index <- seq(from = 2, to = L, by = 2)
  
  them <- deck[odd_index]
  me   <- deck[even_index]
  
  win <- all(me != them)
  
  return(win)
}


trials <- 10^7
burper <- 50000
wins <- 0
denom <- comma(trials)

cards <- 13 * 4
decks <- 2

total_cards <- cards * decks
stopifnot(total_cards %% 2 == 0)

for (j in (1:trials)) {
  temp_game <-  Fiddler_Card_Matching_Game_XC(cards, decks) 
  wins <- wins + temp_game 
  if (j %% burper == 0) {
    paste(comma(j), denom, sep = " / " ) %>% print
  }
}

win_perc <- wins/trials
sprintf("%1.2f%%", win_perc*100)
# 60.35%

rm(j, temp_game, denom)
# https://thefiddler.substack.com/p/can-you-play-the-price-is-right-continuously