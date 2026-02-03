# https://thefiddler.substack.com/p/can-you-win-the-collaborative-card

# options(scipen = 999)
library(scales)


Fiddler_Card_Matching_Game <- function(n_cards=52) {
  deck <- 1:n_cards

  me <- sample(deck)
  them <- sample(deck)

  win <- all(me != them)
  return(win)
}


trials <- 10^6
burper <- 25000
wins_52 <- 0
big_wins <- 0

denom <- comma(trials)


for (i in (1:trials)) {
  game_52 <- Fiddler_Card_Matching_Game(52) 
  big_game <- Fiddler_Card_Matching_Game(9999)
  
  wins_52 <- wins_52 + game_52
  big_wins <- big_wins + big_game

  if (i %% burper == 0) {
    update_line <- paste(comma(i), denom, sep = " / " )
    print(update_line)
  }
}

win_perc_52 <- wins_52 / trials
big_win_perc <-  big_wins / trials

sprintf("%1.3f%%", win_perc_52 * 100)
sprintf("%1.3f%%", big_win_perc * 100)
sprintf("%1.3f%%", 100 / exp(1))

# 1/e, or about 36.788 percent (for an infinitely large deck)
# https://thefiddler.substack.com/p/can-you-play-the-price-is-right-continuously

rm(i, game_52, big_game)
rm(denom, burper)