# https://thefiddler.substack.com/p/can-you-win-at-non-traditional-blackjack

library(pbapply)


fiddler_blackjack_xc <- function(xc_deck) {
  deck <- sample(xc_deck)
  draw <- deck[1:2] # draw two cards since you can draw 19 max
  deck <- deck[-1:-2]
  
  keep_drawing <- TRUE
  
  while (keep_drawing) {
    delta <- 21 - sum(draw)
    max_card <- max(deck)
    keep_drawing <- delta >= max_card
    if (keep_drawing) {
      draw <- c(draw, deck[1])
      deck <- deck[-1]
    }
      
  }
  winner_xc <- sum(draw) == 21
  return(winner_xc)
    
}


card_deck2 <- 1:10
trials2 <- 10^7

mc2 <- pbreplicate(trials2, fiddler_blackjack_xc(card_deck2))

win_perc2 <- sum(mc2)/trials2
sprintf("%1.2f%%", win_perc2*100)
# 2.06%

# Expected games played is 1/win_percentage
expected_games <- 1/win_perc2
sprintf("%1.2f", expected_games)
# 48.52