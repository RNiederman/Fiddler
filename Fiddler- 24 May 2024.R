# https://thefiddler.substack.com/p/can-you-win-at-non-traditional-blackjack

library(pbapply)


fiddler_blackjack <- function(deck) {

  shuffled_deck <- sample(deck)
  csums <- cumsum(shuffled_deck)
  winner <- any(csums == 21)

  return(winner)
}


card_deck <- 1:10
trials <- 10^7

mc <- pbreplicate(trials, fiddler_blackjack(card_deck))

win_perc <- sum(mc)/trials
sprintf("%1.2f%%", win_perc*100)
# 17.50%

# https://thefiddler.substack.com/p/can-you-ace-the-technical-interview