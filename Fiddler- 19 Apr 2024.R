# https://thefiddler.substack.com/p/can-you-win-the-collaborative-card

options(scipen = 999)

numbs <- c(2:10, "J", "Q", "K", "A") 
suits <- c("S", "D", "C", "H")

card <- rep(numbs, times = length(suits))
suit <- rep(suits, each = length(numbs))
deck <- paste(card, suit, sep = "|")


Fiddler_Card_Matching_Game <- function() {

  me <- sample(deck)
  them <- sample(deck)

  matches <- me == them
  lose <- any(matches)
  win <- !lose

  return(win)
}


trials <- 10^7
burper <- 250000
wins <- 0

for (i in (1:trials)) {
  wins <- wins + Fiddler_Card_Matching_Game() 
  if (i %% burper == 0) {
    update_line <- paste(i, trials, sep = " / " )
    print(update_line)
  }
}

win_perc <- wins/trials
sprintf("%1.3f%%", win_perc*100)

# 36.784%
# 1/e, or about 36.788 percent (for an infinitely large deck)

# https://thefiddler.substack.com/p/can-you-play-the-price-is-right-continuously