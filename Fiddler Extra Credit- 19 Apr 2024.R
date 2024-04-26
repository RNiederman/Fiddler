# https://thefiddler.substack.com/p/can-you-win-the-collaborative-card

options(scipen = 999)

numbs <- c(2:10, "J", "Q", "K", "A") 
suits <- c("S", "D", "C", "H")

card <- rep(numbs, times = length(suits))
suit <- rep(suits, each = length(numbs))
deck <- paste(card, suit, sep = "|")
double_deck <- rep(deck, each = 2)

L <- length(double_deck)
index_set <- 1:L


Fiddler_Card_Matching_Game_XC <- function() {

  shuffled_deck <- sample(double_deck)
  
  my_card_index <- sample(index_set, L/2)
  their_card_index <- setdiff(index_set, my_card_index)
  
  me <- shuffled_deck[my_card_index]
  them <- shuffled_deck[their_card_index]
  
  matches <- me == them
  lose <- any(matches)
  win <- !lose
  
  return(win)
}


trials <- 10^7
burper <- 250000
wins <- 0

for (j in (1:trials)) {
  wins <- wins + Fiddler_Card_Matching_Game_XC() 
  if (j %% burper == 0) {
    update_line <- paste(j, trials, sep = " / " )
    print(update_line)
  }
}

win_perc <- wins/trials
sprintf("%1.3f%%", win_perc*100)

# 60.35%

# https://thefiddler.substack.com/p/can-you-play-the-price-is-right-continuously