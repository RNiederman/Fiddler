# https://thefiddler.substack.com/p/can-you-throw-the-card-game

library(magrittr)
library(pbapply)

numb <- 2:10
face <- c("J", "Q", "K", "A")
deck <- c(numb, face) %>% rep(4)

Fiddler.Slap.Deck <- function() {
  require(magrittr)
  
  shuffled.deck <- sample(deck)
  deal <- split(shuffled.deck, c("Me", "Them"))
  # me <- deal[1] %>% unlist
  them <- deal[2] %>% unlist
  
  face.counts <- sapply(face, function(x, y) sum(x == y), them)
  major.face <- all(face.counts > 2)
  
  return(major.face)
}

n <- 10^7 # 10M Trials
mc <- pbreplicate(n, Fiddler.Slap.Deck())

odds <- mean(mc)
sprintf("%1.2f%%", odds*100)
# 0.34%

rm(mc)

# https://thefiddler.substack.com/p/can-you-ride-out-the-slow-car-chase