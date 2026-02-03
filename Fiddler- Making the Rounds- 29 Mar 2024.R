# https://thefiddler.substack.com/p/can-you-pour-the-water

library(tidyverse)
library(parallel)
library(doSNOW)

cards <- c("A", 2:10, "J", "Q", "K")
suits <- c("\u2660","\u2666","\u2665", "\u2663")
# suits <- c("S",     "D",     "H",      "C")
suit_colors <- c("black", "red", "red", "black")

deck <- tibble(
  number = rep(cards, length(suits)),
  suit = rep(suits, length(cards)),
  color = rep(suit_colors, length(cards))
  ) %>% 
  arrange(factor(suit, levels = suits), factor(number, levels = cards))

deck[27:52,] <- deck[52:27,]


Three_Card_Flip <- function() {
  require(tidyverse)
  
  deal <- sample_n(deck, 3) %>% 
    arrange(fct_infreq(color), suit, number)
  
  tester <- deal$color[1] == deal$color[3]
  
  return(tester)
}


n <- 10^6
iterations <- 5000
n0 <- n %/% iterations

n_cores <- detectCores()
n_cores <- ifelse(n_cores > 5, 5, n_cores - 1)

cl <- makeCluster(n_cores)
registerDoSNOW(cl)

pb <- txtProgressBar(max = iterations, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

mc <- foreach(k = 1:iterations, .combine = "+", .options.snow = opts) %dopar% {
  replicate(n0, Three_Card_Flip())
}

close(pb)
stopCluster(cl)

perc <- sum(mc)/n
sprintf("%1.2f%%", perc*100)

rm(cards, suits, suit_colors)
rm(cl, n_cores)
rm(iterations, n0)
rm(pb, opts)

# https://web.northeastern.edu/seigen/11Magic/Articles/Modeling%20Mathematics%20with%20Playing%20Cards.pdf