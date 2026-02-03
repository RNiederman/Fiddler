# https://thefiddler.substack.com/p/can-you-find-a-matching-pair-of-socks

library(dplyr)
library(uniqtag)
library(pbapply)


Sock_Fiddler <- function(sock_pairs=5) {
  require(dplyr)
  require(uniqtag)
  
  socks <- 1:sock_pairs %>% as.character %>% rep(2) %>% sample
  draw <- cumcount(socks)
  first_match <- which(draw == 2) %>% min
  
  return(first_match)
}


trials <- 10^6

n_socks <- 5
bins <- 0:(n_socks*2)

mc <- pbreplicate(trials, Sock_Fiddler(n_socks))
mn <- sum(mc) / trials

sprintf("%1.0f", mn)
hist(mc, prob = TRUE, breaks = bins)
probs <- table(mc)/trials
bp <- barplot(probs)
y_buffer <- 0.02
text(x = bp, y = probs - y_buffer, labels = sprintf("%1.1f%%", probs*100))

# https://thefiddler.substack.com/p/can-you-spy-on-the-infinite-corridor
