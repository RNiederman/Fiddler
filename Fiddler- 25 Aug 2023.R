# https://thefiddler.substack.com/p/can-you-bet-solely-on-your-teams
library(magrittr)

Fiddler.25Aug <- function(coh = 100, wager1 = 50) {
  require(magrittr)

  games <- c(1, -1) %>% sample
  g1 <- wager1 * games[1]
  
  balance <- coh + g1
  
  wager2 <- ifelse(games[1] == 1, 0, balance) 
  # Bet zero if game 1 was a win, else go all in knowing game 2 is a Win
  g2 <- wager2 * games[2]
  
  balance <- balance + g2
  return(balance)  
}

n <- 100
# bet.set <- 1:99
bet.set <- seq(from = 32, to = 34, by = .0001)
results <- vector()

for (b in bet.set) {
  q <- replicate(n, Fiddler.25Aug(100, b))
  z <- min(q)
  results <- c(results, z)
  
  paste(b, z, sep = " --> ") %>% print
}

plot(results~bet.set)

g <- max(results)
i <- which(results == g)
w <- bet.set[i]

print(g)
print(w)

# https://thefiddler.substack.com/p/can-you-bob-and-weave