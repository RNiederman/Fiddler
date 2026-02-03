# https://thefiddler.substack.com/p/how-low-or-high-can-you-go

library(dplyr)
library(pbapply)


Fiddler_Hi_Lo <- function(big_n=100) {
  require(dplyr)
  
  v <- runif(big_n)
  guess_higher <- v < 1/2
  is_higher <- lead(v) > v
  win <- guess_higher == is_higher
  
  # Find the first Instanace of FALSE & Subtract 1
  score <- match(FALSE, win) - 1

  return(score)  
}


trials <- 10^7
mc <- pbreplicate(trials, Fiddler_Hi_Lo(75))

wins <- sum(mc >= 2)
soln <- wins / trials
sprintf("%0.1f%%", soln * 100)
# 54.2%
# 13/24

summary(mc)

tab <- table(mc)
plot(tab/trials)

df <- data.frame(
  wins = as.integer(names(tab)),
  n = as.integer(tab),
  perc = sprintf("%0.1f%%", tab / trials * 100),
  win_perc = sprintf("%0.1f%%", (1 - cumsum(tab) / trials) * 100)
)

rm(mc, tab)

# https://thefiddler.substack.com/p/can-you-box-the-letters
