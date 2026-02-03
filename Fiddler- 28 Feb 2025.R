# https://thefiddler.substack.com/p/how-many-rabbits-can-you-pull-out

library(dplyr)
library(pbapply)


colors <- c("orange", "green", "purple")


rolling_mode <- function(v) {
  
  v_len <- length(v)
  new_v <- rep(NA, v_len)
  
  for (j in seq_along(new_v)) {
    
    v2 <- v[j:v_len]
    counts <- table(v2)
    max_values <- names(counts)[counts == max(counts)]
    m <- sample(max_values, 1)
    new_v[j] <- m
  }
  return(new_v)
}


Rabbit_Fiddler <- function(rabbit_count=2) {
  require(dplyr)
  
  hat <- rep(colors, rabbit_count) %>% sample
  guesses <- rolling_mode(hat)
  
  matches <- hat == guesses
  score <- sum(matches)

  return(score)
}


trials <- 50000

mc2 <- pbreplicate(trials, Rabbit_Fiddler(2))
mc10 <- pbreplicate(trials, Rabbit_Fiddler(10))

main_soln <- sum(mc2) / trials
xc_soln <- sum(mc10) / trials

sprintf("%#.2f", main_soln)
# 3.37

sprintf("%#.2f", xc_soln)
# 13.71


t2 <- table(mc2)
t2
hist(mc2, main = "2 Rabbits")

t10 <- table(mc10)
t10
hist(mc10, main = "10 Rabbits")

rm(colors)
rm(mc2, mc10)

# https://thefiddler.substack.com/p/can-you-tip-the-dominoes

