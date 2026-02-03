# https://thefiddler.substack.com/p/dont-flip-out

library(pbapply)

coin <- c("H", "T")

kyle_patt <- c("T", "T", "H")
jule_patt <- c("T", "T", "T")

fiddler_coin_toss <- function() {
  
  kyle <- sample(coin, 2, replace = TRUE)
  jule <- sample(coin, 2, replace = TRUE)
  
  winner <- FALSE
  while (!winner) {

    k <- sample(coin, 1)
    j <- sample(coin, 1)
    
    kyle <- c(kyle, k)
    jule <- c(jule, j)
    
    kyle_tail <- tail(kyle, 3)
    jule_tail <- tail(jule, 3)
    
    kyle_test <- identical(kyle_tail, kyle_patt)
    jule_test <- identical(jule_tail, jule_patt)
    
    winner <- kyle_test + jule_test == 1
  }
  
  return(kyle_test)
}

n <- 10^6
mc <- pbreplicate(n, fiddler_coin_toss())

odds <- sum(mc)/n
sprintf("%1.2f%%", odds*100)
# 64.24%

rm(mc)

# https://thefiddler.substack.com/p/happy-almost-new-year-from-the-fiddler