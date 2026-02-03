# https://thefiddler.substack.com/p/can-you-even-the-odds

library(parallel)
library(furrr)
library(tictoc)


die <- 1:6
our_rolls <- c(1, 4)


Fiddler_Dice_Game <- function(winner=5) {
  require(magrittr)
  stopper <- FALSE
  
  while (!stopper) {
    rolls <- sample(die, 4, replace = TRUE)
    stopper <- any(rolls == winner)
  }
  
  marker <- which(rolls == winner) %>% min
  we_win <- any(marker == our_rolls)
  
  return(we_win) 
}


n <- 10^6 * 25

worker_cores <- detectCores()
worker_cores <- min(worker_cores, 5) 
options(future.rng.onMisuse = "ignore")
plan(multisession, workers = worker_cores)

tic()
  mc <- future_map_lgl(1:n, ~Fiddler_Dice_Game())
toc()

plan(sequential)

win_perc <- sum(mc)/n
sprintf("%1.2f%%", win_perc*100)
# 50.82%
rm(mc, worker_cores)

# https://thefiddler.substack.com/p/can-you-hack-the-olympics
