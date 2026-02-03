# https://thefiddler.substack.com/p/can-you-hop-to-the-lily-pad

library(dplyr)
library(parallel)
library(furrr)
library(scales)

starting_pad <- 2
moves <- c(-1, 1)


Froggy_Fiddler <- function(pads=4) {
  k <- starting_pad
  enders <- c(1, pads)  
  
  while (all(k != enders) ) {
    odds <- c(1/k, (k - 1)/k )
    hop <- sample(x = moves, size = 1, prob = odds)
    k <- k + hop
  }
  
  win <- k == 1
  return(win)
}


trials <- 25000
yuge_n <- 50000
small_trials <- 500
small_run_set <- rep(yuge_n, small_trials)
denom <- comma(trials)

worker_cores <- 5
options(future.rng.onMisuse = "ignore")

wins <- 0
i <- 0



plan(multisession, workers = worker_cores)

while (i < trials) {
  i <- i + small_trials
  
  small_mc <- future_map_lgl(small_run_set, Froggy_Fiddler)
  
  wins <- wins + sum(small_mc)
  
  wp <- sprintf("%1.1f%%", wins / i * 100)
  
  paste(comma(i), denom, sep = " / ") %>% 
    paste(., wp, sep = " --> ") %>% 
    print
}

plan(sequential)

win_perc <- wins / trials

rm(i, wp, small_mc, denom)
rm(small_trials, small_run_set, worker_cores)
rm(starting_pad, moves)

sprintf("%1.2f%%", win_perc * 100)
# 63.37%

# https://thefiddler.substack.com/p/can-you-spin-the-graph
