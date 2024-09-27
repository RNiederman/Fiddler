# https://thefiddler.substack.com/p/can-you-win-at-rock-paper-scissors
# https://www.youtube.com/watch?v=x5Q6-wMx-K8

library(parallel)
library(furrr)
library(tictoc)

# R - Rock
# P - Paper
# S - Scissors
# Z - Lizard
# K - Spock

key <- cbind(
    c("S", "P", "R", "Z", "K"), # Element
    c("P", "R", "Z", "K", "S"), # Defeats
    c("Z", "K", "S", "P", "R")  # Also Defeats
)
elements <- key[,1]

RPSLS <- function(players=3) {

  throw <- sample(elements, players, replace = TRUE)
  
  for (p in (1:players)) {
    us <- throw[p]
    them <- throw[-p] 
    rw <- which(elements == us)
    defeats <- key[rw, 2:3]
    remain <- setdiff(them, defeats) 
    win <- length(remain) == 0
    if (win) {break}
  }
  return(win)
}

trials <- 10^7
worker_cores <- detectCores() - 1
run_set <- rep(3, trials)

options(future.rng.onMisuse = "ignore")

tic()
plan(multisession, workers = worker_cores)

  mc <- future_map_lgl(run_set, RPSLS)

plan(sequential)
toc()

win_perc <- sum(mc)/trials
sprintf("%1.2f%%", win_perc*100)
# 48%

rm(mc, run_set)
rm(worker_cores)