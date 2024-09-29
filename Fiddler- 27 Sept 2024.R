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

options(future.rng.onMisuse = "ignore")
plan(multisession, workers = worker_cores)

# 3 Players
run_set_3 <- rep(3, trials)
tic()
  mc_3 <- future_map_lgl(run_set_3, RPSLS)
toc()

# 4 Players
run_set_4 <- rep(4, trials)
tic()
  mc_4 <- future_map_lgl(run_set_4, RPSLS)
toc()

plan(sequential)


win_perc_3 <- sum(mc_3)/trials
sprintf("%1.2f%%", win_perc_3*100)
# 48%

win_perc_4 <- sum(mc_4)/trials
sprintf("%1.2f%%", win_perc_4*100)
# 25.6%

rm(mc_3, mc_4)
rm(run_set_3, run_set_4)
rm(worker_cores)
