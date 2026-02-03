# https://thefiddler.substack.com/p/round-round-get-a-round-i-get-a-round

library(magrittr)
library(parallel)
library(furrr)
library(tictoc)


Fiddler_Rounder <- function(n_numbers=2) {
  require(magrittr)
  
  randos <- runif(n_numbers)
  
  A1 <- randos %>% round %>% sum # Round First, then Sum
  B2 <- randos %>% sum %>% round # Sum First, then Round
  tester <- A1 == B2
  
  return(tester)
}


Vector_Slicer <- function(vect, label_set) {
  chunks <- length(label_set)
  sliced_vect <- split(vect, cut(seq_along(vect), chunks, labels = label_set))
  return(sliced_vect)
}


trials <- 10^6
maxx <- 50
number_set <- 2:maxx
run_set <- rep(number_set, each = trials)

worker_cores <- detectCores() %>% subtract(1)

options(future.rng.onMisuse = "ignore")
plan(multisession, workers = worker_cores)

tic()
  mc <- future_map_lgl(run_set, Fiddler_Rounder)
toc()

plan(sequential)

mc_split <- Vector_Slicer(mc, number_set)
win_percs <- lapply(mc_split, mean)

cbind(
  lapply(win_percs, function(x) sprintf("%1.2f%%", x*100))
)

plot(number_set, win_percs)

rm(run_set, mc, mc_split)
rm(worker_cores, maxx, trials)


# https://thefiddler.substack.com/p/can-you-turn-a-right-triangle-into

