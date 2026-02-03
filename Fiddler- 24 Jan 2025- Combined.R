# https://thefiddler.substack.com/p/can-you-hop-to-the-lily-pad
# https://furrr.futureverse.org/articles/progress.html

library(magrittr)
library(parallel)
library(furrr)
library(progressr)


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


Slicer <- function(mixed_list, n_groups) {
  vec <- unlist(mixed_list, use.names = FALSE)
  sliced_list <- split(vec, (seq_along(vec) - 1) %% n_groups + 1)
  return(sliced_list)
}


trials <- 25000
small_n <- 4
big_n <- 30000

numb_set <- c(small_n, big_n)
n_splits <- length(numb_set)
run_set <- rep(numb_set, trials)
stopifnot(length(unique(run_set) ) == n_splits)

pause_time <- .125
worker_cores <- detectCores() - 1
options(future.rng.onMisuse = "ignore")

plan(multisession, workers = worker_cores)

with_progress({
  p <- progressor(steps = length(run_set) )
  
  mc <- future_map_lgl(run_set, ~{
    p()
    Sys.sleep(pause_time)
    Froggy_Fiddler(.)
  })
})

plan(sequential)
Sys.sleep(1)

mc_splits <- Slicer(mc, n_splits) 

win_percs <- mc_splits %>%
  lapply(., mean) %>% 
  unlist(., use.names = FALSE)
  
sprintf("%1.1f%%", win_percs*100) 
# 59.8%; 63.6%

rm(mc, mc_splits)
rm(p, run_set, worker_cores)
rm(starting_pad, moves)
rm(trials, small_n, big_n, numb_set, n_splits)
rm(pause_time)

# https://thefiddler.substack.com/p/can-you-spin-the-graph
