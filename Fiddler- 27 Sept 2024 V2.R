# https://thefiddler.substack.com/p/can-you-win-at-rock-paper-scissors
# https://www.youtube.com/watch?v=x5Q6-wMx-K8

library(parallel)
library(furrr)
library(tictoc)

#### Part 0- Setup the Scoring Key Functions #### 


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
valid_combos <- c(2, 3)


RPSLS <- function(players=3) {
  
  win <- FALSE
  
  throw <- sample(elements, players, replace = TRUE)
  unique_elements <- length(unique(throw))
  loop_it <- any(unique_elements == valid_combos)
  
  if (loop_it) {
    player_set <- 1:length(throw) 
    for (p in player_set) {
      us <- throw[p]
      them <- throw[-p]
      rw <- which(elements == us)
      defeats <- key[rw, 2:3]
      remain <- setdiff(them, defeats)
      win <- length(remain) == 0
      if (win) {break}
    }
  }
  return(win)
}


Vector_Slicer <- function(vect, label_set) {
  chunks <- length(label_set)
  sliced_vect <- split(vect, cut(seq_along(vect), chunks, labels = label_set))
  return(sliced_vect)
}


#### Part 1- Monte Carlo Simulation #### 

trials <- 10^6
n_players <- 2:15
run_set <- rep(n_players, each = trials)

worker_cores <- detectCores() - 1
options(future.rng.onMisuse = "ignore")
plan(multisession, workers = worker_cores)

tic()
  mc <- future_map_lgl(run_set, RPSLS)
toc()

plan(sequential)

mc_split <- Vector_Slicer(mc, n_players)
win_percs <- lapply(mc_split, mean)


####  Part 2- Mathematical Solution ####

rpsls_math_soln <- function(p) {
  exp <- p - 1
  right_side <- (2/5)^exp
  answ <- p * right_side
  
  return(answ)
}


math_percs <- rpsls_math_soln(n_players)

#### Combined Solutions ####

cbind(
  "MC" = lapply(win_percs, function(x) sprintf("%1.2f%%", x*100)),
  "Math" = sprintf("%1.2f%%", math_percs*100)
)

rm(mc, mc_split, run_set, worker_cores)

# https://thefiddler.substack.com/p/how-many-dice-can-you-roll-the-same
