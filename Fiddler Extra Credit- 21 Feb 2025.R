# https://thefiddler.substack.com/p/can-you-defend-your-trivia-knowledge

library(magrittr)
library(parallel)
library(furrr)

points <- c(0, 1, 1, 2, 2, 3)


Fiddler_Learned_League <- function(correct=2) {
  require(magrittr)
  wrong <- 6 - correct

  them <- c(rep(T, correct), rep(F, wrong) ) %>% sample
  us <- sample(points)
  score <- us[them] %>% sum
  
  least <- head(points, correct) %>% sum
  most <- tail(points, correct)  %>% sum
  num <- most - score
  denom <- most - least
  defensive_efficiency <- num / denom

  return(defensive_efficiency)
}


trials <- 10^6
correct_answer_sets <- 1:5
run_set <- rep(correct_answer_sets, trials)

worker_cores <- detectCores() - 1
options(future.rng.onMisuse = "ignore")

plan(multisession, workers = worker_cores)
  mc <- future_map_dbl(run_set, Fiddler_Learned_League)
plan(sequential)


fiddler_xc <- sum(mc > 1/2) / length(mc)
sprintf("%1.1f%%", fiddler_xc*100) 

hist(mc)

rm(points)
rm(trials, correct_answer_sets)
rm(run_set, worker_cores)
rm(mc)

# 43.3%
