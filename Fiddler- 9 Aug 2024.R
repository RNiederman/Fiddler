# https://thefiddler.substack.com/p/can-you-hack-gymnastics

library(magrittr)
library(pbapply)
library(parallel)
library(magrittr)
library(furrr)
library(tictoc)


#### base function ####
Gymnastics.Fiddler <- function(a.diff=6, b.diff=5) {
  
  rando <- runif(4, 0, 10)
  
  diff <- c(a.diff, b.diff)
  diff <- ifelse(is.na(diff), rando[1:2], diff)

  exec <- rando[3:4]

  addd <- diff + exec   
  mult <- diff * exec
  
  a.rank <- rank(addd) 
  m.rank <- rank(mult)
  
  matched <- a.rank[1] == m.rank[1]
  return(matched)
}

#### number of runs ####
trials <- 10^7


##### pbapply::pbreplicate ####
gc() %>% invisible()
mc1 <- pbreplicate(trials, Gymnastics.Fiddler(6, 5))
mc2 <- pbreplicate(trials, Gymnastics.Fiddler(NA, NA))

wp1 <- sum(mc1)/trials
wp2 <- sum(mc2)/trials

rm(mc1, mc2)


#### furrr::future_map ####
set.a <- rep(6, trials)
set.b <- rep(5, trials)
na.set <- rep(NA, trials)

worker.cores <- detectCores()
worker.cores <- min(worker.cores, 5) 

gc() %>% invisible
tic()
  options(future.rng.onMisuse = "ignore")
  plan(multisession, workers = worker.cores)
    mc.sim.1 <- future_map2_lgl(set.a, set.b, Gymnastics.Fiddler)
    mc.sim.2 <- future_map2_lgl(na.set, na.set, Gymnastics.Fiddler)
  plan(sequential)
toc()

win.perc.1 <- sum(mc.sim.1)/trials
win.perc.2 <- sum(mc.sim.2)/trials

rm(worker.cores)
rm(set.a, set.b, na.set)
rm(mc.sim.1, mc.sim.2)


#### the results ####
sprintf("%1.2f%%", wp1*100) # bapply::pbreplicate; Main
# 96.17%
sprintf("%1.2f%%", win.perc.1*100) # furrr::future_map; Main
# 96.17%
sprintf("%1.2f%%", wp2*100) # bapply::pbreplicate; Extra Credit
# 91.67%
sprintf("%1.2f%%", win.perc.2*100) # furrr::future_map; Extra Credit
# 91.69%


#### the answer ####
# https://thefiddler.substack.com/p/how-high-can-you-jump
