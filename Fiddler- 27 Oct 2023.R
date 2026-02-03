# https://thefiddler.substack.com/p/how-long-would-it-take-to-pick-a

library(magrittr)
library(parallel)
library(doSNOW)

Fiddler.Speaker <- function(candidates=3, members=221) {
  require(magrittr)
  
  majority <- (members/2) %>% ceiling
  rounds <- 0
  no.speaker <- TRUE

  while (no.speaker) {
    rounds <- rounds + 1
    votes <- sample(candidates, members, replace = TRUE) %>% table
    no.speaker <- max(votes) < majority
    candidates <- candidates - no.speaker
  }
return(rounds)
}

cands <- 3
membs <- 221

trials <- 10^6
multi <- 25
trials <- trials * multi

updates <- 25000

iterations <- trials %/% updates

ncores <- detectCores()
ncores <- ifelse(ncores > 5, 5, ncores - 1)

pb <- txtProgressBar(max = iterations, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

cl <- makeCluster(ncores)
registerDoSNOW(cl)

mc <- foreach(k = 1:iterations, .combine = '+', .options.snow = opts) %dopar% {
  require(magrittr)
  replicate(updates, Fiddler.Speaker(cands, membs)) %>% sum
}

close(pb)
stopCluster(cl)

sprintf("%1.9f", mc/trials)
# 1.9999996

# https://thefiddler.substack.com/p/iceberg-right-ahead
# 1.99999951
# 8.99999951

# table(mc)
# length(mc) == trials

rm(cands, membs)
rm(multi, updates, iterations, ncores)
rm(pb, cl, progress, opts)
