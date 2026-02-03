# https://thefiddler.substack.com/p/can-you-set-a-winning-baseball-lineup

library(dplyr)
library(parallel)
library(doSNOW)


Inning.Sim <- function(sluggers=4) {
  batters <- rep(TRUE, 9)
  batters[sluggers] <- FALSE
  # TRUE are contact hitters, FALSE are the sluggers
  runs <- 0
  outs <- 0
  b <- 1
  bases <- rep(FALSE, 4)
  while (outs < 3) {
    batter <- batters[b]
    h <- ifelse(batter, 1/3, 1/10)
    o <- runif(1)
    hit <- h >= o
    if (hit & batter) {
      # Single, All Runners Advance One Base
      bases <- c(hit, bases[1:3])
      runs <- runs + bases[4]
      bases[4] <- 0
    }  
    else if (hit & !batter) {
      # Home Run -> Bases Clear
      runs <- runs + sum(bases) + 1
      bases <- rep(FALSE, 4)
    }
    else {outs <- outs + 1}

    b <- ifelse(b < 9, b + 1, 1)
  }
  return(runs)
  
}

n <- 10^6
multi <- 5
n <- n * multi

forloops <- 50000
n0 <- n %/% forloops

lineup <- 1:9
runs.scored <- rep(0, 9)

ncores <- detectCores()
ncores <- ifelse(ncores > 5, 5, ncores - 1)
cl <- makeCluster(ncores)
registerDoSNOW(cl)

for (slugger in lineup) {
  q <- foreach(j = 1:forloops, .combine = '+') %dopar% {
      require(magrittr)
      replicate(n0, Inning.Sim(slugger)) %>% sum
    }
  z <- q / n
  runs.scored[slugger] <- z
  
  paste(slugger, sprintf("%.4f", z) , sep = " --> ") %>% print
}

stopCluster(cl)

barplot(height = runs.scored, name = lineup, col = "#4287F6")

inning.df <- data.frame(lineup, runs.scored) %>% 
  arrange(desc(runs.scored))

rm(q, z, slugger)
rm(n, n0, multi, ncores)
rm(lineup)


# https://thefiddler.substack.com/p/can-you-bet-solely-on-your-teams