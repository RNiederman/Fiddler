# https://thefiddler.substack.com/p/can-you-set-a-winning-baseball-lineup

library(combinat)
library(dplyr)
library(parallel)
library(doSNOW)

Inning.Sim <- function(lead.off.man=1, sluggers=c(3,4)) {
  batters <- rep(TRUE, 9)
  batters[sluggers] <- FALSE
  # TRUE are contact hitters, FALSE are the sluggers
  runs <- 0
  outs <- 0
  b <- lead.off.man
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
  return(c(runs, b))
  
}

Baseball.Sim <- function(sluggers=c(3, 4)) {
  i <- 1
  bb <- 1
  runs <- 0
  
  while (i <= 9) {
    inn <- Inning.Sim(bb, sluggers)
    runs <- runs + inn[1]
    bb <- inn[2]
    i <- i + 1
  }
  return(runs)
}

combos <- combn(1:9, 2) %>% t
combo.pairs <- paste(combos[,1], combos[,2], sep = "|")

n <- 10^6
multi <- 5
n <- n * multi

forloops <- 10000
n0 <- n %/% forloops

ledger <- vector(mode = "numeric")

pb <- txtProgressBar(max = forloops, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

ncores <- detectCores()
ncores <- ifelse(ncores > 5, 5, ncores - 1)
cl <- makeCluster(ncores)
registerDoSNOW(cl)

for (combo in 1:nrow(combos)) {
  sluggers <- combos[combo,]

  q <- foreach(j = 1:forloops, .combine = '+', .options.snow = opts) %dopar% {
    require(magrittr)  
    replicate(n0, Baseball.Sim(sluggers)) %>% sum
    }
  z <- q / n
  ledger <- c(ledger, z)
  paste(combo.pairs[combo], sprintf("%.4f", z) , sep = " --> ") %>% print
}
close(pb)
stopCluster(cl)

baseball.df <- data.frame(combo.pairs, ledger) %>% 
  arrange(desc(ledger))

rm(q, z)
rm(combo, sluggers)
rm(n, n0, multi, ncores)

# https://thefiddler.substack.com/p/can-you-bet-solely-on-your-teams