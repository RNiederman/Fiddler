# https://thefiddler.substack.com/p/can-you-light-up-the-pinball-machine

library(magrittr)
library(parallel)
library(doSNOW)

Pinball <- function(n.lanes=4) {
  lanes <- rep(FALSE, n.lanes)
  flips <- 0
  
  while (!all(lanes)) {
    flips <- flips + 1
    q <- sample(n.lanes, 1)
    lanes[q] <- !lanes[q]
  }
  return(flips)
}

n <- 5000
interations <- 1000
n0 <- n %/% interations

lane.set <- 2:15
v <- vector(mode = "numeric")
ncores <- detectCores()
ncores <- ifelse(ncores > 5, 5, ncores - 1)

cl <- makeCluster(ncores)
registerDoSNOW(cl)

pb <- txtProgressBar(max = interations, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

for (L in lane.set) {
  Z <- foreach(j = 1:interations, .combine = "+", .options.snow = opts) %dopar% {
    require(magrittr)
    replicate(n0, Pinball(L)) %>% sum
  }
  mn <- Z / n
  v <- c(v, mn)
  paste(L, round(mn, 4), sep = " --> ") %>% print
}
stopCluster(cl)
close(pb)

L <- length(v)

temp1 <- v[2:L]
temp2 <- v[1:(L - 1)]
r <-  temp1 / temp2
r <- c(NA, r)
m <- cbind(lane.set, v, r)

plot(r ~ lane.set)

lowest.r <- min(r, na.rm = TRUE)
idex <- which(r == lowest.r) %>% subtract(1)
N <- lane.set[idex]
print(N)

# https://thefiddler.substack.com/p/can-you-figure-the-factorial-numbers
