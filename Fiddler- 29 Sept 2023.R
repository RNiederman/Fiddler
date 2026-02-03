# https://thefiddler.substack.com/p/can-you-light-up-the-pinball-machine

library(magrittr)
library(parallel)
library(doSNOW)


Pinball <- function(n_lanes=4) {
  lanes <- rep(FALSE, n_lanes)
  flips <- 0
  
  while (!all(lanes)) {
    flips <- flips + 1
    q <- sample(n_lanes, 1)
    lanes[q] <- !lanes[q]
  }
  return(flips)
}

L <- 4

n <- 10^6
multi <- 5
n <- n * multi

interations <- 50000
n0 <- n %/% interations

pb <- txtProgressBar(max = interations, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

ncores <- detectCores()
ncores <- ifelse(ncores > 5, 5, ncores - 1)
cl <- makeCluster(ncores)
registerDoSNOW(cl)

z <- foreach(j = 1:interations, .combine = 'c', .options.snow = opts) %dopar% {
    replicate(n0, Pinball(L))
  }
stopCluster(cl)
close(pb)

m <- mean(z) %>% round(5)
m2 <- floor(m)
m1 <- m2 - 1
m3 <- max(z)
v <- sum(z == m1)

h <- hist(z, breaks = seq(from = L, to = m3, by = 1), plot = FALSE)
cuts <- cut(h$breaks, c(L, m1, m2, m3))
plot(h, col = c("grey60", "blue", "grey60")[cuts], main = paste(L, "Lanes"))
abline(v = m, col = "black", lwd = 1)
text(m, v, m, adj = c(-0.085, 0))

table(z)
print(m)

print(64/3)
# https://thefiddler.substack.com/p/can-you-figure-the-factorial-numbers