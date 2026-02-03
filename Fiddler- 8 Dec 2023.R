# https://thefiddler.substack.com/p/can-you-make-the-connection

library(dplyr)
library(magrittr)
library(parallel)
library(doSNOW)

tiles <- LETTERS[1:4] %>% rep(4)


fiddler.connections <- function() {
  draw <- sample(tiles, 4)
  tab <- table(draw)
  mxx <- max(tab)
  
  return(mxx)
}


n <- 10^7
iterations <- 1000
n0 <- n %/% iterations

ncores <- detectCores()
ncores <- ifelse(ncores > 5, 5, ncores - 1)

cl <- makeCluster(ncores)
registerDoSNOW(cl)

pb <- txtProgressBar(max = iterations, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

mc <- foreach(k = 1:iterations, .combine = c, .options.snow = opts) %dopar% {
  replicate(n0, fiddler.connections())
}

close(pb)
stopCluster(cl)

bins <- table(mc)

rm(mc)
rm(cl, ncores)
rm(iterations, n0)
rm(pb, opts)

bins %>% divide_by(n) %>% multiply_by(100) %>% sprintf("%1.2f%%", .)


# https://thefiddler.substack.com/p/dont-flip-out