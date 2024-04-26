# https://thefiddler.substack.com/p/can-you-paint-by-number

library(pbapply)

Fiddler_Strip <- function(squares=10^6) {
  strip <- rbinom(squares, 1, 0.5)

  clusters <- rle(strip)
  mean_cluster_size <- mean(clusters$lengths)
  
  return(mean_cluster_size)
}

trials <- 5000
mc <- pbreplicate(trials, Fiddler_Strip(10^6))

avg <- sum(mc)/trials

sprintf("%.3f", avg)
sprintf("%.9f", var(mc))

# https://thefiddler.substack.com/p/can-you-eclipse-via-ellipse