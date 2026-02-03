# https://thefiddler.substack.com/p/how-far-can-you-run-before-sundown
library(pbapply)

distances <- c(1, 3, 3.5, 4.5)

Fiddler_Race <- function(pace=10, minutes_left=65, mulligans=0) {
  score <- 0
  time_remaining <- minutes_left
  keep_running <- TRUE
  
  while (keep_running) {
    leg <- sample(distances, 1)
    leg_time <- leg * pace
    time_remaining <- time_remaining - leg_time
    keep_running <- time_remaining >= 0
    score <- score + (keep_running * leg)
    if (!keep_running & mulligans > 0) {
      mulligans <- mulligans - 1
      time_remaining <- time_remaining + leg_time
      keep_running <- TRUE
    }
  }
  return(score)
}


n <- 10^7
mc <- pbreplicate(n, Fiddler_Race())
mc_xc <- pbreplicate(n, Fiddler_Race(10, 65, 1))

soln <- sum(mc) / n
sprintf("%0.3f", soln)
# 4.886

soln_xc <- sum(mc_xc) / n
sprintf("%0.3f", soln_xc)
# 5.290

plot(table(mc))
plot(table(mc_xc))

rm(mc, mc_xc)

# https://thefiddler.substack.com/p/how-low-or-high-can-you-go