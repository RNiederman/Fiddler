# https://thefiddler.substack.com/p/can-you-sprint-to-the-finish

library(pbapply)


Fiddler_Bike_Race <- function(threshold=0.25) {
  # 1 = I win, 0 = they win

  winner <- rbinom(1, 1, 1/2)
  legs <- runif(2) # them, me

  they_sprint <- legs[1] >= 1/2
  i_sprint <- legs[2] >= threshold
  
  sprint <- any(they_sprint, i_sprint)
  
  if (sprint) {
    winner <-  which.max(legs) - 1
  }

  return(winner)
}


trials <- 10^7

lo <- 0
hi <- 75

perc_set <- lo:hi / 100
ledger <- rep(NA, length(perc_set) )

i <- 1
for (p in perc_set) {
  mc <- pbreplicate(trials, Fiddler_Bike_Race(p) )
  soln <- sum(mc) / trials
  ledger[i] <- soln
  
  plot_set <- 1:i
  
  v1 <- perc_set[plot_set]
  v2 <- ledger[plot_set]
  
  plot(v2 ~ v1)
  
  if (i > 1 & soln < 1/2) {break}
  i <- i + 1
  
}

perc_set <- perc_set[1:i]
ledger <- na.omit(ledger)

rm(i, mc, soln, p)
rm(v1, v2, plot_set)

win_perc <- max(ledger, na.rm = TRUE)
idx <- which.max(ledger)
threshold_perc <- perc_set[idx]

sprintf("%.3f%%", win_perc*100) 
sprintf("%.0f%%", threshold_perc*100) 
