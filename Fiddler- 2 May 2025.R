# https://thefiddler.substack.com/p/how-many-rides-can-you-reserve

library(dplyr)
library(pbapply)

strt <- 9
endd <- strt + 12 - 1


Fiddler_Lightning_Lane <- function(rides = 3) {
  require(dplyr)
  
  times <- strt:endd
  slots <- sample(times, rides, replace = FALSE) %>% sort
  last_ride <- max(slots)

  while (last_ride < endd) {
    times <- (last_ride + 1):endd
    last_ride <- sample(as.character(times), 1) %>% as.numeric
    slots <- c(slots, last_ride)
  }
  
  ride_count <- length(slots)
  return(ride_count)
}


trials <- 10^6
mc <- pbreplicate(trials, Fiddler_Lightning_Lane(3) )

expected_rides <- sum(mc) / trials
sprintf("%1.2f", expected_rides)
# 4.27

table(mc) %>% barplot

rm(strt, endd)

# https://thefiddler.substack.com/p/can-you-sweep-the-series

math_soln <- 118361/27720
abs(math_soln - expected_rides)