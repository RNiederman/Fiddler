# https://thefiddler.substack.com/p/how-many-rides-can-you-reserve

library(dplyr)
library(pbapply)

strt <- 9
endd <- strt + 12 - 1


Fiddler_Lightning_Lane_XC <- function(rides = 3) {
  require(dplyr)
  
  i <- 0
  times <- strt:endd
  slots <- sample(times, rides, replace = FALSE) %>% sort
  
  repeat {
    i <- i + 1   
    recent_ride <- slots[i]
    
    all_times <- (recent_ride + 1):endd
    times <- setdiff(all_times, slots)
    
    if (length(times) == 0) {break}

    new_ride <- sample(as.character(times), 1) %>% as.numeric
    slots <- c(slots, new_ride) %>% sort
  }
  
  ride_count <- length(slots)
  return(ride_count)
}


trials2 <- 10^6
mc2 <- pbreplicate(trials2, Fiddler_Lightning_Lane_XC(3) )

expected_rides2 <- sum(mc2) / trials2
sprintf("%1.2f", expected_rides2)
# 6.81

table(mc2) %>% barplot

rm(strt, endd)

# https://thefiddler.substack.com/p/can-you-sweep-the-series