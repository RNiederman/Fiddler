# https://thefiddler.substack.com/p/can-you-meet-me-at-the-mall


Mall_Fiddler <- function(friends=3, wait_time=15) {
  mins <- 60
  
  start_times <- runif(friends, 0, mins)
  end_times <- start_times + wait_time
  end_times <- pmin(end_times, mins)

  arrive <- rep(1, friends)
  depart <- rep(-1, friends)
  
  times <- c(start_times, end_times)
  counts <- c(arrive, depart)
 
  idx <- order(times)
  
  # v1 <- times[idx]
  v2 <- counts[idx]
  
  friend_counts <- cumsum(v2)
  maxx <- max(friend_counts)
  
  return(maxx)
}

trials <- 50000

hi <- 500
lo <- 1
friend_set <- lo:hi
ledger <- double(hi)


for (i in seq_along(friend_set) ) {
  mc <- replicate(trials, Mall_Fiddler(i, 15) )
  soln <- sum(mc) / trials
  ledger[i] <- soln
  
  v1 <- friend_set[1:i]
  v2 <- ledger[1:i]
  plot(v2 ~ v1)
  
  update <- paste(i, hi, sep = " / ")
  print(update)
}


plot(ledger ~ friend_set)                                                                                                                                                                                                                                                                             

rm(mc, v1, v2)
rm(i, soln, update)


# https://www.sciencenews.org/puzzle-answers