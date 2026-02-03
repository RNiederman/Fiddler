# https://thefiddler.substack.com/p/can-you-meet-me-at-the-mall

library(pbapply)


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


trials <- 10^6
multi <- 5
trials <- trials * multi

mn <- pbreplicate(trials, Mall_Fiddler(3, 15) )
xc <- pbreplicate(trials, Mall_Fiddler(4, 15) )

mn_soln <- sum(mn) / trials
xc_soln <- sum(xc) / trials

mn_tab <- table(mn)
xc_tab <- table(xc)

rm(mn, xc)

sprintf("%.4f", mn_soln)
sprintf("%.4f", xc_soln)

# 2.0315 for 3 friends
# 2.4766 for 4 friends

barplot(mn_tab)
barplot(xc_tab)

lotsa_friends <- 100
xxc <- pbreplicate(trials, Mall_Fiddler(lotsa_friends, 15) )

xxc_soln <- sum(xxc) / trials
xxc_tab <- table(xxc)

rm(xxc)

sprintf("%.4f", xxc_soln)
barplot(xxc_tab)
# 33.4226 for 100 friends

# https://thefiddler.substack.com/p/can-you-sprint-to-the-finish
# https://www.sciencenews.org/puzzle-answershttps://www.sciencenews.org/puzzle-answers