# https://thefiddler.substack.com/p/where-will-the-sorting-hat-put-you

library(pbapply)

houses <- c("G", "R", "H", "S")
preferred_house <- houses[1]

Fiddler_Sorting_Hat <- function(students=10) {

  preferences <- sample(houses, students, replace = TRUE)
  preferences[students] <- preferred_house
  assignments <- rep(NA, students)

  last_house_assignd <- assignments[1] <- preferred_house 

  for (s in 2:students) {
    eligble_houses <- setdiff(houses, last_house_assignd)
    pref <- preferences[s]
    assignments[s] <- ifelse(any(eligble_houses == pref), pref, sample(eligble_houses, 1))
    last_house_assignd <- assignments[s]
  }
  
  winner <- assignments[students] == preferred_house
  return(winner)
}

trials <- 10^6
mc <- pbreplicate(trials, Fiddler_Sorting_Hat(10))

win_perc <- sum(mc)/trials
sprintf("%1.5f%%", win_perc*100)

rm(mc, trials)
rm(houses, preferred_house)

# https://thefiddler.substack.com/p/can-you-figure-out-how-magna-tiles