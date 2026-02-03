# https://thefiddler.substack.com/p/could-you-have-won-the-super-bowl

library(pbapply)


outcomes <- c(7, 3, 0)

fiddler.football <- function() {
  
  scores <- sample(outcomes, 2, replace = TRUE)
  # scores[1] will be 'us'; scores[2] will be 'them'
  
  # Do we need a continued overtime period?
  continued.ot <- scores[1] == scores[2]
  
  while (continued.ot) {
    us <- sample(outcomes, 1)
    if (us > 0) {
      scores[1] <- scores[1] + us
      break} # Bailout of the loop if we score
    them <- sample(outcomes, 1)
    continued.ot <- them == 0 # Continue Overtime if they don't Score 
    scores[2] <- scores[2] + them
  }
  
  we.win <- scores[1] > scores[2]
  return(we.win)
}

trials <- 10^7
mc <- pbreplicate(trials, fiddler.football())

win.perc <- sum(mc)/trials
sprintf("%1.1f%%", win.perc*100)
# 58.3%

# https://thefiddler.substack.com/p/can-you-fairly-cut-the-birthday-cake
sprintf("%1.1f%%", 7/12*100)

rm(mc, outcomes)
