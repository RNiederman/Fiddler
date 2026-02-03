# https://thefiddler.substack.com/p/could-you-have-won-the-super-bowl

library(pbapply)

strategy1 <- c(7, 3, 0) # original strategy
strategy2 <- c(7, 0)    # aggressive strategy

fiddler.football.xc <- function(agressive=TRUE) {

  scores <- c(NA, NA)
  # scores[1] will be 'us'; scores[2] will be 'them'
  
  if (agressive) {
      us.strategy <- strategy2} else
      {us.strategy <- strategy1}
  scores[1] <- sample(us.strategy, 1)
  
  # They will go for the aggressive strategy if we score
  # Otherwise it makes sense with a 2/3 chance of scoring (& winning)
  if (scores[1] > 0) {
    them.strategy <- strategy2} else
    {them.strategy <- strategy1}
  scores[2] <- sample(them.strategy, 1)
  
  # Do we need a continued overtime period?
  continued.ot <- scores[1] == scores[2]
  
  # Once the game is in sudden death, 
  # the original strategy is the best option
  while (continued.ot) {
    us <- sample(strategy1, 1)
    if (us > 0) {
      scores[1] <- scores[1] + us
      break} # Bailout of the Loop if we Score
    them <- sample(strategy1, 1)
    continued.ot <- them == 0 # Continue Overtime if they don't Score 
    scores[2] <- scores[2] + them
  }
  
  we.win <- scores[1] > scores[2]
  return(we.win)
}

trials.xc <- 10^7

mc.agg <- pbreplicate(trials.xc, fiddler.football.xc(TRUE))
mc.org <- pbreplicate(trials.xc, fiddler.football.xc(FALSE))

win.perc.agg <- sum(mc.agg)/trials.xc
win.perc.org <- sum(mc.org)/trials.xc

sprintf("Aggressive Strategy: %1.1f%%", win.perc.agg*100)
sprintf("Original Strategy: %1.1f%%", win.perc.org*100)

# 56.2% if I go aggressive 1st
# 54.1% if I go with the original strategy 1st

# https://thefiddler.substack.com/p/can-you-fairly-cut-the-birthday-cake

rm(mc.agg, mc.org, strategy1, strategy2)
