# https://thefiddler.substack.com/p/can-you-take-a-risk

library(pbapply)
library(dplyr)

types <- c("I", "C", "A")
n_cards <- 14
risk_deck <- rep(types, each = n_cards)


Risk_Fiddler <- function() {
  require(dplyr)
  
  draw <- sample(risk_deck, 3, replace = FALSE)
  n_distinct <- draw %>% unique %>% length

  win <- n_distinct != 2
  return(win)
}


trials <- 10^7
mc <- pbreplicate(trials, Risk_Fiddler())

soln <- sum(mc) / trials
sprintf("%.1f%%", soln * 100)
# 33.4%

rm(mc)
# rm(risk_deck, types, n_cards)

# https://thefiddler.substack.com/p/when-will-you-cross-your-path
# https://www.davidyding.com/navPages/riddlers/fiddler09262025
