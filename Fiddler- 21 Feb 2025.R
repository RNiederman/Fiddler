# https://thefiddler.substack.com/p/can-you-defend-your-trivia-knowledge

library(magrittr)
library(pbapply)

points <- c(0, 1, 1, 2, 2, 3)


Fiddler_Learned_League <- function(correct=2) {
  require(magrittr)
  wrong <- 6 - correct

  them <- c(rep(T, correct), rep(F, wrong) ) %>% sample
  us <- sample(points)
  score <- us[them] %>% sum
  
  least <- head(points, correct) %>% sum
  most <- tail(points, correct)  %>% sum
  num <- most - score
  denom <- most - least
  defensive_efficiency <- num / denom

  return(defensive_efficiency)
}


trials <- 10^6

mc2 <- pbreplicate(trials, Fiddler_Learned_League() )

fiddler_main <- sum(mc2 > 1/2) / length(mc2)
sprintf("%1.1f%%", fiddler_main*100) 

hist(mc2)

rm(points, trials)
rm(mc2)

# 1 / 3
