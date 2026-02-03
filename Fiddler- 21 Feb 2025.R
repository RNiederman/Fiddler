# https://thefiddler.substack.com/p/can-you-defend-your-trivia-knowledge

library(magrittr)
library(pbapply)


points <- c(0, 1, 1, 2, 2, 3)
questions <- length(points)
threshold <- 1/2

Fiddler_Learned_League <- function(correct=2) {
  require(magrittr)
  wrong <- questions - correct

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

fiddler_main <- sum(mc2 > threshold) / length(mc2)
sprintf("%1.1f%%", fiddler_main*100) 

hist(mc2)

rm(points, questions, threshold)
rm(trials, mc2)

# 1 / 3

# https://thefiddler.substack.com/p/how-many-rabbits-can-you-pull-out