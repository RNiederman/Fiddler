# https://thefiddler.substack.com/p/can-you-defend-your-trivia-knowledge

library(combinat)
library(purrr)


grid_expander <- function(vect_1, vect_2) {
  require(combinat)
  
  perm_1 <- permn(vect_1) # Them
  perm_2 <- permn(vect_2) # Us
  
  expanded_df <- expand.grid(perm_1, perm_2) 
  
  return(expanded_df)
}


trivia_scorer <- function(list_1, list_2) {
  v1 <- unlist(list_1) # Them (Boolean)
  v2 <- unlist(list_2) # Us (Integer)
  
  scores_1 <- v2[v1]
  scores_2 <- sum(scores_1)

  return(scores_2)
}


points <- c(0, 1, 1, 2, 2, 3)
questions <- length(points)
correct <- 2
incorrect <- questions - correct
threshold <- 1/2

v <- c(rep(TRUE, correct), rep(FALSE, incorrect) )

combo_df <- grid_expander(v, points)
games <- nrow(combo_df)

scores <- map2_int(combo_df$Var1, combo_df$Var2, ~trivia_scorer(.x, .y) )

least <- sum(head(points, correct) )
most <- sum(tail(points, correct) )
nums <- most - scores
denom <- most - least  

defn_eff <- nums / denom

wins <- sum(defn_eff > threshold)

hist(defn_eff, main = paste("Defensive Efficiency ->" , correct, "Correct Answers") )

rm(correct, incorrect, questions, threshold)
rm(v, points, combo_df)
rm(scores, defn_eff)
rm(most, least, nums, denom)

soln <- wins / games
sprintf("%1.3f%%", soln*100) 
# 1/3

# https://thefiddler.substack.com/p/how-many-rabbits-can-you-pull-out