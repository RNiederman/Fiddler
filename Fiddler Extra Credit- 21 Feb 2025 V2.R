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
threshold <- 1/2

correct_set <- 1:(questions - 1)
wins <- 0
games <- 0

for (correct in correct_set) {
  incorrect <- questions - correct
  v <- c(rep(TRUE, correct), rep(FALSE, incorrect) )
  
  combo_df <- grid_expander(v, points) 
  
  scores <- map2_int(combo_df[,1], combo_df[,2], ~trivia_scorer(.x, .y) )

  least <- sum(head(points, correct) )
  most <- sum(tail(points, correct) )
  nums <- most - scores
  denom <- most - least  

  defn_eff <- nums / denom
  temp_wins <- sum(defn_eff > threshold)
  temp_games <-  nrow(combo_df)
  
  wins <- wins + temp_wins
  games <- games + temp_games
  
  update <- paste(correct, max(correct_set), sep = " / ") 
  print(update)
  
  title_helper <- ifelse(correct == 1 , "Correct Answer", "Correct Answers")
  hist(defn_eff, main = paste("Defensive Efficiency:" , correct,  title_helper) )
}

rm(correct, incorrect, questions, threshold)
rm(v, points, combo_df)
rm(scores, defn_eff)
rm(temp_wins, temp_games, correct_set)
rm(most, least, nums, denom, update, title_helper)

xc_soln <- wins / games
sprintf("%1.3f%%", xc_soln*100) 
# 43.333%

# https://thefiddler.substack.com/p/how-many-rabbits-can-you-pull-out