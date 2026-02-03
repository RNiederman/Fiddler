# https://thefiddler.substack.com/p/can-you-win-at-rock-paper-scissors
# https://www.youtube.com/watch?v=x5Q6-wMx-K8


library(tictoc)
library(stringr)
library(magrittr)

# R - Rock
# P - Paper
# S - Scissors
# Z - Lizard
# K - Spock

key <- cbind(
    c("S", "P", "R", "Z", "K"), # Element
    c("P", "R", "Z", "K", "S"), # Defeats
    c("Z", "K", "S", "P", "R")  # Also Defeats
)
elements <- key[,1]
element_count <- length(elements)
valid_combos <- c(2, 3)


R.P.S.L.S <- function(throw) {
  
  win <- FALSE
  
  unique_elements <- length(unique(throw))
  loop_it <- any(unique_elements == valid_combos)
  
  if (loop_it) {
    player_set <- 1:length(throw) 
    for (p in player_set) {
      us <- throw[p]
      them <- throw[-p]
      rw <- which(elements == us)
      defeats <- key[rw, 2:3]
      remain <- setdiff(them, defeats)
      win <- length(remain) == 0
      if (win) {break}
    }
  }
  return(win)
}


max_players <- 10
wins <- rep(NA, max_players)
player_set <- 2:max_players

tic.clearlog()

for (p in player_set) {
  tic(p)
    combo_matrix <- expand.grid(replicate(p, elements, simplify = FALSE))
    winners <- apply(combo_matrix, 1, R.P.S.L.S)
  toc(log = TRUE)
      
  wins[p] <- sum(winners)
  rm(combo_matrix, winners)
}

wins <- na.omit(wins)
combos <- element_count^player_set
win_percs <- wins / combos

tictoc_regex <- "(?<=\\d:\\s).*(?=\\s[elapsed])"
time_log <- tic.log(format = TRUE) %>% 
  str_extract(tictoc_regex)


data.frame(
  "Players" = player_set,
  "Wins" = wins %>% format(big.mark = ",", trim = TRUE),
  "Combos" = combos %>% format(big.mark = ",", trim = TRUE),
  "Win.Perc" = win_percs %>% multiply_by(100) %>% sprintf("%1.2f%%", .),
  "Run.Time" = time_log
)

run_time_3 <- str_extract(time_log, "\\b[-+]?\\d*\\.?\\d+\\b") %>% as.numeric %>% sum

rm(p, max_players)
rm(tictoc_regex)

# https://thefiddler.substack.com/p/how-many-dice-can-you-roll-the-same
