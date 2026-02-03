# https://thefiddler.substack.com/p/can-you-sweep-the-series

library(dplyr)


Mode <- function(x) {
  ux <- unique(x)
  md <- ux[which.max(tabulate(match(x, ux)))]  
  return(md)
}


GAMES <- 7
TRIALS <- 10^6

INTERVAL <- 0.001

START_P <- 0.1
END_P <- 0.9

p_seq <- seq(from = START_P, to = END_P, by = INTERVAL)
p_len <- length(p_seq)

game_register <- rep(NA, p_len)

for (i in seq_along(p_seq) ) {
  p <- p_seq[i]
  mc <- rbinom(n = TRIALS, size = GAMES, prob = p)
  g <- Mode(mc)
  
  game_register[i] <- g
  
  sprintf("%1.1f%%", p * 100) %>% 
    paste(., g, sep = " --> ") %>% 
    print

  # break outta the loop once we're winning all the games 
  if (g == GAMES) {
    game_register[i:p_len] <- GAMES
    break}
}


rm(i, p, mc, g)
rm(p_len)
rm(GAMES, TRIALS, INTERVAL)
rm(START_P, END_P)

game_register <- game_register %>% as.integer

fiddler_df <- tibble(p_seq, game_register) %>% 
  rename(p = p_seq, 
         games_won = game_register)
  
plot(fiddler_df)
table(game_register)


a <- fiddler_df %>% 
  filter(games_won < 5) %>% 
  pull(p) %>% 
  max

b <- fiddler_df %>% 
  filter(games_won > 5) %>% 
  pull(p) %>% 
  min

sprintf("%1.1f%%", a * 100)
sprintf("%1.1f%%", b * 100)

# a -> 62.5% 
# b -> 75%

# https://thefiddler.substack.com/p/can-you-permeate-the-pyramid