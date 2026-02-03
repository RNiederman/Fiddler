# https://thefiddler.substack.com/p/can-you-root-for-the-underdog

library(magrittr)

N_TEAMS <- 4


fiddler_game <- function(team_a, team_b, boost=0) {
    vv <- c(team_a, team_b)
    power_index <- N_TEAMS + 1 - vv
    hi_seed <- min(vv)
    lo_seed <- max(vv)

    lo_power_index <- min(power_index) + boost
    hi_power_index <- max(power_index)

    fav_win <- hi_power_index > lo_power_index
    winner <- ifelse(fav_win, hi_seed, lo_seed)
    return(winner)
}

fiddler_tournament <- function(B) {
    game_1 <- fiddler_game(4, 1, B)
    game_2 <- fiddler_game(3, 2, B)

    final <- fiddler_game(game_1, game_2, B)

    return(final)
}


hi_value <- N_TEAMS + 1.1
vals <- seq(from = 0.1, to = hi_value, by = 0.1) %>% round(1)
test_set <- vals[vals %% 1 != 0]
rm(hi_value, vals)

trials <- length(test_set)
m <- matrix(NA, nrow = trials, ncol = 2)
m[, 1] <- test_set
colnames(m) <- c("B", "Champ")
ledger <- integer(N_TEAMS)

for (i in 1:trials) {
    b <- test_set[i] %>% round(1)
    champ <- fiddler_tournament(b)
    ledger[champ] <- ledger[champ] + 1
    m[i, 2] <- champ
}

rm(i, b, champ, trials, test_set)

ledger
barplot(ledger)

# The 1-seed wins with low boost values (< 1)
# the 4-seed wins with high boost values (> 3)
# The 3-seed wins when 1 < boost value < 3

# There is no way the 2-seed can win. 
# The 2-seed needs a boost > 1 to defeat the 1-seed in the finals.
# But a boost > 1 will always push the 3-seed past the 2-seed in the first round

# https://thefiddler.substack.com/p/can-you-solve-a-high-schoolers-favorite-e7f
# https://www.davidyding.com/navPages/riddlers/fiddler03282025
