# https://thefiddler.substack.com/p/can-you-root-for-the-underdog


library(magrittr)

N_TEAMS <- 64


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


fiddler_xc_tournament <- function(B) {
    # game_Round_HiSeed
    
    # First Round
    game_1_1  <- fiddler_game(1,  64, B)
    game_1_2  <- fiddler_game(2,  63, B)
    game_1_3  <- fiddler_game(3,  62, B)
    game_1_4  <- fiddler_game(4,  61, B)
    game_1_5  <- fiddler_game(5,  60, B)
    game_1_6  <- fiddler_game(6,  59, B)
    game_1_7  <- fiddler_game(7,  58, B)
    game_1_8  <- fiddler_game(8,  57, B)
    game_1_9  <- fiddler_game(9,  56, B)
    game_1_10 <- fiddler_game(10, 55, B)
    game_1_11 <- fiddler_game(11, 54, B)
    game_1_12 <- fiddler_game(12, 53, B)
    game_1_13 <- fiddler_game(13, 52, B)
    game_1_14 <- fiddler_game(14, 51, B)
    game_1_15 <- fiddler_game(15, 50, B)
    game_1_16 <- fiddler_game(16, 49, B)
    game_1_17 <- fiddler_game(17, 48, B)
    game_1_18 <- fiddler_game(18, 47, B)
    game_1_19 <- fiddler_game(19, 46, B)
    game_1_20 <- fiddler_game(20, 45, B)
    game_1_21 <- fiddler_game(21, 44, B)
    game_1_22 <- fiddler_game(22, 43, B)
    game_1_23 <- fiddler_game(23, 42, B)
    game_1_24 <- fiddler_game(24, 41, B)
    game_1_25 <- fiddler_game(25, 40, B)
    game_1_26 <- fiddler_game(26, 39, B)
    game_1_27 <- fiddler_game(27, 38, B)
    game_1_28 <- fiddler_game(28, 37, B)
    game_1_29 <- fiddler_game(29, 36, B)
    game_1_30 <- fiddler_game(30, 35, B)
    game_1_31 <- fiddler_game(31, 34, B)
    game_1_32 <- fiddler_game(32, 33, B)
    
    # Second Round
    game_2_1  <- fiddler_game(game_1_1,  game_1_32, B)
    game_2_2  <- fiddler_game(game_1_2,  game_1_31, B)
    game_2_3  <- fiddler_game(game_1_3,  game_1_30, B)
    game_2_4  <- fiddler_game(game_1_4,  game_1_29, B)
    game_2_5  <- fiddler_game(game_1_5,  game_1_28, B)
    game_2_6  <- fiddler_game(game_1_6,  game_1_27, B)
    game_2_7  <- fiddler_game(game_1_7,  game_1_26, B)
    game_2_8  <- fiddler_game(game_1_8,  game_1_25, B)
    game_2_9  <- fiddler_game(game_1_9,  game_1_24, B)
    game_2_10 <- fiddler_game(game_1_10, game_1_23, B)
    game_2_11 <- fiddler_game(game_1_11, game_1_22, B)
    game_2_12 <- fiddler_game(game_1_12, game_1_21, B)
    game_2_13 <- fiddler_game(game_1_13, game_1_20, B)
    game_2_14 <- fiddler_game(game_1_14, game_1_19, B)
    game_2_15 <- fiddler_game(game_1_15, game_1_18, B)
    game_2_16 <- fiddler_game(game_1_16, game_1_17, B)
    
    # Third Round (Sweet 16)
    game_3_1 <- fiddler_game(game_2_1, game_2_16, B)
    game_3_2 <- fiddler_game(game_2_2, game_2_15, B)
    game_3_3 <- fiddler_game(game_2_3, game_2_14, B)
    game_3_4 <- fiddler_game(game_2_4, game_2_13, B)
    game_3_5 <- fiddler_game(game_2_5, game_2_12, B)
    game_3_6 <- fiddler_game(game_2_6, game_2_11, B)
    game_3_7 <- fiddler_game(game_2_7, game_2_10, B)
    game_3_8 <- fiddler_game(game_2_8, game_2_9,  B)

    # Fourth Round (Elite 8)
    game_4_1 <- fiddler_game(game_3_1, game_3_8, B)
    game_4_2 <- fiddler_game(game_3_2, game_3_7, B)
    game_4_3 <- fiddler_game(game_3_3, game_3_6, B)
    game_4_4 <- fiddler_game(game_3_4, game_3_5, B)

    # Fifth Round (Final Four)
    game_5_1 <- fiddler_game(game_4_1, game_4_4, B)
    game_5_2 <- fiddler_game(game_4_2, game_4_3, B)

    # The 'Ship
    game_6_1 <- fiddler_game(game_5_1, game_5_2, B)

    return(game_6_1)
}


hi_value_2 <- N_TEAMS + 1.1
vals_2 <- seq(from = 0.1, to = hi_value_2, by = 0.1) %>% round(1)
test_set_2 <- vals_2[vals_2 %% 1 != 0]
rm(hi_value_2, vals_2)

trials_2 <- length(test_set_2)
m2 <- matrix(NA, nrow = trials_2, ncol = 2)
m2[, 1] <- test_set_2
colnames(m2) <- c("Boost", "Champs")
ledger_2 <- integer(N_TEAMS)

for (j in 1:trials_2) {
    b2 <- test_set_2[j] %>% round(1)
    champion <- fiddler_xc_tournament(b2)
    ledger_2[champion] <- ledger_2[champion] + 1
    m2[j, 2] <- champion
    
}

rm(j, b2, champion, trials_2, test_set_2)

non_winners <- which(ledger_2 == 0)
cat(non_winners, sep = ", ")
length(non_winners)

barplot(ledger_2)

half_rows <- which(m2[,1] %% 1 == 0.5)
m2b <- m2[half_rows,]

# https://thefiddler.substack.com/p/can-you-solve-a-high-schoolers-favorite-e7f
# https://www.davidyding.com/navPages/riddlers/fiddler03282025

