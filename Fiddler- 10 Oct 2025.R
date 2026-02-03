# https://thefiddler.substack.com/p/lets-make-a-tic-tac-deal

library(pbapply)
library(dplyr)


rws <- 3
numbs <- 3:11
grid <- matrix(numbs, nrow = rws, byrow = TRUE)

# Build a list of winning combinations
winners <- list()
# Horizontals
for (i in 1:rws) {
  winners[[length(winners) + 1]] <- grid[i, ]
}
# Verticals
for (j in 1:rws) {
  winners[[length(winners) + 1]] <- grid[, j]
}
# Diagonals
winners[[length(winners) + 1]] <- diag(grid)
winners[[length(winners) + 1]] <- diag(grid[, rws:1])

rm(numbs, grid, i, j, rws)

Fiddler_TicTacDeal <- function(throws=3) {

  roll_1 <- sample(6, throws, replace = TRUE)
  roll_2 <- sample(6, throws, replace = TRUE)
  scores <- unique(roll_1 + roll_2)
  
  wins <- sapply(winners, function(v) all(v %in% scores))
  win <- match(TRUE, wins)

  return(win)
}


trials <- 10^7

# Standard Fiddler
mc <- pbreplicate(trials, Fiddler_TicTacDeal(3))
soln <- sum(!is.na(mc)) / trials
sprintf("%.2f%%", soln * 100)
# 5.81%

pt <- prop.table(table(mc))
high_y <- max(pt) * 1.1

barplot(pt,
        names.arg = winners,
        col = "blue",
        ylim = c(0, high_y),
        las = 2)

# Extra Credit
xc_mc <- pbreplicate(trials, Fiddler_TicTacDeal(5))
xc_soln <- sum(!is.na(xc_mc)) / trials
sprintf("%.2f%%", xc_soln * 100)
# 36.14%

xc_pt <- prop.table(table(xc_mc))
xc_high_y <- max(xc_pt) * 1.1

barplot(xc_pt,
        names.arg = winners,
        col = "darkgreen",
        ylim = c(0, xc_high_y),
        las = 2)

# Clean Up
rm(mc, xc_mc)
rm(winners)
rm(high_y, xc_high_y)

# https://thefiddler.substack.com/p/can-you-reach-the-edge-of-the-square
# https://www.davidyding.com/navPages/riddlers/fiddler10072025
