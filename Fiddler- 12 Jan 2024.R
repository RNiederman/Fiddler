# https://thefiddler.substack.com/p/can-you-roll-the-dungeon-masters

library(pbapply)

dice_set <- c(4, 6, 8, 10, 12, 20)

Fiddler_Dice_Game <- function(rollers=2) {

  dice <- sample(dice_set, rollers, replace = TRUE)
  rolls <- sapply(dice, function(x) sample(x, 1))
  distinct_values <- unique(rolls)
  distinct_count <- length(distinct_values)
  
  return(distinct_count)
}

n <- 10^7


# 'Standard' Fiddler
mc1 <- pbreplicate(n, Fiddler_Dice_Game(2))
prob <- length(mc1[mc1 == 1])/n
sprintf("%1.3f%%", prob*100)
# 9.375%

t1 <- table(mc1)
ymax1 <- max(t1) * 1.2
p1 <- barplot(t1, axes = FALSE, ylim = c(0, ymax1))
text(p1, t1, sprintf("%1.2f%%", t1/n * 100), cex = 1, pos = 3) 


# Extra Credit
mc2 <- pbreplicate(n, Fiddler_Dice_Game(3))
distinct_numbs <- sum(mc2)/n
sprintf("%1.2f", distinct_numbs)
# 2.73

t2 <- table(mc2)
ymax2 <- max(t2) * 1.2
p2 <- barplot(t2, axes = FALSE, ylim = c(0, ymax2))
text(p2, t2, sprintf("%1.2f%%", t2/n * 100), cex = 1, pos = 3) 

rm(mc1, mc2)

# https://thefiddler.substack.com/p/can-you-make-it-home-in-time-for