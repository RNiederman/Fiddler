# https://thefiddler.substack.com/p/bingo

library(dplyr)
library(parallel)
library(tictoc)


Fiddler_Bingo <- function(rowws=3) {
  require(dplyr)
  
  midpoint <- mean(1:rowws)
  numbs <- rowws^2
  
  numb_set <- 1:numbs %>% sample
  numb_grid <- matrix(numb_set, nrow = rowws, ncol = rowws)
  
  bingo_grid <- matrix(FALSE, nrow = rowws, ncol = rowws)
  bingo_grid[midpoint, midpoint] <- TRUE
  
  free_numb <- numb_grid[midpoint, midpoint]
  numb_set2 <- numb_set[numb_set != free_numb] %>% sample
  
  for (numb2 in numb_set2) {
    posn <- which(numb_grid == numb2)  
    bingo_grid[posn] <- TRUE
    
    h_test <- any(rowSums(bingo_grid) == rowws)
    v_test <- any(colSums(bingo_grid) == rowws)
    d_test1 <- all(diag(bingo_grid))
    d_test2 <- all(diag(bingo_grid[, rowws:1]))
   
    m_test <- any(h_test, v_test, d_test1, d_test2)
    if (m_test) {break}
  }
  
  draws <- which(numb_set2 == numb2)
  return(draws)
}


# Monte Carlo Settings
TRIALS <- 10^7
run_set <- 1:TRIALS

# Setup Parallel Processing
ncores <- detectCores() - 1
cl <- makeCluster(ncores)
clusterExport(cl, "Fiddler_Bingo")

# Run the MC Sims
tic.clearlog()

tic("3x3 Bingo Card")
  mc <- parSapply(cl, run_set, function(x) Fiddler_Bingo(3) )
toc(log = TRUE)

tic("5x5 Bingo Card")
  xc_mc <- parSapply(cl, run_set, function(x) Fiddler_Bingo(5) )
toc(log = TRUE)

tic("7x7 Bingo Card")
  xxc_mc <- parSapply(cl, run_set, function(x) Fiddler_Bingo(7) )
toc(log = TRUE)

stopCluster(cl)

time_df <- tic.log(format = FALSE) %>% 
  do.call(rbind, .) %>% 
  as.data.frame %>% 
  mutate(secs = as.numeric(toc) -  as.numeric(tic) ) %>% 
  mutate(secs = floor(secs) ) %>% 
  transmute(msg, time_delta = sprintf("%d:%02d", secs %/% 60, secs %% 60) )

BAR_COLORS <- c("blue", "seagreen", "darkorange4")
high_y <- rep(1, 3)

# Main Fiddler
soln <- sum(mc) / TRIALS
sprintf("%.2f", soln)
# 3.47

pt <- prop.table(table(mc))
high_y[1]  <- max(pt) * 1.1
barplot(pt,
        col = BAR_COLORS[1],
        ylim = c(0, high_y[1]))

# Extra Credit
xc_soln <- sum(xc_mc) / TRIALS
sprintf("%.2f", xc_soln)
# 13.61

xc_pt <- prop.table(table(xc_mc))
high_y[2] <- max(xc_pt) * 1.1
barplot(xc_pt,
        col = BAR_COLORS[2],
        ylim = c(0, high_y[2]))

# Extra Extra Credit
xxc_soln <- sum(xxc_mc) / TRIALS
sprintf("%.2f", xxc_soln)
# 30.63

xxc_pt <- prop.table(table(xxc_mc))
high_y[3] <- max(xxc_pt) * 1.1
barplot(xxc_pt,
        col = BAR_COLORS[3],
        ylim = c(0, high_y[3]))

# Summaries
summary(mc)
summary(xc_mc)
summary(xxc_mc)

# Clean-Up
rm(ncores, cl, run_set)
rm(BAR_COLORS)
rm(list = ls(pattern = "mc"))
rm(list = ls(pattern = "pt"))
rm(high_y)

# https://thefiddler.substack.com/p/can-you-hop-in-a-spiral