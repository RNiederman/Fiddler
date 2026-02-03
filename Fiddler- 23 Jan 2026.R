# https://thefiddler.substack.com/p/bingo

library(dplyr)
library(parallel)
library(tictoc)


Fiddler_Bingo <- function(rowws=3) {
  require(dplyr)
  
  midpoint <- median(1:rowws)
  numbs <- rowws^2
  
  numb_set <- 1:numbs %>% sample
  numb_grid <- matrix(numb_set, nrow = rowws, ncol = rowws)
  
  bingo_grid <- matrix(FALSE, nrow = rowws, ncol = rowws)
  bingo_grid[midpoint, midpoint] <- TRUE
  
  free_numb <- numb_grid[midpoint, midpoint]
  numb_set2 <- setdiff(numb_set, free_numb) %>% sample # Drop out the Number on the Free Square
  
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
  draws <- sum(bingo_grid) - 1 # -1 for the free square
  return(draws)
}


Histogram_Builder <- function(vect, bar_color=NULL) {
  if (is.null(bar_color)) {bar_color <- sample(colors(), 1)}
  
  tab <- table(vect)
  pt <- prop.table(tab)
  high_y  <- max(pt) * 1.1
  bp <- barplot(pt,
          col = bar_color,
          ylim = c(0, high_y),
          yaxt = "n")
  axis(2, at = axTicks(2), labels = paste0(axTicks(2) * 100, "%"))
  
  return(invisible(bp))
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
  mc_3 <- parSapply(cl, run_set, function(x) Fiddler_Bingo(3) )
toc(log = TRUE)

tic("5x5 Bingo Card")
  mc_5 <- parSapply(cl, run_set, function(x) Fiddler_Bingo(5) )
toc(log = TRUE)

tic("7x7 Bingo Card")
  mc_7 <- parSapply(cl, run_set, function(x) Fiddler_Bingo(7) )
toc(log = TRUE)

tic("9x9 Bingo Card")
  mc_9 <- parSapply(cl, run_set, function(x) Fiddler_Bingo(9) )
toc(log = TRUE)

stopCluster(cl)


time_df <- tic.log(format = FALSE) %>% 
  do.call(rbind, .) %>% 
  as.data.frame %>% 
  mutate(time_delta = as.numeric(toc) -  as.numeric(tic) ) %>% 
  mutate(s = as.integer(time_delta %% 60) ) %>% 
  mutate(m = as.integer((time_delta %% 3600) %/% 60) ) %>% 
  mutate(h = as.integer(time_delta %/% 3600) ) %>% 
  transmute(msg, run_time = ifelse(h > 0,
                                  sprintf("%d:%02d:%02d", h, m, s),
                                  sprintf("%d:%02d", m, s) ) )
  
BAR_COLORS <- c("blue", "seagreen", "darkorange4", "magenta4")

# Main Fiddler - 3x3 Bingo Card
Histogram_Builder(mc_3, BAR_COLORS[1])
soln_3 <- sum(mc_3) / TRIALS
sprintf("%.2f", soln_3)
# 3.47

# Extra Credit- 5x5 Bingo Card
Histogram_Builder(mc_5, BAR_COLORS[2])
soln_5 <- sum(mc_5) / TRIALS
sprintf("%.2f", soln_5)
# 13.61

# 7x7 Bingo Card
Histogram_Builder(mc_7, BAR_COLORS[3])
soln_7 <- sum(mc_7) / TRIALS
sprintf("%.2f", soln_7)
# 30.63

# 9x9 Bingo Card
Histogram_Builder(mc_9, BAR_COLORS[4])
soln_9 <- sum(mc_9) / TRIALS
sprintf("%.2f", soln_9)
# 54.88


# Summaries
summary(mc_3)
summary(mc_5)
summary(mc_7)
summary(mc_9)

# Clean-Up
rm(ncores, cl, run_set, BAR_COLORS)
rm(list = ls(pattern = "mc_\\d+"))

# https://thefiddler.substack.com/p/can-you-hop-in-a-spiral