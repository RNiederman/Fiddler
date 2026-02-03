# https://thefiddler.substack.com/p/can-you-take-a-risk

library(pbapply)
library(dplyr)

types <- c("I", "C", "A", "W")
card_counts <- c(14, 14, 14, 2)
risk_deck <- rep(types, card_counts)
wc <- types[4]


Risk_Fiddler_XC <- function() {
  require(dplyr)

  deck_5 <- sample(risk_deck, 5, replace = FALSE)
  win <- FALSE
  n <- 3
  
  while (!win) {
    draw_0 <- deck_5[1:n]

    draw <- draw_0[draw_0 != wc]
    n_wc <- n - length(draw)

    mode_v <- draw %>% table %>% max
    n_dist <- draw %>% unique %>% length

    test_1 <- (mode_v + n_wc) >= 3 # three of a kind
    test_2 <- (n_dist + n_wc) >= 3 # one of each
    
    win <- any(test_1, test_2)
    n <- n + !win
  }
  
  return(n)
}


xc_trials <- 10^7
xc_mc <- pbreplicate(xc_trials, Risk_Fiddler_XC())

xc_soln <- sum(xc_mc) / xc_trials
sprintf("%.2f", xc_soln)
# 3.76

prop.table(table(xc_mc)) * 100
xc_mc %>% table %>% barplot

rm(xc_mc) 
# rm(risk_deck, types, card_counts, wc)

# https://thefiddler.substack.com/p/when-will-you-cross-your-path
# https://www.davidyding.com/navPages/riddlers/fiddler09262025
