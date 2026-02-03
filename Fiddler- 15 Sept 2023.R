# https://thefiddler.substack.com/p/how-likely-is-a-lopsided-league

library(dplyr)
library(magrittr)
library(pbapply)


perc.format <- function(vect, decimal.places=2) {
  require(magrittr)
  
  n <- length(vect)
  spformat <- paste0("%1.", decimal.places, "f%%")
  
  vect %>% 
    sum %>% 
    divide_by(n) %>% 
    multiply_by(100) %>% 
    sprintf(spformat, .)
}


Fiddler.Baseball <- function(total.divisions=6, teams.per.division=5) {
  require(dplyr)                           
  
  total.teams <- total.divisions * teams.per.division
  
  win.percs <- runif(total.teams)
  divs <- split(win.percs, ceiling(seq_along(win.percs) / teams.per.division))
  
  bottom.teams <- lapply(divs, min) %>% unname %>% unlist
  best.teams <- lapply(divs, max) %>% unname %>% unlist
  m <- outer(bottom.teams, best.teams, FUN = ">")
  
  tester1 <- m[1, 2]
  tester2 <- any(m)
  
  return(c(tester1, tester2))
}

n1 <- 10^6
multi <- 10
n2 <- n1 * multi

Y <- pbreplicate(n2, Fiddler.Baseball(6, 5)) %>% t

Y[,1] %>% perc.format(4)
Y[,2] %>% perc.format(4)

rm(n1, multi)

# https://thefiddler.substack.com/p/can-you-shape-the-peloton
