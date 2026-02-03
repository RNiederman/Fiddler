# https://thefiddler.substack.com/p/can-you-bob-and-weave

library(dplyr)
library(stringr)


vect2numb <- function(vect) {
  require(dplyr)
  
  vect %>% 
    paste(collapse = "") %>% 
    as.numeric
}


numb2vect <- function(numb) {
  require(dplyr)
  require(stringr)
  
  numb %>% 
    as.character %>% 
    strsplit("") %>% 
    unlist %>% 
    as.numeric
}


n4 <- sample(0:9, 4)
v <- vect2numb(n4)

i <- 0
match <- FALSE


while (!match) {
  i <- i + 1
  
  temp <- vect2numb(n4)
  
  x <- n4 %>% 
    sort(decreasing = TRUE) %>% 
    vect2numb
  y <- n4 %>% 
    sort(decreasing = FALSE) %>% 
    vect2numb
  
  tester <- x - y
  n4 <- numb2vect(tester) 

  match <- tester == temp
  if (!match) {v <- c(v, tester)}
  
}

print(v)

# https://medium.com/illumination/the-legend-of-6174-d29a629b1795
