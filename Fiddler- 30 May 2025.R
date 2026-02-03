# https://thefiddler.substack.com/p/can-you-weave-the-web

library(ggplot2)
library(dplyr)
library(tictoc)

SIDE_REF <- LETTERS[1:4]

N <- 25000

rand_a <- runif(N)
rand_b <- runif(N)

m <- matrix(NA, nrow = N, ncol = 4)

run_set <- 1:N

tic()

for (i in seq_along(run_set) ) {

  sides <- sample(SIDE_REF, 2, replace = FALSE)
  a <- rand_a[i]
  b <- rand_b[i]
  
  set_1 <- case_when(
    sides[1] == "A" ~ c(0, a),
    sides[1] == "B" ~ c(a, 1),
    sides[1] == "C" ~ c(1, a),
    sides[1] == "D" ~ c(a, 0),
  )
  
  set_2 <- case_when(
    sides[2] == "A" ~ c(0, b),
    sides[2] == "B" ~ c(b, 1),
    sides[2] == "C" ~ c(1, b),
    sides[2] == "D" ~ c(b, 0),
  )
  
  m[i, 1:2] <- set_1
  m[i, 3:4] <- set_2
}

toc()

rm(rand_a, rand_b, run_set)
rm(sides, set_1, set_2)
rm(a, b, i)


headers <- c("x1", "y1", "x2", "y2")
df <- data.frame(m) %>% 
  set_colnames(headers)
rm(m, headers)

alp <- 0.015
colr <- "#001428"
lw <- 0.75

ggplot(df, aes(x = x1, y = y1, xend = x2, yend = y2)) +
  geom_segment(color = colr, alpha = alp) +
  theme_void() +
  theme(panel.border = element_rect(color = "black", 
                                    fill = NA, 
                                    linewidth = lw))

# https://thefiddler.substack.com/p/can-you-squeeze-the-bubbles
