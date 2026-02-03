# https://thefiddler.substack.com/p/how-long-is-the-river-of-text

library(dplyr)
library(pbapply)


three <- rep("3", 3) %>% paste(collapse = "")
four <- rep("4", 4) %>% paste(collapse = "")
word_choices <- c(three, four)


Fiddler_05232025 <- function(words=10^6) {
  text_string <- sample(word_choices, words, replace = TRUE) %>%
    paste(collapse = " ")
  L <- nchar(text_string) - 1
  low_bound <- floor(.95 * L)
  end_range <- low_bound:L
  placer <- sample(end_range, 1)
  char <- substr(text_string, placer, placer)
  
  space <- char == " "
  
  return(space)
}


word_coumnt <- 10^4
TRIALS <- 10^6
mc <- pbreplicate(TRIALS, Fiddler_05232025(word_coumnt) )

perc <- sum(mc) / TRIALS
sprintf("%1.1f%%", perc * 100)

# 22.2%


# https://thefiddler.substack.com/p/can-you-weave-the-web
# 2/9 or 22.22%