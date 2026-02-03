# https://thefiddler.substack.com/p/can-you-box-the-letters

library(combinat)
library(dplyr)
library(scales)

letter_count <- 8
sides <- 4
q <- letter_count / sides
stopifnot(q %% 1 == 0 )

letter_vect <- LETTERS[1:letter_count]
key <- rep(1:sides, each = q)
stopifnot(length(letter_vect) == length(key))

perm_list <- permn(letter_vect)

perm_indices <- lapply(perm_list, function(x) match(x, letter_vect))

key_matrix <- lapply(perm_indices, function(y) key[y])

checker <- lapply(key_matrix, function(z) !any(diff(z) == 0))

valid_seq <- checker %>% unlist %>% sum

comma(valid_seq)
# 13,824

# https://thefiddler.substack.com/p/can-your-team-self-organize

