# https://thefiddler.substack.com/p/happy-almost-new-year-from-the-fiddler-f0b

library(purrr)
library(lpSolve)


#### Step 1- Get all the prime numbers from 1 to 2025

is_prime <- function(n) {
  
  two <- n == 2
  prime.test <- all(n %% 2:ceiling(sqrt(n)) > 0)
  
  tester <- any(two, prime.test)
  
  return(tester)
}

target <- 2025
test_set <- 1:target

prime_test <- map_lgl(test_set, is_prime) 
primes <- test_set[prime_test]
rm(test_set, prime_test)


#### Step 2- Setup and run a linear optimization problem

obj <- rep(1, length(primes))
const_matrx <- matrix(primes, nrow = 1)

result <- lp(
              direction = "max",       # Maximize the right side
              objective.i = obj,       # Objective coefficients
              const.mat = const_matrx, # Constraint matrix
              const.dir = "=",         # Constraint direction
              const.rhs = target,      # Right hand side
              all.bin = TRUE           # Binary variables
)
              
soln_key <- result$solution


#### Step 3- Extract the prime numbers used

n_primes = sum(soln_key)
primes_used <- primes[soln_key == 1]

cat(sum(primes_used))

cat(n_primes)
cat(primes_used,  sep = ", ")



