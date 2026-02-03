# https://thefiddler.substack.com/p/happy-almost-new-year-from-the-fiddler-f0b


library(purrr)
library(lpSolveAPI)


#### Step 1- Get all the prime numbers from 1 to 2025


is_prime20 <- function(n) {
  
  two <- n == 2
  prime.test <- all(n %% 2:ceiling(sqrt(n)) > 0)
  
  tester <- any(two, prime.test)
  
  return(tester)
}


target_20 <- 2025
test_set_20 <- 1:target_20

prime_test_20 <- map_lgl(test_set_20, is_prime20) 
primes_20 <- test_set_20[prime_test_20]
rm(test_set_20, prime_test_20)


#### Step 2- Setup and run the linear optimization problem

L20 <- length(primes_20)

values_20 <- rep(1, L20)
opt_type_20 <- "max"
direction_20 <- "="
problem_type_20 <- "binary"

lp_model_20 <- make.lp(0, L20)

set.objfn(lp_model_20, values_20)
add.constraint(lp_model_20, primes_20, direction_20, target_20)

lp.control(lp_model_20, sense = opt_type_20)
set.type(lp_model_20, 1:L20, problem_type_20)


#### Run the Problem ####

solve(lp_model_20)
key_20 <- get.variables(lp_model_20)
get.objective(lp_model_20)
get.constr.value(lp_model_20)
get.total.iter(lp_model_20)
get.solutioncount(lp_model_20)


#### Step 3- Extract the prime numbers used

n_primes_20 <- sum(key_20)
primes_used_20 <- primes_20[key_20 == 1]

print(sum(primes_used_20))

print(n_primes_20)
cat(primes_used_20, sep = ", ")

# https://thefiddler.substack.com/p/can-you-squeeze-the-sheets
