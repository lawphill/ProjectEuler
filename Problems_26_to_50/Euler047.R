# The first two consecutive numbers to have two distinct prime factors are:
#  14 = 2 × 7
#  15 = 3 × 5

#The first three consecutive numbers to have three distinct prime factors are:
#   644 = 2^2 × 7 × 23
#   645 = 3 × 5 × 43
#   646 = 2 × 17 × 19.

# Find the first four consecutive integers to have four distinct prime factors. What is the first
#   of these numbers?

# SOLVES IN ABOUT 8-9 SECONDS, USES GMP FOR EFFICIENT FACTORIZATION FUNCTION

library("gmp") # Use for factorize function

num_primes <- 4 # Number of prime factors to find
consecutive <- 0
first_term <- 0
n <- 4

while(consecutive < num_primes){
  #hasN <- hasn_factors(n,num_primes)
  hasN <- length(unique(factorize(n))) == num_primes
  if(hasN & first_term == 0){
    first_term <- n
    consecutive <- 1
  }else if(hasN){
    consecutive <- consecutive + 1
  }else{
    first_term <- 0
    consecutive <- 0
  }
  n <- n + 1
}
print(first_term)