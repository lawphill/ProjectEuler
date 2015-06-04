# It was proposed by Christian Goldbach that every odd composite number can be written as the sum of
#   a prime and twice a square.

#   9 = 7 + 2×1^2
#   15 = 7 + 2×2^2
#   21 = 3 + 2×3^2
#   25 = 7 + 2×3^2
#   27 = 19 + 2×2^2
#   33 = 31 + 2×1^2

# It turns out that the conjecture was false.
# What is the smallest odd composite that cannot be written as the sum of a prime and twice a square?

# SIMPLE BRUTE FORCE ALGORITHM
# GO THROUGH EACH ODD NUMBER, IF PRIME ADD TO LIST, OTHERWISE CHECK AGAINST CONJECTURE
# STOP WHEN WE GET THE FIRST NUMBER TO DISPROVE THE CONJECTURE

poss <- 3
primes <- c(2)

conjecture <- TRUE
while(conjecture){
  if(poss %% 10001 == 0){ print(poss)}
  if(sum((poss %% primes) == 0) == 0){ # IS PRIME
    primes <- c(primes,poss)
  }else{ # IS COMPOSITE
    if( all((sqrt((poss-primes)/2) %% 1) != 0 )){
      conjecture <- FALSE
      print("FALSE")
      print(poss)
    }
  }
  poss <- poss + 2
}