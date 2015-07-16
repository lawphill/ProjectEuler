# Consider the fraction, n/d, where n and d are positive integers. If n<d and HCF(n,d)=1, it is called a
#   reduced proper fraction.
# If we list the set of reduced proper fractions for d ≤ 8 in ascending order of size, we get:
#  1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8
# It can be seen that there are 21 elements in this set.
# How many elements would be contained in the set of reduced proper fractions for d ≤ 1,000,000?


# The number of fractions with any denominator is equal to EulersPhi(denominator), the number of 
# coprime integers less than the denominator.

# The slowest portion is factorizing an integer, so we want to avoid that.
# First calculate primes and the squares of the primes (less than our maximum)
# For primes, EulerPhi(n) = n - 1, for squareless numbers, we determine prime factors and multiply
# their totients together, i.e. EulerPhi(a*b) == EulerPhi(a) * EulerPhi(b) if a & b are coprime
# For squareful numbers, we run the full EulerPhi calculation

library("gmp") # For factorize function

EulerPhi <- function(n){
  factors <- unique(asNumeric(factorize(n)))
  return(round(n * prod(1 - (1/factors))))
}
# This simple approach takes ~46 seconds
total <- as.bigz(0L)
for(i in 2:1000000){
  total <- add.bigz(total,EulerPhi(i))
}
print(total)



# Below is a slower method (maybe because it's storing each totient value?)
# It calculates the totient of all primes, then if it encounters a squareless number, it doesn't need
# to call EulerPhi.

sieve <- function(n)
{
  n <- as.integer(n)
  if(n > 1e8) stop("n too large")
  primes <- rep(TRUE, n)
  primes[1] <- FALSE
  last.prime <- 2L
  fsqr <- floor(sqrt(n))
  while (last.prime <= fsqr)
  {
    primes[seq.int(2L*last.prime,n,last.prime)] <- FALSE
    sel <- which(primes[(last.prime+1):(fsqr+1)])
    if(any(sel)){
      last.prime <- last.prime + min(sel)
    }else last.prime <- fsqr+1
  }
  which(primes)
}

max_d <- 1000000L
primes <- sieve(max_d)
limit <- floor(sqrt(max_d))
squares <- sapply(2:limit,function(x) x^2)

totients <- rep(0L,max_d)
totients[primes] <- primes - 1 # Seed totient list with prime values
total <- as.bigz(0L)
for(i in which(totients==0)){
  # Check if square-free
  if(!any((i %% squares) == 0)){
    f <- unique(asNumeric(factorize(i)))
    totients[i] <- prod(totients[f])
  }else{
    totients[i] <- EulerPhi(i)
  }
}
totients[1] <- 0
print(sum(totients))