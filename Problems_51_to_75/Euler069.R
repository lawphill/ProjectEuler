# Euler's Totient function, φ(n) [sometimes called the phi function], is used to determine the number of
#   numbers less than n which are relatively prime to n. For example, as 1, 2, 4, 5, 7, and 8, are all
#   less than nine and relatively prime to nine, φ(9)=6.

# It can be seen that n=6 produces a maximum n/φ(n) for n ≤ 10.
# Find the value of n ≤ 1,000,000 for which n/φ(n) is a maximum.

sieve <- function(n)
{
  n <- as.integer(n)
  if(n > 1e8) stop("n too large")
  primes <- rep(TRUE, n)
  primes[1] <- FALSE
  last.prime <- 2L
  fsqr <- floor(sqrt(n))
  #fsqr <- floor(n/2)
  while (last.prime <= fsqr)
  {
    multiples <- seq.int(2L*last.prime,n,last.prime)
      # Keeps track of prime factors
    #for(i in multiples){
    #  factors[[i]] <<- c(factors[[i]],last.prime)
    #}
    #factors[[last.prime]] <<- last.prime
    primes[multiples] <- FALSE
    sel <- which(primes[(last.prime+1):(fsqr+1)])
    if(any(sel)){
      last.prime <- last.prime + min(sel)
    }else last.prime <- fsqr+1
  }
  which(primes)
}

# This is a straightforward implementation taken from Wikipedia, it is too slow
# totient(n) = n * prod(1- 1/p) # for p in all prime factors of n
# So the ratio is n / (n * prod(1-1/p)) = 1 / prod(1-1/p)
# Our goal then is to minimize prod(1-1/p)
# We can do that in two ways:
#   1) Increase the number of prime factors of n
#   2) Make those prime factors as small as possible
# Therefore, we want to find the largest number which is a multiple of all the smallest prime factors

primes <- sieve(1000)
n <- 1
limit <- 1000000
i <- 1 # index for primes
while(n * primes[i] <= limit){
  n <- n * primes[i]
  i <- i + 1
}
print(n)