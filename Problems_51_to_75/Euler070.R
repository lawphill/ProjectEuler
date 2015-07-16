# For 1 <= n <= 10^7, which n has the minimum ratio of n/EulerPhi(n) but ALSO EulerPhi(n) is a 
#   permutation of n, e.g. EulerPhi(87109) = 79180

# Since we're looking at a minimum now (as compared to the max in problem 69), we want to maximize
#   prod(1-1/p) where p is all the prime factors of n
# That means as few and as large prime factors as possible

# The lowest ratios are for primes, but n and n-1 won't be permutations of one another
# Next best would be the product of two primes
# Euler Phi for that would be n * (1 - 1/p1) * (1-1/p2)
# n is the product of p1*p2 so: p1*p2 * (1-1/p1) * (1-1/p2)
# that reduces to (p1 - 1) * (p2 - 1), so we want to maximize that


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

isPerm <- function(x,y){
  # Will fail for large enough x or y
  digitsX <- (x %/% 10^((ceiling(log10(x+1))-1):0)) %% 10
  digitsY <- (y %/% 10^((ceiling(log10(y+1))-1):0)) %% 10
  
  return(all(sort(digitsX) == sort(digitsY)))
}

limit <- 10^7
midpoint <- floor(sqrt(limit))
primes <- sieve(10000)

lower_primes <- rev(primes[primes <= midpoint])
upper_primes <- primes[primes >= midpoint]
best_n <- 0
min_ratio <- 10000000
for(p1 in lower_primes){
  ndigits1 <- ceiling(log10(p1+1))
  for(p2 in upper_primes){
    n <- p1 * p2
    if(n > limit){
      break
    }
    ndigits2 <- ceiling(log10(p2+1))
    if(ndigits1 != ndigits2){
      next
    }
    phi <- (p1 - 1) * (p2 - 1)
    if(isPerm(n,phi)){
      ratio <- n / phi
      if(ratio < min_ratio){
        min_ratio <- ratio
        best_n <- n
      }
    }
  }
}
print(best_n)
