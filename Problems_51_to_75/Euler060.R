# The primes 3, 7, 109, and 673, are quite remarkable. By taking any two primes and concatenating them in
#   any order the result will always be prime. For example, taking 7 and 109, both 7109 and 1097 are
#   prime. The sum of these four primes, 792, represents the lowest sum for a set of four primes with this
#   property.

# Find the lowest sum for a set of five primes for which any two primes concatenate to produce another
#   prime.

# BRUTE FORCE SOLUTION. FOR EACH PRIME, GENERATE THE SET OF CONCATENABLE PRIMES, GO THROUGH THAT SET
#   NARROWING DOWN UNTIL 5 ARE FOUND
#   SOLVES VERY SLOWLY, DISCOVERS THE CORRECT ANSWER IN ~20 SECONDS, TAKES MUCH LONGER TO REACH THE
#   END OF THE PRIME SET

library("memoise")

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
    primes[seq.int(2L*last.prime, n, last.prime)] <- FALSE
    sel <- which(primes[(last.prime+1):(fsqr+1)])
    if(any(sel)){
      last.prime <- last.prime + min(sel)
    }else last.prime <- fsqr+1
  }
  which(primes)
}

isPrime <- function(x){
  # DETERMINE PRIMENESS OF A VECTOR X
  ans <- rep(FALSE,length(x))
  if(any(x <= max(primes))){
    ans[x<=max(primes)] <- x[x<=max(primes)] %in% primes
  }
  if(any(x > max(primes))){
    ans[x>max(primes)] <- sapply(x[x>max(primes)], function(z) !any(z %% 2:ceiling(sqrt(z)) == 0))
  }
  return(ans)
}
isPrimeMem <- memoise(isPrime)

isConcatPrime <- function(x,y){
  # ALLOWS VECTORS FOR Y
  return(isPrimeMem(x*10^ceiling(log10(y)) + y) & isPrimeMem(y*10^ceiling(log10(x)) + x))
}
isConcatPrimeMem <- memoise(isConcatPrime)


max_primes <- 9999
primes<-sieve(max_primes)
min_sum <- sum(primes[(length(primes)-4):length(primes)])

for(a in primes){
  if(a >= min_sum){ break }
  print(a)
  poss <- primes[primes > a]
  possA <- poss[isConcatPrime(a,poss)]
  
  for(b in possA){
    if(a+b >= min_sum){ break }
    possB <- possA[isConcatPrime(b,possA)]
    for(c in possB){
      if(a+b+c >= min_sum){ break }
      possC <- possB[isConcatPrime(c,possB)]
      
      for(d in possC){
        if(a+b+c+d >= min_sum){ break }        
        possD <- possC[isConcatPrime(d,possC)]
        for(e in possD){
          if(a+b+c+d+e >= min_sum){ break }
          print(c(a,b,c,d,e))
          min_sum <- min(min_sum,sum(c(a,b,c,d,e)))
        }
      }
    }
  }
}
print(min_sum)