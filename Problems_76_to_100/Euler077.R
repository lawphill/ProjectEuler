# Similar to Problem 76, there are 5 ways to rewrite 10 as the sum of primes
#   7+3
#   5+5
#   5+3+2
#   3+3+2+2
#   2+2+2+2+2
# What is the first number that can be rewritten as the sum of primes in over 5000 ways?


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

count_rewrites <- function(n,max_num){
  # Count the number of ways to rewrite n as a sum with max term equal to max_num
  if(max_num == 0 || n == 0){ return(NA) }
  if(n > max_prime){
    print("Error, n is too large, generate more primes")
    print(c(n,max_num,max_prime))
    return(NA)
  }
  
  if(rewrites[n,max_num] != -1){
    return(rewrites[n,max_num])
  }
  
  count <- 0
  # Go through primes less than our maximum
  for(i in rev(primes[primes <= min(n-1,max_num)])){
    remainder <- n - i
    if(i >= ceiling(n/2) & (remainder %in% primes)){
      count <- count + 1 # Increment only if both i and the remainder are prime
    }
    count <- count + count_rewrites(remainder,min(remainder,i))
  }
  #print(c(n,max_num,count))
  rewrites[n,max_num] <<- count
  return(count)
}

rewrites_to_find <- 5000
max_prime <- 100 # Arbitrary number, we could use as low as 71, but if 100 were too low, the program
# would report an error
primes <- sieve(max_prime)
rewrites <- matrix(data=-1,nrow=max_prime,ncol=max_prime)
rewrites[1,] <- 0
curr <- 1
while(rewrites[curr,curr] < rewrites_to_find){
  curr <- curr + 1
  rewrites[curr,curr] <- count_rewrites(curr,curr)
}
print(curr)