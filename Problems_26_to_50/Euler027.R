# For the quadratic equation n^2 + a*n + b, find the product of a & b
# |a| < 1000, |b| < 1000, where the equation creates the longest string of 
# consecutive primes, starting with n=0


max_primes <- 99999
primes <- 1:max_primes
primes[1] <-0
for(i in 2:floor(max_primes/2)){
  if(primes[i] != 0){
    primes[seq.int(i*2,max_primes,i)] <- 0
  }
}
primes <- primes[primes!=0]


is_prime <- function(n){
  if(n<1){
    return(FALSE)
  }else{
    if(sum((n %% 2:floor(sqrt(n)))==0)==0){
      return(TRUE)
    }else{
      return(FALSE)
    }
  }
}

quadratic <- function(a,b,n){
  return(n^2 + a*n + b)
}

# Brute Force solution

best_a <- -1000
best_b <- -1000
max_primes <- 0

for(b in primes[primes<=999]){
  for(a in (-b+1):999){
    n <- 0
    while(is_prime(quadratic(a,b,n))==TRUE){
      n <- n + 1
    }
    
    if(n > max_primes){
      max_primes <- n
      best_a <- a
      best_b <- b
    }
  }
}
print(best_a*best_b)