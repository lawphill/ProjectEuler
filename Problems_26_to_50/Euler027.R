# For the quadratic equation n^2 + a*n + b, find the product of a & b
# |a| < 1000, |b| < 1000, where the equation creates the longest string of 
# consecutive primes, starting with n=0

# Brute Force solution

best_a <- -1000
best_b <- -1000
max_primes <- 0

abs_max <- 999
#abs_max <- 9

for(a in -abs_max:abs_max){
  for(b in -abs_max:abs_max){
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