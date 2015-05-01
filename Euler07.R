# Find the 10001st prime number

find_prime <- function(x){
  primes <- rep(0,x)
  primes[1]<-2
  curr_index <- 2
  curr_num <- 3
  while(curr_index <= x){
    poss_primes <- primes[primes != 0]
    if(is_prime(curr_num,poss_primes[poss_primes <= sqrt(curr_num)])){ # find new primes
      primes[curr_index] <- curr_num
      curr_index <- curr_index + 1
    }
    curr_num <- curr_num + 2
  }
  return(primes[x])
}

is_prime <- function(x,primes){
  remainders <- x%%primes
  if(sum(remainders==0)==0){
    return(TRUE)
  }else{ return(FALSE)}
}