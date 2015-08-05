# Let p(n) represent the number of different ways in which n coins can be separated into piles.
#   For example, five coins can be separated into piles in exactly seven different ways, so p(5)=7.
#   xxxxx
#   xxxx  x
#   xxx xx
#   xxx x x
#   xx  xx  x
#   xx  x x x 
#   x x x x x

# Very similar to the sum problem previously
# The trick here is finding a way to store the results which can be used efficiently to find the modulus
# We don't need to know anything beyond the first 6 digits, so don't store more than that
# Efficiently calculate p(n) using Euler's recurrent formula. We only need to calculate sqrt(n) terms

euler <- function(n){
  if(n < 0){ return(0) }
  if(n == 0){ return(1) }
  if(n == 1){ return(1) }
  
  sqrtn <- floor(sqrt(n)) # This represents the maximum number of indices
  first_term <- n - 1/2 * (1:sqrtn) * (3*(1:sqrtn) + 1)
  second_term <- n - 1/2 * (1:sqrtn) * (3*(1:sqrtn) - 1)
  first_term <- lookup(first_term)
  second_term <- lookup(second_term)

  return( sum((-1)^((1:sqrtn)+1) * (first_term + second_term) ))
}

lookup <- function(n){
  vals <- rep(0,length(n))
  if(any(n == 0)){
    vals[n==0] <- 1
  }
  vals[n > 0] <- rewrites[n[n > 0]]
  return(vals)
}



maxN <- 100000L
modulo <- 1000000L
rewrites <- rep(0,maxN)
curr <- 1L
rewrites[1] <- euler(curr)
while(rewrites[curr] != 0){
  if(curr > maxN){
    print("Reached cap: Increase maxN")
    break
  }
  curr <- curr + 1L
  rewrites[curr] <- euler(curr) %% modulo
}
print(curr)