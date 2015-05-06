# Using the collatz sequence, find the longest sequence for a starting number
# under 1 million
# Seq:
#   n -> n/2 (n is even)
#   n -> 3n + 1 (n is odd)

max_n <- 999999
collatz_nums <- rep(0,max_n)
collatz_nums[1] <- 1

for(i in 1:max_n){
  if(collatz_nums[i]==0){
    collatz_nums[i] <- collatz(i)
  }
}
max_collatz <- match(max(collatz_nums),collatz_nums)

collatz <- function(n){
  if(n <= max_n){
    if(collatz_nums[n]!=0){ # We already calculated
      return(collatz_nums[n])
    }
  }
  if(n==1){
    return(1)
  }else{
    seq_length <- collatz(next_collatz(n))+1
    if(n <= max_n){
      collatz_nums[n] <<- seq_length
    }
    return(seq_length)
  }
}

next_collatz <- function(n){
  if((n%%2)==0){
    return(n/2)
  }else{
    return((n*3)+1)
  }
}