# Using the collatz sequence, find the longest sequence for a starting number under 1 million
# Seq:
#   n -> n/2 (n is even)
#   n -> 3n + 1 (n is odd)

collatz <- function(n){
  if(n <= max_to_calc & collatz_nums[n]!=0){ # We already calculated
    return(collatz_nums[n])
  }
  seq_length <- collatz(next_collatz(n))+1
  if(n <= max_to_calc){
    collatz_nums[n] <<- seq_length
  }
  return(seq_length)
}

next_collatz <- function(n){
  if((n%%2)==0){
    return(n/2)
  }else{
    return((n*3)+1)
  }
}

max_n <- 999999
max_to_calc <- 3999999
collatz_nums <- rep(0,max_to_calc)
collatz_nums[1] <- 1
max_seq <- 0

for(i in 2:max_n){
  if(collatz_nums[i]==0){
    collatz_nums[i] <- collatz(i)
    if(collatz_nums[i] > max_seq){
      max_seq <- collatz_nums[i]
      max_start <- i
    }
  }
}
print(max_start)