# The proper divisors of a number are all the divisors excluding the number itself.
#   For example, the proper divisors of 28 are 1, 2, 4, 7, and 14. As the sum of
#   these divisors is equal to 28, we call it a perfect number.

# Interestingly the sum of the proper divisors of 220 is 284 and the sum of the
#   proper divisors of 284 is 220, forming a chain of two numbers. For this reason,
#   220 and 284 are called an amicable pair.

# Perhaps less well known are longer chains. For example, starting with 12496, we
#   form a chain of five numbers:  
#   12496 → 14288 → 15472 → 14536 → 14264 (→ 12496 → ...)

# Since this chain returns to its starting point, it is called an amicable chain.

# Find the smallest member of the longest amicable chain with no element exceeding
#   one million.

divisor_sum <- function(n){
  if(n > length(dsums)){
    return(1)
  }else if(dsums[n] == 0){
    dsums[n] <<- alt_divisor_sum(n)
  }
  return(dsums[n])
}

alt_divisor_sum <- function(n){
  if(n < 4){ return(1) }
  maxD <- floor(sqrt(n))
  sum <- 1
  for(i in 2:maxD){
    if((n %% i) == 0){
      sum <- sum + i
      d = n/i
      if(d != i){
        sum <- sum + d
      }
    }
  }
  return(sum)
}

chain_max <- 999999
curr_max <- 0
curr_min_value <- 0
dsums <- rep(1,chain_max)
# Calculate divisor sums
for(i in 2:floor(chain_max/2)){
  dsums[seq.int(2*i,chain_max,i)] <- dsums[seq.int(2*i,chain_max,i)] + i
}

chain_lengths <- rep(0,chain_max)
chain_lengths[which(dsums==1)] <- 1

for(i in 2:chain_max){
  curr_val <- dsums[i]
  chain <- c(i)
  while(curr_val <= chain_max
        && chain_lengths[curr_val] == 0
        && ! curr_val %in% chain){
    chain <- c(chain,curr_val)
    curr_val <- dsums[curr_val]
  }
  if(curr_val > chain_max){
    for(val in chain){
      chain_lengths[val] <- 1
    }
    next
  }else if(curr_val == i){
    if(length(chain) > curr_max){
      curr_max <- length(chain)
      curr_min_value <- i
    }
    for(val in chain){
      chain_lengths[val] <- length(chain)
    }
  }
}
print(curr_min_value)
