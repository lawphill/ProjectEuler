# British coins are 1p, 2p, 5p, 10p, 20p, 50p, 100p (1pound), 200p (2pounds)
# How many ways are there to combine any number of coins to get 200p
#
# Combinations, not permutations

# DP algorithm. Save calculated values in matrix. Scales poorly as total increases. Not sure
# if due to a problem in the algorithm, or the way in which previous values are stored
# I've opted to use a matrix so that I can distinguish between the number of ways to create
# 200p while limiting what the largest coin available is. Described in more detail below.

coins <- c(1,2,5,10,20,50,100,200)
total <- 200

# Matrix for partial results
# row <- current value
# col <- max coin available, necessary b/c we can need to make 200 coins and have 50p pieces
#   available, or we can need to make 200 coins, but only have 1p available. Different
#   answers in each case.
combs <- matrix(data=0,nrow=curr_val,ncol=length(coins))
combs[,1] <- 1 # Only 1 way to solve for 1p

coins_possible <- function(x,coin_ind){
  # x <- number of coins left
  if(combs[x,coin_ind] != 0){
    return(combs[x,coin_ind])
  }
  # Possible remaining values once we use the current coin
  poss <- x - coins[coin_ind]*(0:(x %/% coins[coin_ind]))
  
  n_combs <- 0
  for(i in 1:length(poss)){
    if(poss[i] != 0){
      n_combs <- n_combs + coins_possible(poss[i],coin_ind-1)
    }else{
      n_combs <- n_combs + 1
    }
  }
  combs[x,coin_ind] <<- n_combs
  return(n_combs)
}

print(coins_possible(total,length(coins)))