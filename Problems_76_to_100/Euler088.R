# A natural number, N, that can be written as the sum and product of a given set of at least two natural
#   numbers, {a1, a2, ... , ak} is called a product-sum number: N = a1 + a2 + ... + ak =
#   a1 × a2 × ... × ak.

# For example, 6 = 1 + 2 + 3 = 1 × 2 × 3.

# For a given set of size, k, we shall call the smallest N with this property a minimal product-sum
#   number. The minimal product-sum numbers for sets of size, k = 2, 3, 4, 5, and 6 are as follows.

# k=2: 4 = 2 × 2 = 2 + 2
# k=3: 6 = 1 × 2 × 3 = 1 + 2 + 3
# k=4: 8 = 1 × 1 × 2 × 4 = 1 + 1 + 2 + 4
# k=5: 8 = 1 × 1 × 2 × 2 × 2 = 1 + 1 + 2 + 2 + 2
# k=6: 12 = 1 × 1 × 1 × 1 × 2 × 6 = 1 + 1 + 1 + 1 + 2 + 6

# Hence for 2≤k≤6, the sum of all the minimal product-sum numbers is 4+6+8+12 = 30; note that 8 is only
#   counted once in the sum.

# In fact, as the complete set of minimal product-sum numbers for 2≤k≤12 is {4, 6, 8, 12, 15, 16}, the
#   sum is 61.

# What is the sum of all the minimal product-sum numbers for 2≤k≤12000?

# Maximally, the minimal product-sum will be 2*k, this is in the case where a = 1,1,...1,2,k
#   It can be lower, but no less than k
#   k=7, 1,1,1,1,1,3,4 = 12 < 2*k = 14
#   k=8, 1,1,1,1,1,2,2,3 = 12 < 2*k = 16
#   k=9, 1,1,1,1,1,1,1,3,5 = 15 < 2*k = 18
#   k=10, 1,1,1,1,1,1,1,1,4,4 = 16 < 2*k = 20
#   k=11, 1,1,1,1,1,1,1,1,2,2,4 = 16 < 2*k = 22
#   k=12, 1,1,1,1,1,1,1,1,2,2,2,2 = 16 < 2*k = 24

# LAP: One option is to go through all the factorizations of an integer and keep track of the lowest value
#   for each k
source("/media/Shared_Data/Euler/prime_sieve.R")


return_sums <- function(n){ # Return a list of paired values
  # 1st value = sum of integers whose product is n
  # 2nd value = number of integers summed
  sums <- list()
  curr <- 0
  
  for(x in 2:floor(sqrt(n))){
    if((n%%x) == 0){
      curr <- curr + 1
      sums[[curr]] <- c(sum(x,n/x),2)
      if(length(sum_list[[n/x]]) > 0){
        for(i in 1:length(sum_list[[n/x]])){
          curr <- curr + 1
          sums[[curr]] <- c(x+sum_list[[n/x]][[i]][1],1+sum_list[[n/x]][[i]][2])
        }
      }
    }
  }
  return(unique(sums))
}

maxK <- 12000
primes <- sieve(maxK*2)
sum_list <- vector("list",maxK*2) # List of possible summations leading to the proper product

min_prodsums <- (1:maxK)*2 # Array for results
min_prodsums[1] <- 0

to_check <- 2:(maxK*2)
to_check <- to_check[!to_check %in% primes]
for(i in to_check){
  sum_list[[i]] <- return_sums(i)
  for(j in 1:length(sum_list[[i]])){ # For each possible summation
    k <- i - sum_list[[i]][[j]][1] + sum_list[[i]][[j]][2]
    if(k <= maxK){
      min_prodsums[k] <- min(c(min_prodsums[k],i))
    }
  }
}
#print(min_prodsums)
print(sum(unique(min_prodsums)))