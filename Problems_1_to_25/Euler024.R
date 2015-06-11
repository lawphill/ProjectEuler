# Number of permutations is n!
# Perms for 10 digits is 3,628,800
# Note: package 'permute' doesn't allow this many permutations

# Determine digit by digit
find_perm <- function(n,nums){
  # Find the nth permutation of a set "nums"
  # n should be 1-indexed (math itself is done using 0 indexing)
  # nums should be an ordered list such that the 1st permutation
  # returns nums itself
  #
  # Returns a string representing the nth lexicographic permutation
  
  n <- n - 1 
  ndigits <- length(nums)
  perm <- rep(0,ndigits)
  
  i <- 1
  while(i <= ndigits){
    digit_index <- (n %/% factorial(length(nums)-1)) + 1
    n <- n %% factorial(length(nums)-1)
    
    perm[i] <- nums[digit_index]
    nums <- nums[nums != perm[i]]
    
    i <- i + 1
  }
  
  return(paste(perm,sep="",collapse=""))
}

nums <- c(0:9)
n <- 1000000
print(find_perm(n,nums))