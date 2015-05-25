# A circular prime is a number that stays prime when its digits are rotated
#   e.g. 197, 971, and 719 are all prime
# Below 100 there are 13 such primes:
#   2,3,5,7,11,13,17,31,37,71,73,79,97
# How many circular primes are there below 1 million?

# First, find all primes below 1 million, taken from Problem #10
# Remove any numbers (over 10) with 2,4,5,6,8 as digits, since when rotated each
#   of those will produce at least one non-prime

max_num <- 999999L
all_nums <- c(1L:(max_num+1))
nchars <- ceiling(log10(all_nums + 1))
all_nums[1] <- 0L

vec1 <- 10^(1:7)
vec2 <- vec1/10

for(i in 2L:max_num){
  if(all_nums[i] != 0){
    all_nums[seq.int(min(max_num+1,i*2),length(all_nums),i)] <- 0
    digits <- (i %% vec1[nchars[i]:1]) %/% vec2[nchars[i]:1]
    if(sum(digits %% 2 == 0 | digits == 5) != 0 & nchars[i] > 1){
      all_nums[i] <- 0
    }
  }
}

all_primes <- all_nums[all_nums!=0]
nchars <- ceiling(log10(all_primes + 1))

# Check each to see if shifted digits are also prime
circular_primes <- 0

for(i in 1:length(all_primes)){
  num <- all_primes[i]
  
  if(nchars[i]==1){
    circular_primes <- circular_primes + 1
  }else{ # Only check if all digits are odd
    shifts <- 1
    not_prime <- 0
    while(shifts < nchars[i] & not_prime == 0){
      num <- (num %/% 10) + (num %% 10) * vec2[nchars[i]]
      if(num %in% all_primes == FALSE){
        not_prime <- 1
      }
      shifts <- shifts + 1
    }
    if(not_prime == 0){
      circular_primes <- circular_primes + 1
    }
  }
}