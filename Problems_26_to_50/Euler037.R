# Truncatable Primes are primes which remain prime as you remove digits 
#   from the left or right
#   e.g. 3797 -> 797 -> 97 -> 7, all prime
#     also, 3797 -> 379 -> 37 -> 3, all prime

# Find the sum of all truncatable primes (hint: there are only 11). Single digit primes
# don't count

# Solution: Find all primes (under a certain size)
# Then go through each prime and check if it's truncatable
# We can throw out any case where the first or last digit is non-prime
# We can also throw out if any middle digit which is 0,2,4,5,6,8
# Otherwise, we'll go through and specifically check each trunactation for primeness
# Runs pretty efficiently, nothing to write home about

found <- 0
to_find <- 11
trunc_primes <- rep(0,to_find)

vec1 <- 10^(1:9)
vec2 <- vec1/10

# Find Primes
max_num <- 999999L
all_nums <- c(1L:(max_num+1))
all_nums[1] <- 0L

for(i in 2L:max_num){
  if(all_nums[i] != 0){
    all_nums[seq.int(min(max_num+1,i*2),length(all_nums),i)] <- 0
  }
}

primes <- all_nums[all_nums!=0]
  
index <- 5 # ignore 2,3,5,&7
while(found < to_find & index < length(primes)){
  curr <- primes[index]  
  nchar <- ceiling(log10(curr+1))
  digits <- (curr %% vec1[nchar:1]) %/% vec2[nchar:1]
  all_prime <- TRUE # assume all prime until proven otherwise
  if(nchar > 2){
    for(i in (nchar-1):2){
      if(any(digits[i]==c(0,2,4,5,6,8))){
        all_prime <- FALSE # If last digit is one of above, it's not prime
      }
    }
  }
  if(any(digits[1]==c(1,4,6,8,9)) | any(digits[nchar]==c(0,1,2,4,6,8,9))){
    all_prime <- FALSE
  }
  # Check removing from left
  left_index <- 2
  while(left_index<nchar & all_prime==TRUE){
    num <- sum(digits[left_index:nchar] * vec2[(nchar-left_index+1):1])
    if(!any(primes==num)){
      all_prime <- FALSE
    }
    left_index <- left_index + 1
  }
  
  # Check removing from right
  right_index <- nchar-1
  while(right_index > 1 & all_prime==TRUE){
    num <- sum(digits[1:right_index] * vec2[right_index:1])
    if(!any(primes==num)){
      all_prime <- FALSE
    }
    right_index <- right_index - 1
  }
  
  if(all_prime == TRUE){
    found <- found + 1
    trunc_primes[found] <- curr
  }
  index <- index + 1
}
print(sum(trunc_primes))