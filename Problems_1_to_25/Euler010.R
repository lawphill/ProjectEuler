# Find the sum of all primes below 2 million
# Create vector of all numbers, when we find a prime, add to the sum and remove
# all multiples of that prime.
# Since all numbers are multiples of primes, this removes all non-primes as we go
# so that we don't waste resources checking if they're prime

max_num <- 2000000L
all_nums <- c(1L:max_num)
all_nums[1] <- 0L
sum_of_primes <- 0

for(i in 2L:max_num){
  if(all_nums[i] != 0){
    sum_of_primes <- sum_of_primes + i
    all_nums[seq.int(i,length(all_nums),i)] <- 0
  }
}
print(sum_of_primes)