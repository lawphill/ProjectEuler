# The 5-digit number, 16807=7^5, is also a fifth power. Similarly, the 9-digit number, 134217728=8^9,
#   is a ninth power.

# How many n-digit positive integers exist which are also an nth power?

library("gmp")

max_power <- 100
count <- 0L
for(power in 1:max_power){
  min_num <- 10^(power-1)
  max_num <- 10^(power) - 1
  
  min_root <- ceiling(min_num ^ (1/power))
  max_root <- floor(max_num ^ (1/power)) # Will never be above 9, 10 would make too many digits
  
  # if 9 doesn't cut it, then there are no numbers to that power which make the correct number of digits
  if(min_root > 9){ 
    break
  }
  
  if(max_root > 9){
    max_root <- 9
  }
  
  count <- count + (max_root - min_root + 1)
}
print(count)