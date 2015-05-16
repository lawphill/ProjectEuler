# Find the sum of all numbers that can be written as the sum of their digits
# raised to the 5th power
# i.e. 123 -> 1^5 + 2^5 + 3^5 = 276 which doesn't equal 123, so doesn't count

# Brute force solution, go through all possible numbers and check
# I feel like we could design it so that instead we went through each possible
# number of digits (2:6) and listed out all possible combinations of digits for
# that total number and checking those might result in fewer operations. Not sure.

raised_digits <- (0:9)^5

start_num <- 10
end_num <- 999999
# We can prove that any number with more than 6 digits can't be matched.
# For a 7 digit number, the max we can get is (9^5)*7 = 413,343, that's not big
# enough to make a 7 digit number, the minimum of which would need to sum to 
# 1,000,000. The problem gets worse the more digits you have. So we can feel
# Safe in limiting our search to numbers with fewer than 7 digits.

total <- 0
is_equal <- rep(0,length(start_num:end_num))
for(n in start_num:end_num){
  number_sum <- 0
  n_tmp <- n
  while(n_tmp > 0){
    number_sum <- number_sum + raised_digits[(n_tmp %% 10)+1]
    n_tmp <- n_tmp %/% 10
  }
  if(number_sum == n){
    total <- total + n
    is_equal[n] <- n
  }
}



