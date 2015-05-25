# Find sum of all numbers which are the sum of their digits factorial.
#   e.g. 145 = 1! + 4! + 5!

# Brute force solution. Slow, takes about 14sec

facts <- factorial(0:9)
min_num <- 10
max_num <- facts[10]*7 # Any digit larger is so large it can't be the sum of its digits
nums <- c()
total <- 0

vec1 <- 10^(1:7)
vec2 <- vec1/10

for(i in min_num:max_num){
  nchar <- ceiling(log10(i))
  digit_sum <- sum(facts[(i %% vec1[1:nchar]) %/% vec2[1:nchar] + 1])
  if(digit_sum == i){
    total <- total + i
    nums <- c(nums,i)
  }
}