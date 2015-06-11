# Calculate the sum of all digits for 2^1000
# i.e. 2^15 = 32768, sum of digits = 3+2+7+6+8 = 26
library("gmp") # For dealing with large numbers


# Originally, I was trying to avoid using a library for big integers
# So, the below functions work as a way of using modular arithmetic to grab
# Each digit and sum them
# The modular arithmetic fails, however, when the number of digits to grab is
# sufficiently large. 2^100 calculates fine, but above that things break down
# Left the code as a reminder of how the modular approach would theoretically
# work


digitsum <- function(base,exponent){
  total_sum <- 0
  for(k in 1:ndigits(base^exponent)){
    first_digits <- as.bigz(firstKdigits(base,exponent,k))
    #print(first_digits)
    if(ndigits(first_digits)==k){ # Make sure there's not a leading 0
      total_sum <- total_sum + as.bigz(substring(as.character(first_digits),1,1))
      #print("Counted")
    }
  }
  return(total_sum)
}

ndigits <- function(n){
  return(floor(log10(n))+1)
}

modExpon <- function(base,exponent,mod){
  result <- 1
  for(i in 0:(exponent-1)){
    result <- (as.bigz(result) * as.bigz(base)) %% as.bigz(mod)
  }
  return(result)
}

firstKdigits <- function(base,exponent,k){
  return(modExpon(base,exponent,10^k))
}

num <- as.bigz(2^1000)
result <- 0
while(num > 0){
  result <- result + (num %% 10)
  num <- num %/% 10
}
print(result)
