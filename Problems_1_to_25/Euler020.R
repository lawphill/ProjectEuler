# Calculate the sum of all digits in 100!
library("gmp") # For dealing with large numbers

# Solution adapted from problem 16
# Had to create a factorial function in order to ensure it returned as a large
# integer correctly

fact <- function(n){
  n <- as.bigz(n)
  if(n <= 1){
    return(1)
  }
  return( as.bigz(n * fact(n-1)) )
}
num <-fact(100)


result <- 0
while(num > 0){
  result <- result + (num %% 10)
  num <- num %/% 10
}
print(result)
