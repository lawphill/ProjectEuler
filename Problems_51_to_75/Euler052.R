# It can be seen that the number, 125874, and its double, 251748, contain exactly the same digits,
#   but in a different order.

# Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x, contain the same digits.

# Needs to start with 1, otherwise 6x would have too many digits


i <- 10L
found <- 0
while(found==0){
  if(i %/% 10^(nchar(i)-1) != 1){
    i <- i*5L # Go from 20->100, 200->1000, 2000->10000, etc.
    next
  }
  
  x <- i*1:6 # numbers to check
  digits<-lapply(x, function(i) sort((i %/% 10^((nchar(i)-1):0)) %% 10))
  
  if(all(lapply(digits, function(x) all(x == digits[[1]])))){
    print(i)
    found <- 1
    break
  }
  
  i <- i + 1L
}