# It can be seen that the number, 125874, and its double, 251748, contain exactly the same digits,
#   but in a different order.

# Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x, contain the same digits.

# Needs to start with 1, otherwise 6x would have too many digits


i <- 100L
found <- 0
ndigits <- ceiling(log10(i+1))
while(found==0){
  if(ndigits != ceiling(log10(i*6+1))){
    i <- i*6L-2L # Go from 17->100,167->1000,1667->10000
    ndigits <- ceiling(log10(i+1))
    next
  }
  if(ndigits < 5){
    if(!any((i %% 10) != c(0,5))){
      i <- i + 1
      next
    }
  }else if(ndigits < 6){
    if(!any((i %% 10) != c(0,2,4,5,6,8))){
      i <- i + 1
      next
    }
  }
  
  first_digits <- sort((i %/% 10^((nchar(i)-1):0)) %% 10)
  if(all(vapply(i*2:6,function(z) all(sort((z %/% 10^((ndigits-1):0)) %% 10)==first_digits),FUN.VALUE=FALSE))){
    print(i)
    found <-1
    break
  }

  i <- i + 1L
}