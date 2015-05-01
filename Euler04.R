# A palindromic number reads the same both ways.
# The largest palindrome made from the product of two 2-digit numbers
# is 9009 = 91 × 99.
#
# Find the largest palindrome made from the product of two 3-digit numbers.

max_num <- 999*999
min_num <-100*100
find_max_palindrome <- function(min_num,max_num){
  for(i in max_num:min_num){
    y <- unlist(strsplit(toString(i),split=""))
    if(sum(y==rev(y)) == nchar(i)){ # Check if palindrome
      if(has_threedigit_factor(i)){
        return(i)
      }
    }
  }
  return(0)
}

has_threedigit_factor <- function(x){
  if(x<=10000){ return(0) }
  
  for(i in floor(sqrt(x)):100){
    if(x %% i == 0){
      if(nchar(x/i)==3){
        return(1)
      }
    }
  }
  return(0)
}