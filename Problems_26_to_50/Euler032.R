# A number is pandigital if it is made up of the digits 1:n, where n is the total number
# of digits in the number. For example, 15234 is a 5-digit pandigital number b/c  it contains
# 1:5 and is 5 digits long. Each digit can only appear once.
#
# 7254 is a pandigital product in that 39 x 186 = 7254, where the number and two of its
#   factors are together 1:9 pandigital (i.e. contain 1:9 each exactly once)
#
# Find the sum of all products (e.g. 7254) which are pandigital products of 1:9

# Only logical possibilities would be:
# x * xxxx = xxxx
# xx * xxx = xxxx

# Note to self: This code is terrifyingly bad. Like, I wrote this way too late at night.
# Clean it up on the morrow.

poss_products <- rep(0,9876)

no_repeats <- function(x,missing_digits){
  digits <- 1:9
  digits[missing_digits] <- 0
  missing <- rep(0,9)
  while(x > 0){
    if((x %% 10) == 0){
      return(NA)
    }else if(digits[x %% 10]==0){
      return(NA)
    }
    digits[x %% 10] <- 0
    missing[x %% 10] <- x %% 10
    x <- x %/% 10
  }
  if(length(missing) > 0){
    return(missing[missing!=0])
  }else{
    return(0)
  }
}

for(product in 1234:9876){
  used <- no_repeats(product,c())
  if(!is.na(used)){
    for(factor1 in 1:89){
      if((product %% factor1) == 0){
        used2 <- no_repeats(factor1,used)
        if(!is.na(used2)){
          factor2 <- product / factor1
          used3 <- no_repeats(factor2,c(used,used2))
          if(!is.na(used3)){
            if(length(c(used,used2,used3))==9){
              poss_products[product] <- product
            }
          }
        }
      }
    }
  }
}
