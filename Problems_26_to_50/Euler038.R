# FROM PROMPT:
#Take the number 192 and multiply it by each of 1, 2, and 3:
  
#  192 × 1 = 192
#  192 × 2 = 384
#  192 × 3 = 576
#By concatenating each product we get the 1 to 9 pandigital, 192384576. We will call
#   192384576 the concatenated product of 192 and (1,2,3)
#The same can be achieved by starting with 9 and multiplying by 1, 2, 3, 4, and 5,
#   giving the pandigital, 918273645, which is the concatenated product of 9 and (1,2,3,4,5).

#What is the largest 1 to 9 pandigital 9-digit number that can be formed as the concatenated
#   product of an integer with (1,2, ... , n) where n > 1?

# So, integer needs to begin with 9, so that 9abc x 1 begins the pandigital number off as
#   large as possible.
# Max base integer is 4 digits, since n > 1, the pandigital number has to be a concatenation
#   of abcd and (abcd * 2)

# FROM EULER #32
no_repeats <- function(x,missing_digits){
  # Returns BOOLEAN
  # TRUE if x has no repeats and no digit is 0
  # FALSE if x has repeat digits or any digits is 0
  r <- rep(TRUE,length(x))
  for(i in 1:length(x)){
    dont_match <- missing_digits
    while(x[i] > 0){
      mod <- x[i] %% 10
      if(mod == 0){
        r[i] <- FALSE
      }else if(sum(match(dont_match,mod),na.rm=TRUE) > 0){
        r[i] <- FALSE
      }
      dont_match <- c(dont_match,mod)
      x[i] <- x[i] %/% 10
    }
  }
  return(r)
}

digits <- function(x){
  d <- c()
  while(x > 0){
    d <- c(d,x %% 10)
    x <- x %/% 10
  }
  return(rev(d))
}

max_found <- 0
for(i in 1:9999){
  no_reps <- TRUE
  n <- 0
  d <- c()
  while(no_reps){
    d <- c(d,digits(i * n))
    n <- n + 1
    no_reps <- no_repeats(i * n,d)
  }
  n <- n - 1
  
  if(length(d) == 9 & n > 1){
    pandigital_num <- sum(d * 10^(8:0))
    if(pandigital_num > max_found){
      max_base <- i
      max_found <- pandigital_num
      max_n <- n
    }
  }
}