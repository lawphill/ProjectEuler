# It is well known that if the square root of a natural number is not an integer, then it is irrational.
#   The decimal expansion of such square roots is infinite without any repeating pattern at all.

# The square root of two is 1.41421356237309504880..., and the digital sum of the first one hundred
#   decimal digits is 475.

# For the first one hundred natural numbers, find the total of the digital sums of the first one hundred
#   decimal digits for all the irrational square roots.

# Solved by making a vector with the necessary number of digits. Start with a lower bound. Make a guess
#   at the next digit and if it's larger than N, choose something smaller and try again. Once we pass
#   our test, increment to next digit to find. Solves in ~1 minute.
# Implemented with a vector of digits so that R doesn't have to deal with large numbers

# NOT USED
addition <- function(x,y){
  lx <- length(x)
  ly <- length(y)
  x <- rev(x)
  y <- rev(y)
  
  new_digits <- rep(0,max(lx,ly))
  remainder <- 0
  for(i in 1:max(lx,ly)){
    summed <- remainder
    if(i <= lx){
      summed <- summed + x[i]
    }
    if(i <= ly){
      summed <- summed + y[i]
    }
    new_digits[i] <- summed %% 10
    remainder <- (summed - new_digits[i]) %/% 10
  }
  while(remainder > 0){
    new_digits <- c(new_digits, remainder %% 10 )
    remainder <- remainder %/% 10
  }
  return(rev(new_digits))
}


multiply <- function(a,b){
  a <- rev(a)
  b <- rev(b)
  # MULTIPLE TWO VECTORS OF DIGITS a AND b
  prod <- rep(0,length(a)+length(b)) # May not use all, but this is the maximum length we might need
  for(i in 1:length(a)){
    prod[i:(i+length(b)-1)] <- prod[i:(i+length(b)-1)] + a[i]*b
  }
  
  # Deal with any remainders
  remainder <- 0
  for(i in 1:length(prod)){
    prod[i] <- prod[i] + remainder
    if(prod[i] > 9){
      remainder <- prod[i] %/% 10
      prod[i] <- prod[i] %% 10
    }else{
      remainder <- 0
    }
  }
  
  prod <- rev(prod)
  if(prod[1] == 0){
    return(prod[2:length(prod)])
  }else{
    return(prod)
  }
}

# NOT USED
babylonian <- function(n,precision){
  guess <- rep(0,precision+1)
  guess[1] <- floor(sqrt(n))
  
  for(i in 2:(precision+1)){
    guess[i] <- (guess[i-1] + n/guess[i-1]) / 2
  }
  return(guess)
}

precision <- 100 # number of digits to calculate
to_check <- 1:100
to_check <- to_check[! to_check %in% (1:10)^2]
length_m <- (precision-1)*2

total <- 0
for(n in to_check){
  guess <- rep(0,precision)
  digits_above_decimal <- ceiling(log10(n+1))
  guess[1:ceiling(log10(floor(sqrt(n))+1))] <- addition(c(floor(sqrt(n))),0)
  i <- 2
  new_digit <- 9
  while(i <= precision){
    if(new_digit < 0){ 
      print(new_digit)
      break
    }
    guess[i] <- new_digit
    m <- multiply(guess,guess)
    if(sum(m[1:(length(m)-length_m)]*10^(((length(m)-length_m)-1):0)) >= n){
      new_digit <- new_digit - 1
    }else{
      i <- i + 1
      new_digit <- 9
    }
  }
  print(c(n,sum(guess)))
  total <- total + sum(guess)
}
print(total)