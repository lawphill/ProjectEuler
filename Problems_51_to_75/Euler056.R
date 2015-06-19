# A googol (10^100) is a massive number: one followed by one-hundred zeros; 100^100 is almost unimaginably
#   large: one followed by two-hundred zeros. Despite their size, the sum of the digits in each number is
#   only 1.
# Considering natural numbers of the form, a^b, where a, b < 100, what is the maximum digital sum?

get_digits <- function(x){ (x %/% 10^((ceiling(log10(x+1))-1):0)) %% 10 }

find_max <- function(a,b){
  # RETURN MAX DIGITSUM OF a^1:b
  # a SHOULD ALREADY BE A VECTOR
  # b IS AN INTEGER
  
  max_sum <- sum(a)
  if(b<=1){ return(max_sum) }
  
  prod <- a  
  for(iter in 2:b){ # Each time multiply by a
    prod <- multiply(prod,a)
    if(sum(prod) > max_sum){
      max_sum <- sum(prod)
    }
  }
  
  return(max_sum)
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


max_num <- 99
max_sum <- 0
digits <- lapply(1:max_num,get_digits)
print(max(sapply(1:max_num, function(x) find_max(digits[[x]],max_num))))