# A unit fraction is 1/d, some have repeating sequences 0.(3), 0.1(6), 0.(142857)
# For d < 1000, find the value of d which has the longest repeating sequence

# The length of the sequence is equally to the Multiplicative Order of 10 mod d
# It seems like Multiplicative Order of A mod B is defined as such:
# A^0 = 1, = 0*B + 1 -> 1
# A^1 = A, = x*B + y -> y
# A^k = x*B + y
# keep this up, return first non-zero k where A^k = 1 mod B

# Code runs well enough for searching 2:999
# Runs very slowly for 2:9999

gcd_func <- function(a,b){
  divisible_a <- a %% 1:a
  divisible_b <- b %% 1:b

  common <- 1
  for(i in 2:min(a,b)){
    if((divisible_a[i] == 0) && (divisible_b[i] == 0)){
      common <- i
    }
  }
  return(common)
}

MultOrd <- function(a,b){
  k <- 1
  actual_mod <- a %% b
  while(actual_mod != 1){
    # Since k is sometimes very large, modulo operator breaks down (even with gmp)
    # Instead, we rely on the fact that a^2 %% b = ( (a %% b) ^ 2 ) %% b
    k <- k + 1
    actual_mod <- (actual_mod * a) %% b
  }
  return(k)
}

max_d <- 9999
longest_remainder <- 0
index <- 0
remainders <- rep(-1,max_d)
remainders[1] <- 0
# No repeating digits for multiples of 2 or 5 (the factors of 10)
remainders[(1:max_d %% 2) == 0] <- 0 
remainders[(1:max_d %% 5) == 0] <- 0

for(d in 2:max_d){
  if(remainders[d] == -1){
    if(gcd_func(10,d)==1){
      r <- MultOrd(10,d)
    }
    
    if( r > longest_remainder){
      longest_remainder <- r
      index <- d
    }
    for(i in 1:(max_d %/% d)){
      # For all multiples, if r is greater than previous value, replace it
      if(remainders[d*i] < r){
        remainders[d*i] <- r
      }
    }
  }
}
print(which.max(remainders))