# Find the largest pandigital prime
# Max pandigital number is 987654321

# Pandigital numbers are a permutation of 1:n, so generate all permutations for n
# Then check each for primeness, work from largest to smallest and stop after the first
# prime

# Solves in about 4 seconds. Main culprit for slowness is probably the prime checking

permutations <- function(n){
  perms <- matrix(data=0,nrow=factorial(n),ncol=n)
  perms[1,] <- 1:n
  curr <- 1:n
  for(i in 2:factorial(n)){
    m <- n - 1
    while(curr[m] > curr[m+1]){
      m <- m - 1
    }
    k <- n
    while( curr[m] > curr[k]){
      k <- k - 1
    }
    
    cm <- curr[m]
    ck <- curr[k]
    curr[m] <- ck
    curr[k] <- cm
    
    p <- m + 1
    q <- n
    
    while(p < q){
      cp <- curr[p]
      cq <- curr[q]
      curr[p] <- cq
      curr[q] <- cp
      p <- p + 1
      q <- q - 1
    }
    
    perms[i,] <- curr
  }
  return(perms)
}

# Skips any permutation whose last digit is 2,4,5,6,8 (excepting single digit numbers)
# Skips any permutation divisible by 3 (sum of digits %% 3 == 0)
largest <- 0
for(ndigit in 9:1){
  tens <- 10^((ndigit-1):0)
  perms <- permutations(ndigit)
  for(i in factorial(ndigit):1){
    if(!any(perms[i,ndigit]==c(2,4,5,6,8)) & ndigit > 1){
      if(sum(perms[i,]) %% 3 != 0){
        num <- sum(perms[i,] * tens)
        if(sum(num %% seq.int(5,ceiling(sqrt(num)),2) ==0)==0){
          largest <- num
          break
        }
      }
    }
  }
  if(largest != 0){
    break
  }
}
print(largest)
