# The arithmetic sequence, 1487, 4817, 8147, in which each of the terms increases by 3330, is unusual in
#   two ways: (i) each of the three terms are prime, and, (ii) each of the 4-digit numbers are
#   permutations of one another.

# There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes, exhibiting this property, 
#   but there is one other 4-digit increasing sequence.

# What 12-digit number do you form by concatenating the three terms in this sequence?

# NOTE, TERRIBLE TERRIBLE CODE. BUT IT WORKS

max_primes <- 9999
primes <- 1:max_primes
primes[1] <-0
for(i in 2:floor(max_primes/2)){
  if(primes[i] != 0){
    primes[seq.int(i*2,max_primes,i)] <- 0
  }
}
primes <- primes[primes!=0 & primes > 1000]
indices <- 1:length(primes)

prime_digits <- matrix(data=0,nrow=length(primes),ncol=4)
for(i in 1:ncol(prime_digits)){
  prime_digits[,i] <- (primes %/% 10^(4-i)) %% 10
}
sorted_primes <- t(apply(prime_digits,1,sort))

for(i in 1:nrow(sorted_primes)){
  ind <- indices[apply(sorted_primes,1,function(j) all(j==sorted_primes[i,]))]
  if(length(ind) >= 3){
    p <- primes[ind]
    diffmat <- matrix(data=0,ncol=length(p),nrow=length(p))
    for(nr in 1:length(p)){
      for(nc in nr:length(p)){
        diffmat[nr,nc] <- p[nc] - p[nr]
      }
    }
    for(col in 3:length(p)){
      for(val in diffmat[diffmat[,col]!=0,col]){
        if(any(diffmat[,col]==val*2) & !(8147 %in% p)){
          print(rev(c(p[col],p[col]-val,p[col]-val*2)))
        }
      }
    }
    
  }
}
