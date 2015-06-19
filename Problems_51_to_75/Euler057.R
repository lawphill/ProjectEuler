# It is possible to show that the square root of two can be expressed as an infinite continued fraction.
#   sqrt(2) = 1 + 1/(2 + 1/(2 + 1/(2 + ... ))) = 1.414213...
# By expanding this for the first four iterations, we get:
#   1 + 1/2 = 3/2 = 1.5
#   1 + 1/(2 + 1/2) = 7/5 = 1.4
#   1 + 1/(2 + 1/(2 + 1/2)) = 17/12 = 1.41666...
#   1 + 1/(2 + 1/(2 + 1/(2 + 1/2))) = 41/29 = 1.41379...

# The next three expansions are 99/70, 239/169, and 577/408, but the eighth expansion, 1393/985, is the
# first example where the number of digits in the numerator exceeds the number of digits in the
# denominator.

# In the first one-thousand expansions, how many fractions contain a numerator with more digits than
# denominator?

# LAP: The key insight is that numerator[1] = 1, demoninator[1] = 1,
# but that denominator[i] = numerator[i-1] + denominator[i-1]
# and that numerator[i] = denominator[i] + denominator[i-1]
library("gmp")

max_iters <- 1000
fractions <- matrix(data=0,ncol=max_iters,nrow=2)
digits <- matrix(data=0,ncol=max_iters,nrow=2)
numerator <- as.bigz(3)
denominator <- as.bigz(2)

numerator_moredigits <- 0
for(i in 2:max_iters){
  old_denom <- denominator
  denominator <- add.bigz(denominator,numerator)
  numerator <- add.bigz(denominator,old_denom)
  
  if(ceiling(log10(add.bigz(numerator,1))) > ceiling(log10(add.bigz(denominator,1)))){
    numerator_moredigits <- numerator_moredigits + 1
  }
}
print(numerator_moredigits)