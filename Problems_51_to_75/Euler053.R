# There are exactly ten ways of selecting three from five, 12345:
#   123, 124, 125, 134, 135, 145, 234, 235, 245, and 345
# In combinatorics, we use the notation, 5C3 = 10.

# In general,
#   nCr =  n! / (r!(n−r)!) , where r ≤ n, n! = n×(n−1)×...×3×2×1, and 0! = 1.

# It is not until n = 23, that a value exceeds one-million: 23C10 = 1144066.
# How many, not necessarily distinct, values of  nCr, for 1 ≤ n ≤ 100, are greater than one-million?

max_n <- 100
factorials <- factorial(0:max_n)

thresh <- 1000000
over_thresh <- 0
for(n in 23:max_n){
  for(r in n:1){
    nCr <- factorials[n+1] / (factorials[r+1] * factorials[n-r+1]) # +1 is just for indexing
    if(nCr >= thresh){
      over_thresh <- over_thresh + 1
    }
  }
}