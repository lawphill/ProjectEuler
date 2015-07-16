# Consider quadratic Diophantine equations of the form:
#  x^2 – Dy^2 = 1

# For example, when D=13, the minimal solution in x is 649^2 – 13×180^2 = 1.
# It can be assumed that there are no solutions in positive integers when D is square.
# By finding minimal solutions in x for D = {2, 3, 5, 6, 7}, we obtain the following:
  
# 3^2 – 2×2^2 = 1
# 2^2 – 3×1^2 = 1
# 9^2 – 5×4^2 = 1
# 5^2 – 6×2^2 = 1
# 8^2 – 7×3^2 = 1

# Hence, by considering minimal solutions in x for D ≤ 7, the largest x is obtained when D=5.

# Find the value of D ≤ 1000 in minimal solutions of x for which the largest value of x is obtained.

library("gmp")

# A modification of the code from 64 & 65, x and y have to be a pair of convergents of sqrt(D), so go
#   through all the convergents, checking for a solution

max_val <- 1000

to_check <- 2:max_val
to_check <- to_check[(sqrt(to_check) %% 1) != 0]
max_num <- 0
max_x <- 0
for(num in to_check){
  a0 <- sqrt(num) %/% 1
  a <- a0
  m <- 0
  d <- 1
  numer1 <- 0
  numer2 <- 1
  numer3 <- as.bigz(a)
  
  denom1 <- 0
  denom2 <- 0
  denom3 <- 1
  
  while(sub.bigz(pow.bigz(numer3,2), mul.bigz(num,pow.bigz(denom3,2))) != 1){
    m <- d*a - m
    d <- (num - m^2) / d
    a <- floor((a0 + m) / d)
    
    numer1 <- numer2
    numer2 <- numer3
    numer3 <- add.bigz(mul.bigz(numer2,a),numer1)
    
    denom1 <- denom2
    denom2 <- denom3
    denom3 <- add.bigz(mul.bigz(denom2,a),denom1)
  }
  if(numer3 > max_x){
    max_x <- numer3
    max_num <- num
  }
  
}
print(max_num)