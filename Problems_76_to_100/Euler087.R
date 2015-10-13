# The smallest number expressible as the sum of a prime square, prime cube, and prime fourth power is 28.
#   In fact, there are exactly four numbers below fifty that can be expressed in such a way:
  
#  28 = 2^2 + 2^3 + 2^4
#  33 = 3^2 + 2^3 + 2^4
#  49 = 5^2 + 2^3 + 2^4
#  47 = 2^2 + 3^3 + 2^4

# How many numbers below fifty million can be expressed as the sum of a prime square, prime cube, and
#   prime fourth power?

# LAP: The largest possible integer raised to the fourth still less than 50,000,000 is 84.
#   Cubed is 368, squared is 7071
#   Calculate primes less than that and explore the possible combinations.

# IMPORT SIEVE FUNCTION
source("/media/Shared_Data/Euler/prime_sieve.R")

max_int <- 7071
primes <- sieve(max_int)

squares <- primes^2
cubes <- primes[primes<=368]^3
quads <- primes[primes<=84]^4

# Since there are so few primes (908 squares, 73 cubes, 23 quads), we can brute force
sum_matrix <- array(data=squares,dim=c(length(squares),length(cubes),length(quads)))
for(i in 1:length(cubes)){
  sum_matrix[,i,] <- sum_matrix[,i,] + cubes[i]
}
for(i in 1:length(quads)){
  sum_matrix[,,i] <- sum_matrix[,,i] + quads[i]
}

print(length(unique(c(sum_matrix[sum_matrix < 50000000]))))