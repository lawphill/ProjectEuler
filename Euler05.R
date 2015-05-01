# Find smallest positive number evenly divisible by all numbers from 1:20

divisors <- c(20:2)
increment <- 19*17*13*11*7*5*3*2 # Has to be a multiple of the product of primes
num <- 0
found <- 0

while(found==0){
  num <- num + increment
  if(sum(num%%divisors==0)==length(divisors)){
    found <- 1
  }
}
print(num)