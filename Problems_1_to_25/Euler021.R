# Find sum of all amicable numbers < 10000
# Amicable pairs are numbers where d(a)=b, d(b)=a
# d(n) = sum of all proper divisors

d <- function(n){
  poss <- 1:max(1,floor(n/2))
  return(sum(poss[(n %% poss) == 0]))
}

is_pair <- function(n){
  return( n == (d(d(n))) && n != d(n) )
}

max <- 10000
nums <- 1:(max-1)
total <- 0

for(i in nums){
  if(is_pair(i)){
    total <- total + i
  }
}
print(total)