# Calculate largest prime factor of 600851475143 

# Factorize by going through 2:floor(sqrt(x)), and removing that factor
# e.g. for 45, remove factor 2 (if possible), it's not so we move on to 3
# 45, remove factor 3 -> 15, remove factor 3 -> 5
# 5, remove factor 4, not possible
# 5, remove factor 5 -> 1, 5 is largest factor so return it

num <- 600851475143

factorize <- function(x){
  curr_max <- 1
  
  while(x %% 2 == 0){
    curr_max <- 2
    x <- x / 2
  }
  
  for(i in 3:max(3,floor(sqrt(x)))){
    while(x %% i == 0){
      curr_max <- i
      x <- x / i
    }
  }
  if(x > 2){
    curr_max <- max(curr_max,x)
  }
  return(curr_max)
}

print(factorize(num))