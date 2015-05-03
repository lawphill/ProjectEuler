# Calculate largest prime factor of 600851475143 
# Go through factors largest to smallest, check primeness
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