# This algorithm taken from wikipedia. 

to_check <- 2:10000
to_check <- to_check[(sqrt(to_check) %% 1) != 0]

odd <- 0
for(num in to_check){
  m <- 0
  d <- 1
  a0 <- floor(sqrt(num))
  a <- a0
  
  index <- 0
  while(a != 2*a0){
    index <- index + 1
    
    m <- d*a - m
    d <- (num - m^2) / d
    a <- floor((a0 + m) / d)
  }
  if((index %% 2) != 0){
    odd <- odd + 1
  }
}

print(odd)