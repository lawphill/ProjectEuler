# Return the index of the first Fibonacci number with 1000 digits
library("gmp")

max_num <- 10000 # Just to ensure we don't go too far
fibs <- c(as.bigz(1),as.bigz(1))

fibonacci <- function(){
  new_fib <- add.bigz(fibs[1],fibs[2])
  fibs[1] <<- fibs[2]
  fibs[2] <<- new_fib
  return(new_fib)
}

found <- 0
counter <- 2
ndigits <- 1000
while(found == 0){
  counter <- counter + 1
  if(counter >= max_num){
    found <- 1
  }
  if(ceiling(log(fibonacci(),base=10))>=ndigits){
    found <- 1
    print(counter)
  }
}