setwd("/media/Shared_Data/Euler/Problems_76_to_100/")
filename <- 'data/p099_base_exp.txt'

data <- read.table(filename,sep=",")

data_exp <- function(i){
  # Log transform so that we're dealing with smaller numbers but order is preserved
  # log(a^x) = x*log(a)
  return(log10(data[i,1]) * data[i,2])
}


max <- data_exp(1)
max_line <- 1

for(i in 2:1000){
  n <- data_exp(i)
  if(n > max){
    max <- n
    max_line <- i
  }
}
print(max_line)