# For all numbers a^b in the range 2<=a<=100, 2<=b<=100 (a,b are integers)
# How many distinct values are there?

# Brute Force solution, calculate all using pow.bigz from "gmp", then remove
# duplicates

library("gmp")

min_a <- 2
max_a <- 100
min_b <- 2
max_b <- 100

values <- matrix(data=as.bigz(0),nrow=(max_a-min_a+1),ncol=(max_b-min_b+1))

for(a in min_a:max_a){
  values[a-1,] <- pow.bigz(a,(min_b:max_b))
}

print(length(unique(c(values)))) # To grab unique values, first convert to list