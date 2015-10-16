# A number chain is created by continuously adding the square of the digits in a
#   number to form a new number until it has been seen before.

# For example,
# 44 → 32 → 13 → 10 → 1 → 1
# 85 → 89 → 145 → 42 → 20 → 4 → 16 → 37 → 58 → 89

# Therefore any chain that arrives at 1 or 89 will become stuck in an endless loop.
#   What is most amazing is that EVERY starting number will eventually arrive at 1
#   or 89.

# How many starting numbers below ten million will arrive at 89?

next_chain <- function(n){
  digits <- (n %/% 10^(floor(log10(n):0))) %% 10
  return(sum(digits^2))
}

lazy_chain <- function(n){
  if(n > length(end_vals)){
    n <- next_chain(n)
  }
  
  if(end_vals[n] == 0){
    end_vals[n] <<- lazy_chain(next_chain(n))
  }
  return(end_vals[n])
}

maxN <- 9999999
end_vals <- rep(0,next_chain(maxN))
end_vals[1] <- 1
end_vals[89] <- 89
total <- 0
for(i in 1:maxN){
  if(lazy_chain(i) == 89){
    total <- total + 1
  }
}
print(total)