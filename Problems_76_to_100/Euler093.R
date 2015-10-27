# By using each of the digits from the set, {1, 2, 3, 4}, exactly once, and making
#   use of the four arithmetic operations (+, −, *, /) and brackets/parentheses, it
#   is possible to form different positive integer targets.

# For example,
# 8 = (4 * (1 + 3)) / 2
# 14 = 4 * (3 + 1 / 2)
# 19 = 4 * (2 + 3) − 1
# 36 = 3 * 4 * (2 + 1)

# Note that concatenations of the digits, like 12 + 34, are not allowed.

# Using the set, {1, 2, 3, 4}, it is possible to obtain thirty-one different target
#   numbers of which 36 is the maximum, and each of the numbers 1 to 28 can be
#   obtained before encountering the first non-expressible number.

# Find the set of four distinct digits, a < b < c < d, for which the longest set of
#   consecutive positive integers, 1 to n, can be obtained, giving your answer as a
#   string: abcd.

library("permute")

# LAP: Current problem. Code assumes parentheses don't matter. Just order the digits
#   and do each operation in turn, looking at all permutations
#   But it doesn't do well with things like:
#     (7 - (9/2)) * 6 = 15
# Need to implement somehow to look at all possible parentheses permutations
#   This should only matter for division and subtraction

highestN <- function(digits,digitPerms,operPerms){
  # Take a set of digits and operators to act on them. Find all possible integer
  #   outcomes and return the largest N for which 1..N can all be created
  
  outcomes <- rep(0,nrow(digitPerms)*nrow(operPerms))
  curr <- 1
  for(i in 1:nrow(digitPerms)){
    d <- digits[digitPerms[i,]]
    for(j in 1:nrow(operPerms)){
      o <- operPerms[j,]
      
      n <- d[1]
      for(opIndex in 1:ncol(operPerms)){
        if(o[opIndex] == "/" || o[opIndex] == "-"){
          n <- c(mapply(o[opIndex],n,d[opIndex+1]),
                 mapply(o[opIndex],d[opIndex+1],n))
        }else{
          n <- mapply(o[opIndex],n,d[opIndex+1])
        }
      }
      for(val in n){
        outcomes[curr] <- val
        curr <- curr + 1
      }
    }
  }
  outcomes <- sort(unique(outcomes[outcomes > 0]))
  outcomes <- outcomes[(outcomes %% 1) == 0]
  #return(outcomes)
  x <- which(outcomes != 1:length(outcomes))
  return(x[1]-1)
}

digits <- 1:9
operators <- c("*","/","+","-")

nOpers <- length(operators)
operPerms <- matrix(data=NA,ncol=3,nrow=nOpers^3)
for(r in 1:nrow(operPerms)){
  divisor <- 1
  for(c in 1:ncol(operPerms)){
    operPerms[r,c] <- operators[(floor((r-1)/divisor) %% nOpers) + 1]
    divisor <- divisor * nOpers
  }
}

digitPerms <- matrix(data=NA,ncol=4,nrow=4*3*2)
digitPerms[1,] <- 1:4
digitPerms[2:nrow(digitPerms),] <- allPerms(c(1:4))

maxN <- 28 # Highest known n
maxDigits <- c(0,0,0,0)
for(a in digits){
  for(b in digits[digits > a]){
    for(c in digits[digits > b]){
      for(d in digits[digits > c]){
        n <- highestN(c(a,b,c,d),digitPerms,operPerms)
        if(n >= maxN){
          maxN <- n
          maxDigits <- c(a,b,c,d)
        }
      }
    }
  }
}
print(maxDigits)