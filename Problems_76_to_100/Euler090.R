# We have two 6-sided die each with a different number 0-9 printed on each side.
#   If we place the die side-by-side, we can produce two-digit numbers
#       (the die can be placed in any order)
#   We'll let any side with 6 or 9 represent the other.
#   Ordering of the numbers on a dice does not matter
# How many distinct sets of numbers for the two dice allow for each of the square numbers to be displayed?
#   e.g. 01, 04, 09, 16, 25, 36, 49, 64, 81

# One die needs 0, the other need 1,4,6/9
# The die with 1 needs the other to have 6/9
# The die with 2 needs the other to have 5
# The die with 3 needs the other to have 6/9
# The die with 4 needs the other to have 6/9
# The die with 6/9 needs the other to have 4
# The die with 8 needs the other to have 1

has_digits <- function(squares,x,y){
  # digit_matrix is set of digits to find, x,y are sets
  x[x==9] <- 6 # Replace 9 with 6 b/c they're equivalent
  y[y==9] <- 6
  return(all(sapply(1:ncol(squares),function(i) (squares[1,i] %in% x && squares[2,i] %in% y) ||
                      (squares[1,i] %in% y && squares[2,i] %in% x))))
}

combs <- combn(0:9,6) # We leave the 9s in the combinations b/c 6 and 9 are distinct for this purpose
    # only
squares <- matrix(data=c(0,1,0,4,0,6,1,6,2,5,3,6,4,6,6,4,8,1),nrow=2,ncol=9) # Replaced 9s with 6s


total <- 0
for(i in 1:ncol(combs)){
  for(j in i:ncol(combs)){
    if(has_digits(squares,combs[,i],combs[,j])){
      total <- total + 1
    }
  }
}
print(total)