# Move through the pyramid of numbers only moving to adjacent numbers
# Sum the numbers along the path and find the path with the highest sum
# Return this sum

# For this data, an array of arrays, "adjacent" means moving from index, i, to
# i or i+1 on the next row

# Solved similar to Viterbi logic, go through each row, calculate the best path
# To each of the possible states
# When you reach the end, you have the best path to reach all of the possible options

r1 <- c(75)
r2 <- c(95,64)
r3 <- c(17,47,82)
r4 <- c(18,35,87,10)
r5 <- c(20,04,82,47,65)
r6 <- c(19,01,23,75,03,34)
r7 <- c(88,02,77,73,07,63,67)
r8 <- c(99,65,04,28,06,16,70,92)
r9 <- c(41,41,26,56,83,40,80,70,33)
r10 <- c(41,48,72,33,47,32,37,16,94,29)
r11 <- c(53,71,44,65,25,43,91,52,97,51,14)
r12 <- c(70,11,33,28,77,73,17,78,39,68,17,57)
r13 <- c(91,71,52,38,17,14,91,43,58,50,27,29,48)
r14 <- c(63,66,04,68,89,53,67,30,73,16,69,87,40,31)
r15 <- c(04,62,98,27,23,09,70,98,73,93,38,53,60,04,23)

pyramid <- list(r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13,r14,r15)

nrows <- length(pyramid)

path <- rep(0,nrows)
path[1] <- 1 # We know our starting point
sums <- rep(0,length(pyramid[[nrows]]))
sums[1] <- pyramid[[1]][path[1]] # Keep track of the max sum of each index
for(r in 2:nrows){
  # Calculate max sum that would lead to the current index
  new_sums <- rep(0,nrows)
  for(c in 1:r){
    if(c > 1){
      max_prev <- max(sums[c],sums[c-1])
    }else{
      max_prev <- sums[c]
    }
    new_sums[c] <- max_prev + pyramid[[r]][c]
  }
  sums <- new_sums
}
print(max(sums))