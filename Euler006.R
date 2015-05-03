# Calculate the difference between the sum of squares of 1-100 and the 
# square of the sum, i.e. (1^2+2^2...+100^2) - (1+2+...+100)^2

nums <- c(1:100)

sum_squares <- sum(nums^2)
square_sums <- sum(nums)^2

diff <- square_sums - sum_squares

print(diff)