# If a box contains twenty-one coloured discs, composed of fifteen blue discs and
#   six red discs, and two discs were taken at random, it can be seen that the
#   probability of taking two blue discs, P(BB) = (15/21)×(14/20) = 1/2.

# The next such arrangement, for which there is exactly 50% chance of taking two blue
#   discs at random, is a box containing eighty-five blue discs and thirty-five red
#   discs.

# By finding the first arrangement to contain over 10^12 = 1,000,000,000,000 discs in
#   total, determine the number of blue discs that the box would contain.

# (numBlue / total) * ((numBlue - 1) / (total - 1)) = 1/2
# 2*(numBlue * (numBlue - 1)) = (total * (total - 1))
# 2*(numBlue^2 - numBlue) = total^2 - total
# 2*numBlue^2 - 2*numblue - total^2 + total = 0
# NOTE: This is an exponential diophantine equation basically of the form
#   a*X^2 + b*X + c*Y^2 + d*Y = 0
# where a = 2, b = -2, c = -1, d = 1
min_total <- 10^12

next_pair <- function(pair){
  # Update equation taken from the solution given by:
  #   http://www.alpertron.com.ar/QUAD.HTM
  new_b <- 3*pair[1] + 2*pair[2] - 2
  new_n <- 4*pair[1] + 3*pair[2] - 3
  return(c(new_b,new_n))
}


init <- c(1,1)
while(init[2] <= min_total){
  init <- next_pair(init)
}
print(init[1])