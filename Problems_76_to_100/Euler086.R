# CUBOID ROUTES
# A cuboid is a rectangular room, defined by L*W*H. The Cuboid Route is the shortest route from one
#   corner to the opposite, following the walls of the cuboid. The cuboid route need not be an integer
#   even when the room's dimensions are integers
#
# There are 2060 distinct cuboids, ignoring rotations, with integer dimensions, up to a maximum size of 
#   M by M by M, for which the shortest route has integer length when M = 100. This is the least value of
#   M for which the number of solutions first exceeds two thousand; the number of solutions when M = 99 is
#   1975.
#
# Find the least value of M such that the number of solutions first exceeds one million.

# LAP: The trick here is that if we let L be the longest side, then we just need to know whether W+H forms
#   an integer hypotenuse. If it does, then there are two possible situations.
#   If W+H (wh in the code) is less than or equal to L, then there are floor(wh/2) possible sets of W and
#   H.
#   If W+H is greater than L, then there are 1+(L-(W+H+1)/2) possible sets of W and H.
# We just increase L as we go until we find a larger count than our target


l <- 2L
target <- 1000000
total <- 0L
while(total < target){
  l <- l + 1L
  for(wh in 3L:(2L*l)){
    hypotenuse <- sqrt(wh^2L + l^2L)
    if((hypotenuse %% 1)==0){
      total <- total + ifelse(wh <= l, floor(wh/2),ceiling(1+(l-(wh+1)/2)))
    }
  }
}
print(c(l,total))