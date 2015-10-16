# The points P(x1, y1) and Q(x2, y2) are plotted at integer co-ordinates and are joined to the origin,
#   O(0,0), to form ΔOPQ.

# There are exactly fourteen triangles containing a right angle that can be formed when each co-ordinate
#   lies between 0 and 2 inclusive; that is,
#   0 ≤ x1, y1, x2, y2 ≤ 2.

# Given that 0 ≤ x1, y1, x2, y2 ≤ 50, how many right triangles can be formed?
gcd <- function(x,y) {
  r <- x%%y;
  return(ifelse(r, gcd(y, r), y))
}

smarter <- function(minCoord,maxCoord){
  total <- 0
  # If right angle is at origin, O
  # If right angle is at first coordinate P, with either x1==0 or y1==0
  total <- total + 3*(maxCoord-minCoord)^2
  
  # If right angle is at P with x1,y1 > 0
  for(x1 in (minCoord+1):maxCoord){
    for(y1 in (minCoord+1):maxCoord){
      # For downward solutions
      div <- gcd(x1,y1) # GCD to reduce fractions
      dx <- y1 / div
      dy <- x1 / div
      total <- total + floor(min(c((maxCoord-x1)/dx,y1/dy)))
      
      # For upward solutions
      total <- total + floor(min(c((maxCoord-y1)/dy,x1/dx)))
    }
  }  
  return(total)
}
print(smarter(0,50))