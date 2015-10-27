# It is easily proved that no equilateral triangle exists with integral length sides
#   and integral area. However, the almost equilateral triangle 5-5-6 has an area of
#   12 square units.

# We shall define an almost equilateral triangle to be a triangle for which two
#   sides are equal and the third differs by no more than one unit.

# Find the sum of the perimeters of all almost equilateral triangles with integral
#   side lengths and area and whose perimeters do not exceed one billion
#   (1,000,000,000).

# LAP: From Wikipedia:
#   All isosceles Heronian triangles are given by rational multiples of:
#   a=2(u^2-v^2),
#   b=u^2+v^2,
#   c=u^2+v^2,
#   for coprime integers u and v with u>v.

# SOLVING FOR U
# a - b = 1
# 2*(u^2 - v^2) - (u^2 + v^2) = 1
# 2*u^2 - 2*v^2 - u^2 - v^2 = 1
# u^2 - 3*v^2 = 1
# u = sqrt(1 + 3*v^2)

# a - b = -1
# 2*(u^2 - v^2) - (u^2 + v^2) = -1
# 2*u^2 - 2*v^2 - u^2 - v^2 = -1
# u^2 - 3*v^2 = -1
# u = sqrt(-1 + 3*v^2)

# SOLVING FOR V
# a - b = 1
# u^2 - 3*v^2 = 1
# sqrt((u^2 - 1)/3) = v

# a - b = -1
# 2*(u^2-v^2) - (u^2+v^2) = -1
# 2*(u^2-v^2) = (u^2+v^2) - 1

# 2u^2 - 2v^2 - u^2 - v^2 = -1
# u^2 - 3*v^2 = -1
# 3v^2 - u^2 = 1
# 3v^2 = 1 + u^2
# v^2 = (1+u^2)/3
# v = sqrt((1+u^2)/3)

library('Matrix')
source("/media/Shared_Data/Euler/gcd.R")

tri_area_integer <- function(sides){
  p <- sum(sides)/2
  return((sqrt(p*(p-sides[1])*(p-sides[2])*(p-sides[3])) %% 1)==0)
}

tri_area <- function(sides){
  p <- sum(sides)/2
  sqrt(p*(p-sides[1])*(p-sides[2])*(p-sides[3]))
}


calcPerimeter <- function(u,v){
  return(4*u^2)
}
calcSides <- function(u,v){
  return(c(2*(u^2-v^2),u^2+v^2,u^2+v^2))
}

# Since perimeter is 4*u^2, maximum u for 1,000,000,000 is floor(sqrt(1bil/4))

max_perimeter <- 1000000000
max_u <- floor(sqrt(max_perimeter/4))
found_values <- Matrix(0,nrow=max_u,ncol=max_u,sparse=TRUE)

for(u in 2:max_u){
  poss_v <- c(sqrt((u^2+(1:(u-1)))/3),sqrt((u^2-(1:(u-1)))/3))
  #poss_v2 <- sqrt((u^2-(1:(u-1)))/3)
  v <- poss_v[which((poss_v%%1)==0)]
  #v2 <- poss_v2[which((poss_v2%%1)==0)]
  #for(val in c(v,v2)){
  for(val in v){
    s <- calcSides(u,val)
    side_gcd <- gcd(s[1],s[2])
    u_gcd <- gcd(u,val)
    if(abs(s[1]-s[2]) == side_gcd && found_values[u/u_gcd,val/u_gcd]==0){
      found_values[u/u_gcd,val/u_gcd] <- sum(s)/side_gcd
      #print(c(u/u_gcd,val/u_gcd,sum(s)/side_gcd))
    }
  }
}
print(sum(found_values))