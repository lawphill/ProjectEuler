# It turns out that 12 cm is the smallest length of wire that can be bent to form an integer sided right
#   angle triangle in exactly one way, but there are many more examples.

# 12 cm: (3,4,5)
# 24 cm: (6,8,10)
# 30 cm: (5,12,13)
# 36 cm: (9,12,15)
# 40 cm: (8,15,17)
# 48 cm: (12,16,20)

# In contrast, some lengths of wire, like 20 cm, cannot be bent to form an integer sided right angle
#   triangle, and other lengths allow more than one solution to be found; for example, using 120 cm it
#   is possible to form exactly three different integer sided right angle triangles.
# 120 cm: (30,40,50), (20,48,52), (24,45,51)

# Given that L is the length of the wire, for how many values of L ≤ 1,500,000 can exactly one integer
#   sided right angle triangle be formed?

# a^2 + b^2 = c^2
max_L <- 48L
sides <- (1L:(max_L-2L))
squares <- (sides)^2
max_squares <- max(squares)
singles <- 0

perimeters <- c()
for(a in 1:(length(sides)-2)){
  poss_c <- squares[a] + squares[sides >= a]
  poss_c <- poss_c[poss_c <= max_squares]
  c2 <- poss_c[which(poss_c %in% squares)]
  if(!is.null(length(c2))){
    c <- sqrt(c2)
    b <- round(sqrt(squares[c]-squares[a]))
    
    if(sum(c(a,b,c)) <= max_L){
      print(a+b+c)
      perimeters <- c(perimeters,a+b+c)
    }
  }
}
p_vals <- unique(perimeters)
for(p in p_vals[p_vals <= max_L]){
  if(sum(perimeters == p) == 1){
    singles <- singles + 1
  }
}
print(singles)