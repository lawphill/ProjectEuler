# let p be the perimeter of a right triangle with sides {a,b,c}
# For any p, there are X solutions with a,b,c as integers, e.g. p=120 can be solved with:
#   {20,48,52}
#   {24,45,51}
#   {30,40,50}
#
# For p <= 1000, find the maximum number of solutions

max_solutions <- 0
max_p <- 0

for(p in 2:1000){
  solutions <- 0
  for(c in ceiling(p/2):floor(p/3)){
    a <- 2:(c-1)  
    a <- a[a>(p-c-a)] # Restrict a > b
    b <- p - c - a
    
    solutions <- solutions + sum(a^2 + b^2 == c^2)
  }
  if(solutions > max_solutions){
    max_solutions <- solutions
    max_p <- p
  }
}