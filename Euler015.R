# Lattice paths
# In a 2x2 grid, getting from top left to bottom right corners there are 6 paths
# Which only go either right or down
# How many paths in a 20x20 grid?
# Solved brute force using dynammic programming
size <- 20

# Apparently, this can be solved analytically as:
analytic_solution = factorial(2*size) / (factorial(size)^2)

path_lengths <- matrix(0,size,size)

count_paths <- function(n,m){
  if(n==0 || m==0){
    return(1)
  }
  if(path_lengths[n,m]!=0){
    return(path_lengths[n,m])
  }
  
  move_n <- count_paths((n-1),m)
  move_m <- count_paths(n,(m-1))
  path_lengths[n,m] <<- (move_n+move_m)
  return(move_n+move_m)
}
count_paths(size,size)