# Starting from the middle, create a spiral (going right and counterclockwise)
# For example, a 5x5 spiral would be
#
# 21 22 23 24 25
# 20 7  8  9  10
# 19 6  1  2  11
# 18 5  4  3  12
# 17 16 15 14 13
#
# The diagonals of that matrix sum to 101, calculate the sum of the diagonals
# for a 1001x1001 matrix

# Brute force solution, generate matrix, add sums when they're on the diagonal

size <- 1001
#size <- 5
spiral <- matrix(data=0,ncol=size,nrow=size)

num <- 1
x <- ceiling(size/2)
y <- ceiling(size/2)
if((size %% 2) == 0){
  print("Error, size must be odd")
}
# Generate matrix
spiral[x,y] <- num
dir <- 4
sum_diag <- num
while(num <= size^2){
  num <- num + 1

  # Look ahead to see if we should turn
  new_dir <- (dir %% 4) + 1
  new_coords <- next_coord(x,y,new_dir)
  if(spiral[new_coords[1],new_coords[2]] == 0){
    dir <- new_dir
  }else{
    new_coords <- next_coord(x,y,dir)
  }
  x <- new_coords[1]
  y <- new_coords[2]
  
  spiral[x,y] <- num
  
  # Check if diagonal
  if(x==y || (size-x+1)==y){
    sum_diag <- sum_diag + num
  }
}

next_coord <- function(x,y,dir){
  # dir = 1, going right
  # dir = 2, going down
  # dir = 3, going left
  # dir = 4, going up
  
  if(dir == 1){
    y <- y + 1
  }else if(dir == 2){
    x <- x + 1
  }else if(dir == 3){
    y <- y - 1
  }else if(dir == 4){
    x <- x - 1
  }
  return(c(x,y))
}