# The 6K text file, sudoku.txt (right click and 'Save Link/Target As...'), contains
#   fifty different Su Doku puzzles ranging in difficulty, but all with unique
#   solutions (the first puzzle in the file is the example above).

# By solving all fifty puzzles find the sum of the 3-digit numbers found in the top
#   left corner of each solution grid; for example, 483 is the 3-digit number found
#   in the top left corner of the solution grid above.

callIterate <- function(m,func){
  return_val <- iterate(which(m==0,arr.ind=TRUE),m,func)
  if(return_val[[1]]){
    return(return_val[[2]])
  }else{
    return("Failed to solve sudoku")
  }
}

iterate <- function(indices,m,func){
  return_val <- list()
  if(length(indices)==0){
    return_val[[1]] <- func(m)
    return_val[[2]] <- m
    return(return_val)
  }else if(length(indices) == 2){
    remaining <- c()
  }else if(length(indices) == 4){
    remaining <- t(as.matrix(indices[2,]))
  }else{
    remaining <- indices[2:nrow(indices),]
  }
  
  poss_vals <- find_poss_values(indices[1,1],indices[1,2],m)
  # If we reach a dead end
  if(length(poss_vals) == 0){
    return_val[[1]] <- FALSE
    # LAP: No need to send return_val[[2]] if [[1]] is FALSE
    #return_val[[2]] <- m
    return(return_val)
  }
  
  for(i in poss_vals){
    current_m <- m # Make our guess
    current_m[indices[1,1],indices[1,2]] <- i
    # Attempt to solve using our guess
    return_val <- iterate(remaining,current_m,func)
    #print(return_val)
    if(return_val[[1]]){
      return(return_val)
    }else{
      next
    }
  }
  # Only reach this point if we've failed and need to go back a level
  return_val[[1]] <- FALSE
  #return_val[[2]] <- m
  return(return_val)
}

check_valid <- function(m){
  if(is.null(m)){
    return(FALSE)
  }
  n <- nrow(m)
  gridSize <- sqrt(n)
  if(gridSize != floor(gridSize)){
    return(FALSE)
  }
  if(ncol(m) != n){
    return(FALSE)
  }
  if(any(m > n)){
    return(FALSE)
  }
  
  for(i in 1:n){
    # CHECK ROWS
    if(length(m[i,m[i,]!=0]) != length(unique(m[i,m[i,]!=0]))){
      return(FALSE)
    }
    # CHECK COLUMNS
    if(length(m[m[,i]!=0,i]) != length(unique(m[m[,i]!=0,i]))){
      return(FALSE)
    }
  }
  # CHECK GRIDS
  for(min_x_coord in seq.int(1,n,gridSize)){
    for(min_y_coord in seq.int(1,n,gridSize)){
      max_x_coord <- min_x_coord + 2
      max_y_coord <- min_y_coord + 2
      vals <- c(m[min_x_coord:max_x_coord,min_y_coord:max_y_coord])
      if(length(vals[vals!=0]) != length(unique(vals[vals!=0]))){
        return(FALSE)
      }
    }
  }
  return(TRUE)
}

find_poss_values <- function(r,c,m){
  if(m[r,c] != 0){ return(c()) }
  
  row_vals <- m[r,m[r,]!=0]
  col_vals <- m[m[,c]!=0,c]
  
  min_row_coord <- floor(((r-1)/3) %% 3)*3+1
  min_col_coord <- floor(((c-1)/3) %% 3)*3+1
  max_row_coord <- min_row_coord + 2
  max_col_coord <- min_col_coord + 2                                                        
  grid_vals <- c(m[min_row_coord:max_row_coord,min_col_coord:max_col_coord])
  grid_vals <- grid_vals[grid_vals!=0]
  
  poss <- 1:9
  poss <- poss[! poss %in% c(row_vals,col_vals,grid_vals)]
  return(poss)
}


# READ IN DATA
filename <- 'data/p096_sudoku.txt'
con <- file(filename, open="r")
problem <- 0
sudoku <- vector("list")
while (length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 0) {
  if(length(grep("Grid",oneLine)) > 0){
    problem <- problem + 1
    line <- 0
    sudoku[[problem]] <- matrix(data=0,nrow=9,ncol=9)
  }else{
    line <- line + 1
    sudoku[[problem]][line,] <- as.numeric(unlist(strsplit(oneLine,"")))
  }
}
close(con)

# SOLVE PROBLEMS
total <- 0
for(problem in 1:length(sudoku)){
  print(problem)
  
  # SOLVE SIMPLE PORTIONS
  m <- sudoku[[problem]]
  indices <- which(m==0,arr.ind=TRUE)
  for(iter in 1:5){
    for(i in 1:nrow(indices)){
      poss <- find_poss_values(indices[i,1],indices[i,2],m)
      if(length(poss)==1){ # Fill in singleton values
        m[indices[i,1],indices[i,2]] <- poss
      }
    }
  }
  
  # GRAB FULL SOLUTION
  solution <- callIterate(m,check_valid)
  if(is.matrix(solution)){
    total <- total + sum(solution[1,1:3] * 10^(2:0))
  }else{
    print(c("Failed to solve problem:",problem))
  }  
}
print(total)