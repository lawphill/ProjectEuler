# FIND THE SHORTEST PATH SUM THROUGH THE MATRIX GOING FROM THE TOP-LEFT TO BOTTOM-RIGHT
#
# SOLUTION: KEEP TRACK OF PATH SUM IN A MATRIX, INITIALIZE WITH SOME POSSIBLE PATH
#   ITERATE THROUGH EACH CELL OF MATRIX, CHOOSING MINIMUM PATH
#   REPEAT UNTIL CONVERGENCE (13 iterations)
# SOLVES IN ~4 SECONDS, RELATIVELY STUPID SOLUTION, WOULD NEED A BETTER ALGORITHM FOR LARGER DATA

filename <- "data/p083_matrix.txt"
data<-read.table(filename,sep=",")
totals <- matrix(data=0,ncol=ncol(data),nrow=nrow(data))
totals[1,1] <- data[1,1]

# Fill in edges, works here because it's square
for(j in 2:ncol(data)){
  totals[1,j] <- totals[1,j-1] + data[1,j]
}

# Initialize assuming we're always going down all the way and then right
for(i in 2:nrow(data)){
  for(j in 1:ncol(data)){
    totals[i,j] <- totals[i-1,j] + data[i,j]
  }
}

n_updated <- 1 # For initialization purposes
while(n_updated > 0){ # Update each cell one at a time, until we've stopped updating
  n_updated <- 0  
  for(i in 1:nrow(data)){
    for(j in 1:ncol(data)){
      new_min <- c()
      if(i > 1){
        new_min <- min(new_min, totals[i-1,j])
      }
      if(i < nrow(data)){
        new_min <- min(new_min,totals[i+1,j])
      }
      if(j > 1){
        new_min <- min(new_min,totals[i,j-1])
      }
      if(j < ncol(data)){
        new_min <- min(new_min,totals[i,j+1])
      }
      new_min <- new_min + data[i,j]
      
      if(new_min < totals[i,j]){
        totals[i,j] <- new_min
        n_updated <- n_updated + 1
      }
    }
  }  
}
print(totals[nrow(data),ncol(data)])