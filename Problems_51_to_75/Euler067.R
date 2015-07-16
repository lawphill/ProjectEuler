filename <- 'data/p067_triangle.txt'
con=file(filename,open="r")
lines <- readLines(con)
nrows <- length(lines)

data <- vector("list",nrows)


for(i in 1:nrows){
  data[[i]] <- as.numeric(unlist(strsplit(lines[i]," ")))
}

path <- rep(0,nrows)
path[1] <- 1 # We know our starting point
sums <- rep(0,length(data[[nrows]]))
sums[1] <- data[[1]][path[1]] # Keep track of the max sum of each index
for(r in 2:nrows){
  # Calculate max sum that would lead to the current index
  new_sums <- rep(0,nrows)
  for(c in 1:r){
    if(c > 1){
      max_prev <- max(sums[c],sums[c-1])
    }else{
      max_prev <- sums[c]
    }
    new_sums[c] <- max_prev + data[[r]][c]
  }
  sums <- new_sums
}
print(max(sums))