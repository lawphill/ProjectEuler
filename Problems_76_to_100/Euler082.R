filename <- "data/p082_matrix.txt"
data<-read.table(filename,sep=",")
totals <- matrix(data=0,ncol=ncol(data),nrow=nrow(data))
totals[,1] <- data[,1]

# Look at each column filling in as we go
for(j in 2:ncol(data)){
  totals[,j] <- totals[,j-1] + data[,j] # Assume we come from the left to begin with
  # Look at our options if we're coming from the top
  for(i in 2:nrow(data)){
    totals[i,j] <- min(totals[i,j],totals[i-1,j]+data[i,j])
  }
  # Now consider our options coming from the bottom
  for(i in (nrow(data)-1):1){
    totals[i,j] <- min(totals[i,j],totals[i+1,j]+data[i,j])
  }
}
print(min(totals[,ncol(data)]))
