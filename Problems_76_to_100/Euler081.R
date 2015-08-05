filename <- "data/p081_matrix.txt"

data<-read.table(filename,sep=",")
totals <- matrix(data=0,ncol=ncol(data),nrow=nrow(data))
totals[1,1] <- data[1,1]

# Fill in edges, works here because it's square
for(i in 2:nrow(data)){
  totals[1,i] <- totals[1,i-1] + data[1,i]
  totals[i,1] <- totals[i-1,1] + data[i,1]
}

# Fill in other cells
for(i in 2:nrow(data)){
  for(j in 2:ncol(data)){
    totals[i,j] <- min(totals[i-1,j],totals[i,j-1]) + data[i,j]
  }
}
print(totals[nrow(data),ncol(data)])