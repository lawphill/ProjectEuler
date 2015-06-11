# Calculate the largest product of four adjacent numbers in the given matrix
# Adjacent can mean left-to-right, up-down, or diagonal in any direction

x=read.csv("data/Euler011_matrix.txt",header=FALSE,sep=" ")
M=as.matrix(x)

max_product <- 0

for(i in 1:nrow(M)){
  for(j in 1:ncol(M)){
    # Check left-right
    if(j < (ncol(M)-3)){
      max_product <- max(max_product, prod(M[i,j:(j+3)]))
      # Check diagonal up
      if(i > 3){
        max_product <- max(max_product, prod(diag(M[i:(i-3),j:(j+3)])))
      }
      # Check diagonal down
      if(i < (nrow(M)-3)){
        max_product <- max(max_product, prod(diag(M[i:(i+3),j:(j+3)])))
      }
    }
    # Check up-down
    if(i < (nrow(M)-3)){
      max_product <- max(max_product,prod(M[i:(i+3),j]))
    }
  }
}
print(max_product)